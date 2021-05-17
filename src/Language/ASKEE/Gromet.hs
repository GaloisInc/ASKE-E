{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Language.ASKEE.Gromet where

import Data.Text(Text, pack, unpack)
import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import Data.Aeson((.=))
import qualified Control.Monad.State as State
import Control.Monad(void)
import Data.Foldable(traverse_)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Language.ASKEE.Panic(panic)
import qualified Language.ASKEE.Core as Core

type Uid = Text
-- TODO: do this with other UID types
newtype PortUid = PortUid Uid
  deriving (Show, Eq, Ord)
type Ident = Text

data Port =
  Port { portUid :: PortUid
       , portBox :: Uid
       , portType :: Uid
       , portName :: Maybe Text
       }
  deriving(Show, Eq)

data DirectedWire =
  DirectedWire { wireUid :: Uid
               , wireType :: Uid
               , wireInput :: PortUid
               , wireOutput :: PortUid
               }
  deriving(Show, Eq)

data Arg =
    ArgPort PortUid
  | ArgLiteral Text Uid
  deriving(Show, Eq)

data BoxWires =
    WireList [Uid]
  | Operator Text [Arg]
  deriving(Show, Eq)

data Box =
  Box { boxUid :: Uid
      , boxName :: Ident
      , boxWiring :: BoxWires
      , boxInputPorts :: [PortUid]
      , boxOutputPorts :: [PortUid]
      }
  deriving(Show, Eq)

data Variable =
  Variable { variableName :: Uid
           , variableType :: Uid
           , variableWires :: [Uid]
           }
  deriving(Show, Eq)

data Gromet =
  Gromet { grometPorts :: Map PortUid Port
         , grometWires :: Map Uid DirectedWire
         , grometBoxes :: Map Uid Box
         }


---------------------------------------------------------------------
-- JSON

instance JSON.ToJSON PortUid where
  toJSON (PortUid uid) = JSON.toJSON uid

instance JSON.ToJSON Port where
  toJSON p =
    JSON.object [ "uid" .= portUid p
                , "box" .= portBox p
                , "type" .= portType p
                , "name" .= portName p
                , "metadata" .= JSON.Null
                ]

instance JSON.ToJSON DirectedWire where
  toJSON w =
    JSON.object [ "uid" .= wireUid w
                , "type" .= wireType w
                , "value" .= JSON.Null
                , "metadata" .= JSON.Null
                , "input" .= wireInput w
                , "output" .= wireOutput w
                ]
instance JSON.ToJSON Arg where
  toJSON a =
    case a of
      ArgLiteral val ty ->
        JSON.object [ "uid" .= JSON.Null
                    , "type" .= ty
                    , "value" .= val
                    , "metadata" .= JSON.Null
                    ]
      ArgPort (PortUid p) -> JSON.String p

instance JSON.ToJSON Box where
  toJSON b = JSON.object (stdFields ++ wiringFields)
    where
      wiringFields =
        case boxWiring b of
          WireList w -> [ "wiring" .= w ]
          Operator o args ->
            [ "operator" .= o
            , "args" .= args
            ]

      stdFields =
        [ "uid" .= boxUid b
        , "name" .= boxName b
        , "input_ports" .= boxInputPorts b
        , "output_ports" .= boxOutputPorts b
        ]

instance JSON.ToJSON Gromet where
  toJSON g =
    JSON.object [ "ports" .= Map.elems (grometPorts g)
                , "wires" .= Map.elems (grometWires g)
                , "boxes" .= Map.elems (grometBoxes g)
                ]

---------------------------------------------------------------------
-- Conversion

data GrometConv =
  GrometConv { gcBoxes :: Map Uid Box
             , gcWires :: Map Uid DirectedWire
             , gcPorts :: Map PortUid Port

             , gcCurUid  :: Int
             , gcPortMap :: Map Text PortUid
             }

type GrometGen = State.State GrometConv

gcGromet :: GrometConv -> Gromet
gcGromet gc =
  Gromet { grometPorts = gcPorts gc
         , grometWires = gcWires gc
         , grometBoxes = gcBoxes gc
         }

newUid :: GrometGen Uid
newUid =
  do  uid <- State.gets gcCurUid
      State.modify (\gc -> gc { gcCurUid = 1 + gcCurUid gc })
      pure (Text.pack . show $ uid)

mkPort :: Uid -> Uid -> Maybe Text -> GrometGen PortUid
mkPort box typeUid name =
  do  uid <- PortUid . uidScheme <$> newUid
      let port =
            Port { portUid = uid
                 , portType = typeUid
                 , portBox = box
                 , portName = name
                 }
      addPort port
      pure uid
  where
    uidScheme uid =
      case name of
        Nothing -> "P:" <> uid
        Just n -> "P:" <> uid <> ":" <> n


mkBox :: Text -> BoxWires -> GrometGen Uid
mkBox name wiring =
  do  uid <- newUid
      let box = Box { boxUid = "B:" <> uid
                    , boxName = name
                    , boxWiring = wiring
                    , boxInputPorts = []
                    , boxOutputPorts = []
                    }

      addBox box
      pure (boxUid box)

mkBoxWires :: Text -> GrometGen Uid
mkBoxWires name = mkBox name (WireList [])

mkBoxOp :: Text -> Text -> GrometGen Uid
mkBoxOp name op = mkBox name (Operator op [])

mkBoxOp' :: Text -> GrometGen Uid
mkBoxOp' op = mkBoxOp op op

mkOutputPort :: Uid -> Uid -> Maybe Text -> GrometGen PortUid
mkOutputPort bUid typeUid name =
  do  p <- mkPort bUid typeUid name
      addOutputPort bUid p
      pure p

mkInputPort :: Uid -> Uid -> Maybe Text -> GrometGen PortUid
mkInputPort bUid typeUid name =
  do  p <- mkPort bUid typeUid name
      addInputPort bUid p
      pure p

modifyBox :: Uid -> (Box -> Box) -> GrometGen ()
modifyBox bUid f =
  do  mbBox <- State.gets (Map.lookup bUid . gcBoxes)
      box <- case mbBox of
                Nothing -> panic "modifyBox" ["tried to modify nonexistent box"]
                Just b -> pure b

      let box' = f box
      if boxUid box /= boxUid box' then
        panic "modifyBox" ["cannot change box uid"]
      else
        addBox box'

addOutputPort :: Uid -> PortUid -> GrometGen ()
addOutputPort bUid port =
  modifyBox bUid (\box -> box { boxOutputPorts = port:boxOutputPorts box })

addInputPort :: Uid -> PortUid -> GrometGen ()
addInputPort bUid port =
  do  modifyBox bUid (\box -> box { boxInputPorts = port:boxInputPorts box })
      addBoxArgIfOperator bUid (ArgPort port)

addBoxArgIfOperator :: Uid -> Arg -> GrometGen ()
addBoxArgIfOperator bUid arg =
  modifyBox bUid (\box -> box { boxWiring = addArg (boxWiring box)})
  where
    addArg wiring =
      case wiring of
        Operator o args -> Operator o (args ++ [arg]) -- TODO: kinda sloppy
        WireList _ -> wiring

mkWire :: PortUid -> PortUid -> GrometGen DirectedWire
mkWire inPort outPort =
  do  uid <- newUid
      ty <- portType <$> getPort inPort
      let wire =
            DirectedWire { wireUid = "W:" <> uid
                         , wireType = ty
                         , wireOutput = outPort
                         , wireInput = inPort
                         }
      pure wire

addBox :: Box -> GrometGen ()
addBox box =
  State.modify (\gc -> gc { gcBoxes = Map.insert (boxUid box) box (gcBoxes gc) })

addPort :: Port -> GrometGen ()
addPort p =
  State.modify
    (\gc -> gc { gcPorts = Map.insert (portUid p) p (gcPorts gc)})

addWire :: DirectedWire -> GrometGen ()
addWire w =
  State.modify
    (\gc -> gc { gcWires = Map.insert (wireUid w) w (gcWires gc)})

withBoxWire :: DirectedWire -> Box -> Box
withBoxWire wire box =
  case boxWiring box of
    WireList wl ->
      box { boxWiring = WireList $ wireUid wire:wl }
    Operator {} -> panic "withBoxWire" ["Operator box should not contain wires"]


getPort :: PortUid -> GrometGen Port
getPort (PortUid uid) =
  do  mdP <- State.gets (Map.lookup (PortUid uid) . gcPorts)
      case mdP of
        Nothing -> panic "getPort" ["port not found " ++ unpack uid]
        Just p -> pure p

varPort :: Text -> GrometGen PortUid
varPort t =
  do  mbPid <- State.gets (Map.lookup t . gcPortMap)
      case mbPid of
        Nothing -> panic "varPort" [unpack $ "no port for symbol '" <> t <> "'"]
        Just uid -> pure uid

portMapScope :: GrometGen a -> GrometGen a
portMapScope g =
  do  cur <- State.gets gcPortMap
      a <- g
      State.modify (\s -> s { gcPortMap = cur })
      pure a

assocVarPort :: Core.Ident -> PortUid -> GrometGen ()
assocVarPort name port =
  State.modify (\s -> s { gcPortMap = Map.insert name port (gcPortMap s) })

getPortBox :: PortUid -> GrometGen Uid
getPortBox pid =
  do  mbPort <- State.gets (fmap portBox . Map.lookup pid . gcPorts)
      case mbPort of
        Nothing -> panic "getPortBox" ["port does not exist"]
        Just uid -> pure uid

-------------------------------------------------------------------------------
-- Easel -> Gromet

typeBool :: Uid
typeBool = "T:Boolean"

typeFloat :: Uid
typeFloat = "T:Float"

typeUndef :: Uid
typeUndef = "T:Undefined"

describeOp1 :: Core.Op1 -> Text
describeOp1 op = pack (show op)

describeOp2 :: Core.Op2 -> Text
describeOp2 op = pack (show op)

asOutput :: Uid -> PortUid -> Maybe Text -> GrometGen PortUid
asOutput boxId pSrcId name =
  do  pSrc <- getPort pSrcId
      pDstId <- mkPort boxId (portType pSrc) name
      void $ mkWire pSrcId pDstId
      addOutputPort boxId pDstId
      pure pDstId

connectIn :: Uid -> PortUid -> Maybe Text -> GrometGen PortUid
connectIn boxId pSrcId label =
  do  pSrc <- getPort pSrcId
      pDstId <- mkPort boxId (portType pSrc) label
      void $ mkWire pSrcId pDstId
      addInputPort boxId pDstId
      pure pDstId

connectIn' :: Uid -> PortUid -> GrometGen ()
connectIn' boxId pSrcId = void $ connectIn boxId pSrcId Nothing

connectArg :: Uid -> Arg -> GrometGen ()
connectArg boxId arg =
  case arg of
    ArgPort p -> connectIn' boxId p
    ArgLiteral {} -> addBoxArgIfOperator boxId arg

mkOpFromInputs :: Text -> Uid -> [Arg] -> GrometGen PortUid
mkOpFromInputs opname tyId args =
  do  boxId <- mkBoxOp' opname
      connectArg boxId `traverse_` args
      mkOutputPort boxId tyId Nothing

expToGrometPort :: Core.Expr -> GrometGen PortUid
expToGrometPort e0 =
  do  arg <- expToGromet e0
      case arg of
        ArgPort p -> pure p
        ArgLiteral _ ty -> mkOpFromInputs "const" ty [arg]

expToGromet :: Core.Expr -> GrometGen Arg
expToGromet e0 =
  case e0 of
    Core.Literal (Core.Num n) -> pure $ ArgLiteral (pack $ show n) typeFloat
    Core.Literal (Core.Bool b) -> pure $ ArgLiteral (pack $ show b) typeBool
    Core.Var name ->
      do  pid <- varPort name
          pure $ ArgPort pid

    Core.Op1 o e ->
      ArgPort <$> op (describeOp1 o) (typeOf1 o) [e]

    Core.Op2 o e1 e2 ->
      ArgPort <$> op (describeOp2 o) (typeOf2 o) [e1, e2]

    Core.If tst thn els ->
      do  tst' <- expToGromet tst
          thn' <- expToGromet thn
          els' <- expToGromet els
          let args' = [tst', thn', els']

          boxId <- mkBoxOp' "if"
          connectArg boxId `traverse_` args'
          tyId <- typeOfArg thn'
          ArgPort <$> mkOutputPort boxId tyId Nothing

    Core.Fail msg -> pure $ ArgLiteral ("exception: " <> pack msg) typeUndef
  where
    op opname tyId args =
      (expToGromet `traverse` args) >>= mkOpFromInputs opname tyId

    typeOfArg a =
      case a of
        ArgLiteral _ t -> pure t
        ArgPort p -> portType <$> getPort p

    typeOf1 o =
      case o of
        Core.Not -> typeBool
        Core.Neg -> typeFloat
        Core.Exp -> typeFloat
        Core.Log -> typeFloat

    typeOf2 o =
      case o of
        Core.Add -> typeFloat
        Core.Sub -> typeFloat
        Core.Div -> typeFloat
        Core.Mul -> typeFloat
        Core.Lt -> typeBool
        Core.Leq -> typeBool
        Core.Eq -> typeBool
        Core.And -> typeBool
        Core.Or -> typeBool

withEnvBox :: Text -> [Core.Ident] -> (Uid -> GrometGen a) -> GrometGen a
withEnvBox label vars action =
  do  boxId <- mkBoxWires label
      ips <- concat <$> (wireInputPort boxId `traverse` List.nub vars)
      portMapScope (bind `traverse_` ips >> action boxId)
  where
    bind = uncurry assocVarPort
    wireInputPort boxId v =
      do  pid <- varPort v
          pid' <- connectIn boxId pid (Just v)
          pure [(v, pid')]

modelToGromet :: Core.Model -> GrometGen ()
modelToGromet model =
  withEnvBox (Core.modelName model) [] \boxId ->
    do  bindInput boxId `traverse_` Core.modelStateVars model
        bindLet `traverse_` Map.toList (Core.modelLets model)
        evtOutputs <- eventToGromet `traverse` Core.modelEvents model
        outputPorts <- Map.fromList <$> outputPort boxId `traverse` Core.modelStateVars model
        _ <- wireOutMap outputPorts `traverse` evtOutputs
        pure ()
  where
    wireOutMap portMap outMap =
      wireOut portMap `traverse` Map.toList outMap
    wireOut portMap (v, srcPId) =
      case Map.lookup v portMap of
        Nothing -> panic "modelToGromet" ["unexpected output in event"]
        Just destPId -> mkWire srcPId destPId

    bindLet (v, e) =
      do  expUid <- expToGrometPort e
          assocVarPort v expUid

    bindInput box v =
      do  p <- mkInputPort box typeFloat (Just v)
          assocVarPort v p

    outputPort boxId s =
      do  p <- mkOutputPort boxId typeFloat (Just s)
          pure (s, p)


eventToGromet :: Core.Event -> GrometGen (Map Ident PortUid)
eventToGromet evt =
  withEnvBox name inputs \boxId ->
    do  _ <- singleExpBox "rate" (Core.eventRate evt) "rate"
        _ <- singleExpBox "when" (Core.eventWhen evt) "when"
        outs <- eventEffectToGromet (Core.eventEffect evt)
        wireOutputPortMap boxId outs
  where
    name = "Event " <> Core.eventName evt
    inputs = allvars [evt]

singleExpBox :: Text -> Core.Expr -> Text -> GrometGen PortUid
singleExpBox name expr out =
  withEnvBox name (allvars [expr]) \boxId ->
    do  pid <- expToGrometPort expr
        asOutput boxId pid (Just out)

eventEffectToGromet :: Map Ident Core.Expr -> GrometGen (Map Core.Ident PortUid)
eventEffectToGromet varMap =
  withEnvBox "effect" vars \boxId ->
    do  outMap <- expToGrometPort `traverse` varMap
        wireOutputPortMap boxId outMap
  where
    vars = allvars ( snd <$> Map.toList varMap)

allvars :: Core.TraverseExprs t => [t] -> [Core.Ident]
allvars ts =
  Set.toList $ Set.unions (Core.collectVars <$> ts)

wireOutputPortMap :: Uid -> Map Core.Ident PortUid -> GrometGen (Map Core.Ident PortUid)
wireOutputPortMap boxId pmap =
    Map.unions <$> (out `traverse` Map.toList pmap)
  where
    out (n,uid) =
      do  p <- asOutput boxId uid (Just n)
          pure $ Map.singleton n p

convertCoreToGromet :: Core.Model -> Gromet
convertCoreToGromet mdl =
  State.evalState (modelToGromet mdl >> State.gets gcGromet) initial
  where
    initial =
      GrometConv { gcBoxes = Map.empty
                 , gcPorts = Map.empty
                 , gcWires = Map.empty
                 , gcCurUid = 1
                 , gcPortMap = Map.empty
                 }


