{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Language.ASKEE.Gromet where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import Data.Aeson((.=))
import qualified Control.Monad.State as State
import Control.Monad(forM,foldM)
import Data.Maybe(maybeToList)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.ASKEE.Panic(panic)
import qualified Language.ASKEE.Core as Core

import Debug.Trace

type Uid = Text

newtype PortUid = PortUid Uid
  deriving (Show, Eq, Ord)

newtype BoxUid = BoxUid Uid
  deriving (Show, Eq, Ord)

newtype WireUid = WireUid Uid
  deriving (Show, Eq, Ord)

newtype TypeUid = TypeUid Uid
  deriving (Show, Eq, Ord)


data Gromet =
  Gromet { grometName  :: Text
         , grometRoot  :: BoxUid
         , grometPorts :: [Port]
         , grometWires :: [Wire]
         , grometBoxes :: [Box]
         }

data Port =
  Port { portUid        :: PortUid
       , portBox        :: BoxUid
       , portType       :: PortType
       , portValueType  :: TypeUid        -- type of value
       , portName       :: Maybe Text
       }
  deriving(Show, Eq)

data PortType =
    ParameterPort     -- Used for a model parameter
  | StatePort         -- Used for a state variable parameter
  | InputPort         -- Generic input port
  | OutputPort        -- Generic output port
    deriving (Show,Eq)


data Wire = Wire
  { wireUid       :: WireUid
  , wireType      :: WireType
  , wireValueType :: TypeUid
  , wireSource    :: PortUid
  , wireTarget    :: PortUid
  }
  deriving(Show, Eq)

data WireType =
    Directed
  | Undirected
  deriving (Show,Eq)

data Arg =
    ArgPort PortUid
  | ArgLiteral Text TypeUid   -- value, type
  | ArgCall Text [Arg]        -- operator, arguments
  deriving(Show, Eq)

data Box = Box
  { boxUid    :: BoxUid
  , boxName   :: Text
  , boxSyntax :: BoxSyntax
  , boxPorts  :: [PortUid]  -- ports
  , boxBoxes  :: [BoxUid]   -- nested boxed
  , boxWires  :: [WireUid]  -- connections betweed ports and nested boxes
  } deriving(Show, Eq)

data BoxSyntax =
    BoxExpression Arg     -- value of `tree` field
  | BoxRelation TypeUid   -- value of `type` field
    deriving (Show,Eq)


--------------------------------------------------------------------------------
-- Predefined Types

typeBool :: TypeUid
typeBool = TypeUid "Boolean"

typeReal :: TypeUid
typeReal = TypeUid "Real"

effectType :: TypeUid
effectType = TypeUid "Effect"

enableType :: TypeUid
enableType = TypeUid "Enable"

eventType :: TypeUid
eventType = TypeUid "Event"

rateType :: TypeUid
rateType = TypeUid "Rate"

petriNetType :: TypeUid
petriNetType = TypeUid "PrTNet"



---------------------------------------------------------------------
-- JSON

jsText :: Text -> JSON.Value
jsText = JSON.toJSON

instance JSON.ToJSON TypeUid where
  toJSON (TypeUid uid) = JSON.toJSON ("T:" <> uid)

instance JSON.ToJSON PortUid where
  toJSON (PortUid uid) = JSON.toJSON ("P:" <> uid)

instance JSON.ToJSON WireUid where
  toJSON (WireUid uid) = JSON.toJSON ("W:" <> uid)

instance JSON.ToJSON BoxUid where
  toJSON (BoxUid uid) = JSON.toJSON ("B:" <> uid)




instance JSON.ToJSON Gromet where
  toJSON g =
    JSON.object [ "syntax"    .= jsText "Gromet"
                , "type"      .= jsText "PrTNet"
                , "name"      .= grometName g
                , "metadata"  .= JSON.Null
                , "uid"       .= jsText "XXX"
                , "root"      .= grometRoot g
                , "ports"     .= grometPorts g
                , "wires"     .= grometWires g
                , "boxes"     .= grometBoxes g
                ]

instance JSON.ToJSON Port where
  toJSON p =
    JSON.object [ "syntax"     .= jsText "Port"
                , "type"       .= portType p
                , "name"       .= portName p
                , "metadata"   .= JSON.Null
                , "value_type" .= portValueType p
                , "uid"        .= portUid p
                , "box"        .= portBox p
                ]


instance JSON.ToJSON PortType where
  toJSON pt =
    case pt of
      ParameterPort  -> "T:Parameter"
      StatePort      -> "T:State"
      InputPort      -> "T:Input"
      OutputPort     -> "T:Output"


instance JSON.ToJSON Wire where
  toJSON w =
    JSON.object [ "syntax"      .= jsText "Wire"
                , "uid"         .= wireUid w
                , "type"        .= wireType w
                , "value_type"  .= wireValueType w
                , "metadata"    .= JSON.Null
                , "src"         .= wireSource w
                , "tgt"         .= wireTarget w
                ]

instance JSON.ToJSON WireType where
  toJSON wt =
    case wt of
      Directed   -> "T:Directed"
      Undirected -> "T:Undirected"



instance JSON.ToJSON Box where
  toJSON b = JSON.object (stdFields ++ varFields)
    where
    varFields =
      case boxSyntax b of
        BoxExpression arg -> [ "syntax" .= jsText "Expression"
                             , "tree"   .= arg ]
        BoxRelation t     -> [ "syntax" .= jsText "Relation"
                             , "type"   .= t ]

    stdFields =
      [ "uid"     .= boxUid b
      , "name"    .= boxName b
      , "wires"   .= boxWires b
      , "boxes"   .= boxBoxes b
      , "ports"   .= boxPorts b
      ]

instance JSON.ToJSON Arg where
  toJSON a =
    case a of
      ArgLiteral val ty ->
        JSON.object [ "type" .= ty
                    , "value" .= val
                    , "metadata" .= JSON.Null
                    ]
      ArgPort (PortUid p) -> JSON.String p
      ArgCall op args ->
        JSON.object [ "syntax" .= jsText "Expr"
                    , "call" .= JSON.object
                                  [ "syntax" .= jsText "RefOp"
                                  , "name"   .= op
                                  ]
                    , "args" .= args
                    ]



--------------------------------------------------------------------------------
-- Conversion to Gromet

convertCoreToGromet :: Core.Model -> Gromet
convertCoreToGromet m =
  Gromet { grometName  = Core.modelName m
         , grometRoot  = rootUID
         , grometPorts = gcPorts finS
         , grometWires = gcWires finS
         , grometBoxes = gcBoxes finS
         }
  where
  (rootUID,finS) = State.runState (convertModel m) initState

  initState = GrometConv { gcBoxes      = []
                         , gcWires      = []
                         , gcPorts      = []
                         , gcCurUid     = 0
                         , gcBoxStack   = []
                         }


-- XXX: Assuming the lets are ordered in dependency order
convertModel :: Core.Model -> GrometGen BoxUid
convertModel model =
  do uid <- BoxUid <$> newUid
     paramPorts <- forM (Core.modelParams model) \paramName ->
        do puid <- PortUid <$> newUid
           let p = Port { portUid       = puid
                        , portBox       = uid
                        , portType      = ParameterPort
                        , portValueType = typeReal
                        , portName      = Just paramName
                        }
           addPort p
           pure (paramName, p)

     statePorts <- forM (Map.keys (Core.modelInitState model)) \x ->
        do puid <- PortUid <$> newUid
           let p = Port { portUid       = puid
                        , portBox       = uid
                        , portType      = StatePort
                        , portValueType = typeReal
                        , portName      = Just x
                        }
           addPort p
           pure (x, p)


     let ports = Map.fromList (paramPorts ++ statePorts)
     traceM (show ports)


     startNestedBox uid (Core.modelName model) petriNetType
     allPorts <- foldM doLet ports (Map.toList (Core.modelLets model))
     mapM_ (convertEvent allPorts) (Core.modelEvents model)
     endNestedBox (map portUid (Map.elems ports))

     pure uid

  where
  doLet ports (x,e) =
    do traceM ("EXPR: " ++ show e)
       uid    <- BoxUid <$> newUid
       lports <- fmap portUid <$>
                 connectInputPorts ports uid (Core.collectVars e)
       traceM ("VARS: " ++ show (Core.collectVars e))
       traceM ("LPORTS: " ++ show lports)
       out    <- PortUid <$> newUid
       let outP = Port { portUid = out
                       , portBox = uid
                       , portType = OutputPort
                       , portValueType = typeReal -- XXX: infer from expr
                       , portName = Just x
                       }
       addPort outP
       addBox Box { boxUid    = uid
                  , boxName   = x
                  , boxSyntax = BoxExpression (exprToArg lports e)
                  , boxPorts  = out : Map.elems lports
                  , boxBoxes  = []
                  , boxWires  = []
                  }

       pure (Map.insert x outP ports)



convertEvent :: Map Core.Ident Port -> Core.Event -> GrometGen ()
convertEvent inPorts ev =
  do evBoxUid <- BoxUid <$> newUid
     evPorts  <- connectInputPorts inPorts evBoxUid allVars
     startNestedBox evBoxUid (Core.eventName ev) eventType
     makeWhen evPorts
     makeRate evPorts
     makeEffect evPorts
     endNestedBox (map portUid (Map.elems evPorts))

  where
  allVars    = Set.unions [ enableVars, rateVars, effVars ]
  enableVars = Core.collectVars (Core.eventWhen ev)
  rateVars   = Core.collectVars (Core.eventRate ev)
  effVars    = Set.unions (Core.collectVars <$> Map.elems (Core.eventEffect ev))

  makeWhen evPorts =
    do uid   <- BoxUid <$> newUid
       let name = "Enabling Condition"
       ports <- connectInputPorts evPorts uid enableVars
       startNestedBox uid name enableType
       mapM_ (convertExpr name ports Nothing) (splitAnd (Core.eventWhen ev))
       endNestedBox (map portUid (Map.elems ports))

  makeRate evPorts =
    do uid   <- BoxUid <$> newUid
       let name = "Event Rate"
       ports <- connectInputPorts evPorts uid rateVars
       startNestedBox uid name rateType
       convertExpr name ports Nothing (Core.eventRate ev)
       endNestedBox (map portUid (Map.elems ports))

  makeEffect evPorts =
    do uid <- BoxUid <$> newUid
       ports <- connectInputPorts evPorts uid effVars
       let name = "Event Effect"
       startNestedBox uid name effectType
       outPs <- forM (Map.toList (Core.eventEffect ev)) \(x,e) ->
                do out <- PortUid <$> newUid
                   let p = Port { portUid        = out
                                , portBox        = uid
                                , portType       = StatePort
                                , portValueType  = typeReal
                                , portName       = Just x
                                }
                   addPort p
                   convertExpr name ports (Just p) e
                   pure out
       endNestedBox (outPs ++ map portUid (Map.elems ports))



-- | Make ports on a nested box and connect them to the corresponding ports
-- in the current box.
connectInputPorts ::
  Map Core.Ident Port {- ^ Ports in outer box -} ->
  BoxUid              {- ^ Uid of the new box -} ->
  Set Core.Ident      {- ^ Variables that need ports -} ->
  GrometGen (Map Core.Ident Port)
connectInputPorts inPorts uid vars =
  Map.fromList <$>
  forM (Set.toList vars) \x ->
     case Map.lookup x inPorts of
       Just p -> do p1 <- freshInputPort' uid p
                    pure (x,p1)
       Nothing -> panic "connectInputPorts" $ [ "Unknown variable"
                                            , show x
                                            , "in scope"
                                            ] ++ map show (Map.toList inPorts)




convertExpr ::
  Text                {- ^ Name for the box -} ->
  Map Core.Ident Port {- ^ Ports in outer box -} ->
  Maybe Port          {- ^ Output port in outer box, if any -} ->
  Core.Expr           {- ^ Definition -} ->
  GrometGen ()
convertExpr name inPorts mbOutPort expr =
  do uid    <- BoxUid <$> newUid
     varMap <- fmap portUid <$>
               connectInputPorts inPorts uid (Core.collectVars expr)
     ourOut <- traverse (freshOutputPort uid) mbOutPort

     addBox Box { boxUid    = uid
                , boxName   = name
                , boxSyntax = BoxExpression (exprToArg varMap expr)
                , boxPorts  = maybeToList ourOut ++ Map.elems varMap
                , boxBoxes  = []
                , boxWires  = []
                }


exprToArg :: Map Core.Ident PortUid -> Core.Expr -> Arg
exprToArg vars expr =
  case expr of

    Core.Literal l ->
      case l of
        Core.Num x  -> ArgLiteral (showText x) typeReal
        Core.Bool x -> ArgLiteral (showText x) typeBool

    Core.Var x ->
      case Map.lookup x vars of
        Just p       -> ArgPort p
        Nothing      -> error ("[bug] Variable " ++ show x ++ " not in scope.")

    Core.Op1 f e ->
      let op x = doOp x [e]
      in case f of
           Core.Not     -> op "not"
           Core.Neg     -> op "-"
           Core.Exp     -> op "exp"
           Core.Log     -> op "log"

    -- XXX: we could turn binary to N-ary operators?
    Core.Op2 f e1 e2 ->
      let op x = doOp x [e1,e2]
      in case f of
           Core.Add     -> op "+"
           Core.Mul     -> op "*"
           Core.Sub     -> op "-"
           Core.Div     -> op "/"
           Core.Lt      -> op "<"
           Core.Leq     -> op "<="
           Core.Eq      -> op "="
           Core.And     -> op "and"
           Core.Or      -> op "or"

    Core.If e1 e2 e3  -> doOp "if" [e1,e2,e3]
    Core.Fail _       -> doOp "error" []

  where
  doOp op es = ArgCall op (exprToArg vars <$> es)


splitAnd :: Core.Expr -> [Core.Expr]
splitAnd expr =
  case expr of
    Core.Op2 Core.And e1 e2 -> splitAnd e1 ++ splitAnd e2
    _                       -> [expr]







--------------------------------------------------------------------------------
-- Monad for conversion to Gromet

data GrometConv =
  GrometConv { gcBoxes   :: [Box]   -- ^ Finished boxes
             , gcWires   :: [Wire]  -- ^ Finished wires
             , gcPorts   :: [Port]  -- ^ Finished ports
             , gcCurUid  :: Int     -- ^ Uid generation

             , gcBoxStack :: [Box]
             }

type GrometGen = State.State GrometConv



-- | Utility to show a value as "Text"
showText :: Show a => a -> Text
showText = Text.pack . show

-- | Generate a fresh name
newUid :: GrometGen Uid
newUid =
  do s <- State.get
     let n = gcCurUid s
     State.put s { gcCurUid = n + 1 }
     pure (showText n)

-- | Add a port to the list of declaraitons.
addPort :: Port -> GrometGen ()
addPort p = State.modify \s -> s { gcPorts = p : gcPorts s }

-- | Add a wire to the list of declarations.
-- Also record it in the current "under construction" box
addWire :: Wire -> GrometGen ()
addWire w = State.modify \s ->
  s { gcWires = w : gcWires s
    , gcBoxStack = case gcBoxStack s of
                     [] -> []
                     b : bs -> b { boxWires = wireUid w : boxWires b } : bs
    }

-- | Add a box to the box on tope of the stack
-- Also record it in the current "under construction" box
addBox :: Box -> GrometGen ()
addBox b = State.modify \s ->
  s { gcBoxes = b : gcBoxes s
    , gcBoxStack = case gcBoxStack s of
                     [] -> []
                     bo : bs -> bo { boxBoxes = boxUid b : boxBoxes bo } : bs
    }

-- | Start a new "under construction" box
pushBox :: Box -> GrometGen ()
pushBox b = State.modify \s ->
  s { gcBoxStack = b : gcBoxStack s }

-- | Finish the construction of a box
popBox :: GrometGen Box
popBox =
  do s <- State.get
     case gcBoxStack s of
       b : bs -> do State.put s { gcBoxStack = bs }
                    pure b
       [] -> panic "popBox" ["No box to pop"]


startNestedBox :: BoxUid -> Text -> TypeUid -> GrometGen ()
startNestedBox uid name ty =
  pushBox Box { boxUid = uid
              , boxName = name
              , boxSyntax = BoxRelation ty
              , boxPorts = []
              , boxBoxes = []
              , boxWires = []
              }

endNestedBox :: [PortUid] -> GrometGen ()
endNestedBox ps =
  do box <- popBox
     addBox box { boxPorts = ps }


-- | Generate a fresh input port linking to the given input
-- port, and also a wire connecting the two.
freshInputPort' :: BoxUid -> Port -> GrometGen Port
freshInputPort' boxid p =
  do puid <- PortUid <$> newUid
     let newPort = p { portUid  = puid
                     , portBox  = boxid
                     , portType = case portType p of
                                    OutputPort    -> InputPort
                                    _             -> portType p
                     }
     addPort newPort
     wuid <- WireUid <$> newUid
     addWire Wire { wireUid       = wuid
                  , wireType      = Directed
                  , wireValueType = portValueType p
                  , wireSource    = portUid p
                  , wireTarget    = puid
                  }
     pure newPort

-- | Same as "freshInputPort'" but returns only the port UID
freshInputPort :: BoxUid -> Port -> GrometGen PortUid
freshInputPort boxid p = portUid <$> freshInputPort' boxid p

-- |Same as "freshInputPort" but for output ports (i.e., in the other direction)
freshOutputPort :: BoxUid -> Port -> GrometGen PortUid
freshOutputPort boxid p =
  do puid <- PortUid <$> newUid
     addPort p { portUid  = puid, portBox  = boxid }
     wuid <- WireUid <$> newUid
     addWire Wire { wireUid       = wuid
                  , wireType      = Directed
                  , wireValueType = portValueType p
                  , wireSource    = puid
                  , wireTarget    = portUid p
                  }
     pure puid


