{-# Language OverloadedStrings #-}
module Language.ASKEE.Gromet where

import Data.Text(Text, pack, unpack)
import qualified Data.Text as Text
import Language.ASKEE.Panic(panic)

import qualified Data.Aeson as JSON
import qualified Language.ASKEE.Syntax as Easel
import qualified Control.Monad.State as State
import Data.Foldable(traverse_)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Language.ASKEE.Core as Core

type Uid = Text
type Ident = Text

data Port =
  Port { portUid :: Uid
       , portBox :: Uid
       , portType :: Uid
       , portName :: Maybe Text
       }
  deriving(Show, Eq)

data DirectedWire =
  DirectedWire { wireUid :: Uid
               , wireType :: Uid
               , wireInput :: Uid
               , wireOutput :: Uid
               }
  deriving(Show, Eq)

data Arg =
    ArgPort Uid
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
      , boxInputPorts :: [Uid]
      , boxOutputPorts :: [Uid]
      }
  deriving(Show, Eq)

data Variable =
  Variable { variableName :: Uid
           , variableType :: Uid
           , variableWires :: [Uid]
           }
  deriving(Show, Eq)

data Gromet =
  Gromet { grometBoxes :: Map Uid Box
         , grometWires :: Map Uid DirectedWire
         , grometPorts :: Map Uid Port
         , grometVars  :: Map Uid Variable
         }

data GrometConv =
  GrometConv { gcBoxes :: Map Uid Box
             , gcWires :: Map Uid DirectedWire
             , gcPorts :: Map Uid Port
             , gcVars  :: Map Uid Variable

             , gcCurUid  :: Int
             , gcPortMap :: Map Text Uid
             }

type GrometGen = State.State GrometConv


---------------------------------------------------------------------


newUid :: GrometGen Uid
newUid =
  do  uid <- State.gets gcCurUid
      State.modify (\gc -> gc { gcCurUid = 1 + gcCurUid gc })
      pure (Text.pack . show $ uid)

mkPort :: Uid -> Uid -> Maybe Text -> GrometGen Uid
mkPort box typeUid name =
  do  uid <- newUid
      let port =
            Port { portUid = "P:" <> uid
                 , portType = typeUid
                 , portBox = box
                 , portName = name
                 }
      addPort port
      pure uid

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

mkOutputPort :: Uid -> Uid -> Maybe Text -> GrometGen Uid
mkOutputPort bUid typeUid name =
  do  p <- mkPort bUid typeUid name
      addOutputPort bUid p
      pure p

mkInputPort :: Uid -> Uid -> Maybe Text -> GrometGen Uid
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

addOutputPort :: Uid -> Uid -> GrometGen ()
addOutputPort bUid port =
  modifyBox bUid (\box -> box { boxOutputPorts = port:boxOutputPorts box })

addInputPort :: Uid -> Uid -> GrometGen ()
addInputPort bUid port =
  modifyBox bUid (\box -> box { boxOutputPorts = port:boxInputPorts box })

addBoxArgs :: Uid -> [Arg] -> GrometGen ()
addBoxArgs bUid args = addBoxArg bUid `traverse_` args

addBoxArg :: Uid -> Arg -> GrometGen ()
addBoxArg bUid arg =
  do  modifyBox bUid (\box -> box { boxWiring = addArg (boxWiring box)})
      case arg of
        ArgPort p -> addInputPort bUid p
        ArgLiteral _ _-> pure ()
  where
    addArg wiring =
      case wiring of
        Operator op args -> Operator op (args ++ [arg]) -- TODO: kinda sloppy
        WireList _ -> panic "addBoxArg" ["tried to add wiring to the wrong kind of box"]

mkWire :: Uid -> Uid -> GrometGen DirectedWire
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


getPort :: Uid -> GrometGen Port
getPort uid =
  do  mdP <- State.gets (Map.lookup uid . gcPorts)
      case mdP of
        Nothing -> panic "getPort" ["port not found"]
        Just p -> pure p

varPort :: Text -> GrometGen Uid
varPort t =
  do  mbPid <- State.gets (Map.lookup t . gcPortMap)
      case mbPid of
        Nothing -> panic "varPort" [unpack $ "no port for symbol '" <> t <> "'"]
        Just uid -> pure uid

-------------------------------------------------------------------------------

typeBool :: Uid
typeBool = "T:Boolean"

typeFloat :: Uid
typeFloat = "T:Float"


describeOp1 :: Core.Op1 -> Text
describeOp1 op = pack (show op)

describeOp2 :: Core.Op2 -> Text
describeOp2 op = pack (show op)

-- eventToGromet :: Core.Event -> GrometGen Uid
-- eventToGromet evt =
--   do  evt' <- mkBoxWires ("Event " <> Core.eventName evt)


expToGromet :: Core.Expr -> GrometGen (Arg, [DirectedWire])
expToGromet e0 =
  case e0 of
    Core.NumLit n -> pure (ArgLiteral (pack $ show n) typeFloat, [])
    Core.BoolLit b -> pure (ArgLiteral (pack $ show b) typeBool, [])
    Core.Var name ->
      do  pid <- varPort name
          pure (ArgPort pid, [])

    Core.Op1 op e ->
      do  (e', eWires) <- expToGromet e
          box <- mkBoxOp' (describeOp1 op)
          argWires <- args box [e']
          out <- mkOutputPort box (typeOf1 op) Nothing
          pure (ArgPort out, eWires ++ argWires)

    Core.Op2 op e1 e2 ->
      do  (e1', e1Wires) <- expToGromet e1
          (e2', e2Wires) <- expToGromet e2
          box <- mkBoxOp' (describeOp2 op)
          argWires <- args box [e1', e2']
          out <- mkOutputPort box (typeOf2 op) Nothing
          pure (ArgPort out, e1Wires ++ e2Wires ++ argWires)

    Core.If tst thn els ->
      do  (tst', tstWires) <- expToGromet tst
          (thn', thnWires) <- expToGromet thn
          (els', elsWires) <- expToGromet els
          box <- mkBoxOp' "if"
          argWires <- args box [tst', thn', els']
          ty <- typeOfArg thn'
          out <- mkOutputPort box ty Nothing
          pure (ArgPort out, tstWires ++ thnWires ++ elsWires ++ argWires)

    Core.Fail msg -> pure (ArgLiteral (pack msg) "T:exception", [])

    _ -> undefined
  where
    args bUid as =
      do  pts <- mkInput bUid `traverse` as
          pure $ concat (snd <$> pts)

    mkInput bUid arg =
      case arg of
        a@ArgLiteral {} -> pure (a, [])
        ArgPort fromPort ->
          do  ty <- typeOfArg arg
              toPort <- mkInputPort bUid ty Nothing
              wire <- mkWire fromPort toPort
              let arg' = ArgPort toPort
              addBoxArg bUid arg'
              pure (arg', [wire])

    typeOfArg a =
      case a of
        ArgLiteral t _ -> pure t
        ArgPort p -> portType <$> getPort p
    typeOf1 op =
      case op of
        Core.Not -> typeBool
        Core.Neg -> typeFloat
        Core.Exp -> typeFloat
        Core.Log -> typeFloat

    typeOf2 op =
      case op of
        Core.Add -> typeFloat
        Core.Sub -> typeFloat
        Core.Div -> typeFloat
        Core.Mul -> typeFloat
        Core.Lt -> typeBool
        Core.Leq -> typeBool
        Core.Eq -> typeBool
        Core.And -> typeBool
        Core.Or -> typeBool










