{-# Language OverloadedStrings #-}
module Language.ASKEE.Gromet where

import Data.Text(Text)
import qualified Data.Text as Text
import Language.ASKEE.Panic(panic)

import qualified Data.Aeson as JSON
import qualified Language.ASKEE.Syntax as Easel
import qualified Control.Monad.State as State
import Data.Map(Map)
import qualified Data.Map as Map

type Uid = Text
type Ident = Text

data Port =
  Port { portUid :: Uid
       , portBox :: Uid
       , portType :: Maybe Uid
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
    ArgWire Uid
  | ArgLiteral Text
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

data GrometConv =
  GrometConv { gcCurUid  :: Int
             , gcBoxes   :: Map Uid Box
             , gcWires   :: Map Uid DirectedWire
             , gcPorts   :: Map Uid Port
             , gcVars    :: Map Uid Variable
             , gcPortMap :: Map Text Uid
             , gcContainer :: Maybe Uid
             }

type GrometGen = State.State GrometConv


---------------------------------------------------------------------


newUid :: GrometGen Uid
newUid =
  do  uid <- State.gets gcCurUid
      State.modify (\gc -> gc { gcCurUid = 1 + gcCurUid gc })
      pure (Text.pack . show $ uid)

getContainer :: GrometGen Uid
getContainer =
  do  mbUid <- State.gets gcContainer
      case mbUid of
        Nothing -> panic "getContainer" ["expecting contianer uid to be set"]
        Just c -> pure c

-- TODO: check that this is a WireList Box
withContainer :: Uid -> GrometGen a -> GrometGen a
withContainer uid op =
  do  curContainer <- State.gets gcContainer
      State.modify (\gc -> gc { gcContainer = Just uid })
      a <- op
      State.modify (\gc -> gc { gcContainer = curContainer })
      pure a

addBox :: Box -> GrometGen ()
addBox box =
  State.modify (\gc -> gc { gcBoxes = Map.insert (boxUid box) box (gcBoxes gc) })

modifyBox :: (Box -> Box) -> Uid -> GrometGen ()
modifyBox modification uid =
  do  mbBox <- State.gets (Map.lookup uid . gcBoxes)
      case mbBox of
        Nothing -> panic "modifyBox" ["Expected box to exist"]
        Just b  ->
          if boxUid (modification b) /= boxUid b
            then panic "modifyBox" ["Cannot change box UID"]
            else addBox (modification b)

addBoxWire :: DirectedWire -> Box -> Box
addBoxWire wire box =
  case boxWiring box of
    WireList wl ->
      box { boxWiring = WireList $ wireUid wire:wl }
    Operator {} -> panic "addBoxWire" ["Operator box should not contain wires"]

setWireContainer :: Uid -> DirectedWire -> GrometGen ()
setWireContainer bid wire =
  modifyBox (addBoxWire wire) bid

addPort :: Port -> GrometGen ()
addPort p =
  State.modify
    (\gc -> gc { gcPortMap = Map.insert (portUid p) p (gcPortmap gc)})

mkPort :: Box -> Uid -> Maybe Text -> GrometGen Port
mkPort box typeUid name =
  do  uid <- newUid
      let port =
            Port { portUid = "P:" <> uid
                 , portType = typeUid
                 , portBox = boxUid box
                 , portName = name
                 }
      addPort port
      pure port

mkWire :: Port -> Port -> GrometGen DirectedWire
mkWire inPort outPort =
  do  uid <- newUid
      let wire =
            DirectedWire { wireUid = "W:" <> uid
                         , wireType = portType inPort
                         , wireOutput = portUid outPort
                         , wireInput = portUid inPort
                         }
      updateBox
      pure wire
  where
    updateBox wire =
      if portBox inPort == portBox outPort then
        setWireContainer (portBox inPort) wire
      else
        do  setOutputPort inPort
            setInputPort inPort
            container <- getContainer
            setWireContainer container wire

mkBox :: Text -> BoxWires -> GrometGen Box
mkBox name wiring =
  do  uid <- newUid
      let box = Box { boxUid = "B:" <> uid
                    , boxName = name
                    , boxWiring = wiring
                    , boxInputPorts = []
                    , boxOutputPorts = []
                    }

      addBox box
      pure box


mkBoxWires :: Text -> GrometGen Box
mkBoxWires name = mkBox name (WireList [])

mkBoxOp :: Text -> Text -> GrometGen Box
mkBoxOp name op = undefined





