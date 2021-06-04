{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Gromet.Conversion where

import Language.ASKEE.Gromet.Syntax
import Data.Map (Map)
import Data.Text (Text)
import qualified Control.Monad.State as State
import qualified Data.Text as Text
import qualified Data.Map as Map
import Language.ASKEE.Panic (panic)
import Control.Monad ( void )
import Data.Foldable (traverse_)

data GrometConv =
  GrometConv { gcBoxes :: Map Uid Box
             , gcWires :: Map Uid DirectedWire
             , gcPorts :: Map PortUid Port

             , gcCurUid  :: Int
             , gcPortMap :: Map Text PortUid
             }

type GrometGen = State.State GrometConv

runGrometGen :: GrometGen a -> GrometConv -> a
runGrometGen = State.evalState

modifyGrometGen :: (GrometConv -> GrometConv) -> GrometGen ()
modifyGrometGen = State.modify

getsGrometGen :: (GrometConv -> a) -> GrometGen a
getsGrometGen = State.gets

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
        Nothing -> panic "getPort" ["port not found " ++ Text.unpack uid]
        Just p -> pure p

varPort :: Text -> GrometGen PortUid
varPort t =
  do  mbPid <- State.gets (Map.lookup t . gcPortMap)
      case mbPid of
        Nothing -> panic "varPort" [Text.unpack $ "no port for symbol '" <> t <> "'"]
        Just uid -> pure uid

portMapScope :: GrometGen a -> GrometGen a
portMapScope g =
  do  cur <- State.gets gcPortMap
      a <- g
      State.modify (\s -> s { gcPortMap = cur })
      pure a



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

