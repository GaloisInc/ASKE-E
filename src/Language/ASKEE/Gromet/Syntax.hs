{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.Gromet.Syntax where

import Data.Map  ( Map )
import Data.Text ( Text )
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import Data.Aeson ((.=))

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