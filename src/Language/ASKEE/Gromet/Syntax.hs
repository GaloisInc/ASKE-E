{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Language.ASKEE.Gromet.Syntax
  ( module Language.ASKEE.Gromet.Syntax
  , BoxUid(..)
  , WireUid(..)
  , PortUid(..)
  , JunctionUid(..)
  , ValueType(..)
  , Literal(..)
  ) where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Aeson as JSON
import Data.Aeson((.=))

import Language.ASKEE.Gromet.Common


data Gromet =
  Gromet { grometName  :: Text
         , grometRoot  :: BoxUid
         , grometPorts :: [Port]
         , grometJunctions :: [Junction]
         , grometWires :: [Wire]
         , grometBoxes :: [Box]
         } deriving (Show,Eq)

type Meta = Map Text [Text]

data Junction =
  Junction { jUID       :: JunctionUid
           , jName      :: Text
           , jValueType :: ValueType
           , jMeta      :: Meta
           } deriving (Show,Eq)

data Port =
  Port { portUid        :: PortUid
       , portBox        :: BoxUid
       , portType       :: PortType
       , portValueType  :: ValueType
       , portName       :: Text
       , portMeta       :: Meta
       }
  deriving(Show, Eq)

data PortType =
    ParameterPort     -- ^ Used for a model parameter
  | InputPort         -- ^ Generic input port
  | OutputPort        -- ^ Generic output port
    deriving (Show,Eq)

data Wire = Wire
  { wireUid       :: WireUid
  , wireType      :: WireType
  , wireValueType :: ValueType
  , wireSource    :: WirePort
  , wireTarget    :: WirePort
  }
  deriving(Show, Eq)

data WireType =
    Directed
  | Undirected
  deriving (Show,Eq)

data WirePort = JPort JunctionUid | PPort PortUid
  deriving (Show,Eq)


data Arg =
    ArgPort PortUid
  | ArgLiteral Literal
  | ArgCall Text [Arg]        -- operator, arguments
  deriving(Show, Eq)

data Box = Box
  { boxUid    :: BoxUid
  , boxName   :: Text
  , boxSyntax :: BoxSyntax
  , boxPorts  :: [PortUid]  -- ports
  , boxBoxes  :: [BoxUid]   -- nested boxed
  , boxWires  :: [WireUid]  -- connections betweed ports and nested boxes
  , boxJuncitons :: [JunctionUid]
  , boxMeta   :: Meta
  } deriving(Show, Eq)

data BoxSyntax =
    BoxExpression Arg     -- value of `tree` field
  | BoxRelation BoxRelType   -- value of `type` field
    deriving (Show,Eq)

data BoxRelType =
    PrTNet
  | EnableRel
  | EventRel
  | RateRel
  | EffectRel
    deriving (Show,Eq)



---------------------------------------------------------------------
-- JSON

doMeta :: Map Text [Text] -> JSON.Value
doMeta mp = JSON.object [ k .= doVal vs | (k,vs) <- Map.toList mp ]
  where
  doVal vs = case vs of
               [v] -> JSON.toJSON v
               _   -> JSON.toJSON vs

instance JSON.ToJSON Gromet where
  toJSON g =
    JSON.object [ "syntax"    .= jsText "Gromet"
                , "type"      .= jsText "PrTNet"
                , "name"      .= grometName g
                , "metadata"  .= JSON.Null
                , "uid"       .= jsText (grometName g) -- XXX: ?
                , "root"      .= grometRoot g
                , "ports"     .= grometPorts g
                , "wires"     .= grometWires g
                , "junctions" .= grometJunctions g
                , "boxes"     .= grometBoxes g
                ]

instance JSON.ToJSON Junction where
  toJSON j =
    JSON.object [ "syntax"     .= jsText "Junction"
                , "uid"        .= jUID j
                , "type"       .= jsText "State"   -- are there any others?
                , "value_type" .= jValueType j
                , "metadata"   .= doMeta (jMeta j)
                ]

instance JSON.ToJSON Port where
  toJSON p =
    JSON.object [ "syntax"     .= jsText "Port"
                , "type"       .= portType p
                , "name"       .= portName p
                , "metadata"   .= doMeta (portMeta p)
                , "value_type" .= portValueType p
                , "uid"        .= portUid p
                , "box"        .= portBox p
                ]

instance JSON.ToJSON Wire where
  toJSON w =
    JSON.object [ "syntax"      .= jsText "Wire"
                , "uid"         .= wireUid w
                , "type"        .= wireType w
                , "value_type"  .= wireValueType w
                , "src"         .= wireSource w
                , "tgt"         .= wireTarget w
                ]

instance JSON.ToJSON Box where
  toJSON b = JSON.object (stdFields ++ varFields)
    where
    stdFields =
      [ "uid"       .= boxUid b
      , "name"      .= boxName b
      , "wires"     .= boxWires b
      , "boxes"     .= boxBoxes b
      , "ports"     .= boxPorts b
      , "junctions" .= boxJuncitons b
      , "metadata"  .= doMeta (boxMeta b)
      ]

    varFields =
      case boxSyntax b of
        BoxExpression arg -> [ "syntax" .= jsText "Expression"
                             , "tree"   .= arg ]
        BoxRelation t     -> [ "syntax" .= jsText "Relation"
                             , "type"   .= t ]



instance JSON.ToJSON BoxRelType where
  toJSON ty =
    case ty of
      EnableRel -> "Enable"
      EventRel  -> "Event"
      RateRel   -> "Rate"
      EffectRel -> "Effect"
      PrTNet    -> "PrTNet"   -- for some reason this has no T: at the front?

instance JSON.ToJSON WirePort where
  toJSON p =
    case p of
      JPort x -> JSON.toJSON x
      PPort x -> JSON.toJSON x

instance JSON.ToJSON PortType where
  toJSON pt =
    case pt of
      ParameterPort  -> "Parameter"
      InputPort      -> "Input"
      OutputPort     -> "Output"

instance JSON.ToJSON WireType where
  toJSON wt =
    case wt of
      Directed   -> "Directed"
      Undirected -> "Undirected"

instance JSON.ToJSON Arg where
  toJSON a =
    case a of
      ArgLiteral l -> JSON.toJSON l
      ArgPort (PortUid p) -> JSON.String p
      ArgCall op args ->
        JSON.object [ "syntax" .= jsText "Expr"
                    , "call" .= JSON.object
                                  [ "syntax" .= jsText "RefOp"
                                  , "name"   .= op
                                  ]
                    , "args" .= args
                    ]



