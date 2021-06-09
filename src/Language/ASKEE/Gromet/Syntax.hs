{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Language.ASKEE.Gromet.Syntax where

import Data.Text(Text)
import qualified Data.Aeson as JSON
import Data.Aeson((.=))

type Uid = Text

newtype PortUid = PortUid Uid
  deriving (Show, Eq, Ord)

newtype BoxUid = BoxUid Uid
  deriving (Show, Eq, Ord)

newtype WireUid = WireUid Uid
  deriving (Show, Eq, Ord)

newtype JunctionId = JunctionId Uid
  deriving (Show, Eq, Ord)

data Gromet =
  Gromet { grometName  :: Text
         , grometRoot  :: BoxUid
         , grometPorts :: [Port]
         , grometJunctions :: [Junction]
         , grometWires :: [Wire]
         , grometBoxes :: [Box]
         } deriving (Show,Eq)

data Junction =
  Junction { jUID       :: JunctionId
           , jName      :: Text
           , jValueType :: ValueType
           } deriving (Show,Eq)

data Port =
  Port { portUid        :: PortUid
       , portBox        :: BoxUid
       , portType       :: PortType
       , portValueType  :: ValueType
       , portName       :: Text
       }
  deriving(Show, Eq)

data PortType =
    ParameterPort     -- ^ Used for a model parameter
  | InputPort         -- ^ Generic input port
  | OutputPort        -- ^ Generic output port
    deriving (Show,Eq)

data ValueType =
    Bool
  | Real
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

data WirePort = JPort JunctionId | PPort PortUid
  deriving (Show,Eq)


data Arg =
    ArgPort PortUid
  | ArgLiteral Text ValueType -- value, type
  | ArgCall Text [Arg]        -- operator, arguments
  deriving(Show, Eq)

data Box = Box
  { boxUid    :: BoxUid
  , boxName   :: Text
  , boxSyntax :: BoxSyntax
  , boxPorts  :: [PortUid]  -- ports
  , boxBoxes  :: [BoxUid]   -- nested boxed
  , boxWires  :: [WireUid]  -- connections betweed ports and nested boxes
  , boxJuncitons :: [JunctionId]
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

-- | Helper to resolve overloaded string
jsText :: Text -> JSON.Value
jsText = JSON.toJSON

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
                , "type"       .= jsText "T:State"   -- are there any others?
                , "value_type" .= jValueType j
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
      ]

    varFields =
      case boxSyntax b of
        BoxExpression arg -> [ "syntax" .= jsText "Expression"
                             , "tree"   .= arg ]
        BoxRelation t     -> [ "syntax" .= jsText "Relation"
                             , "type"   .= t ]



instance JSON.ToJSON ValueType where
  toJSON ty =
    case ty of
      Real -> "T:Real"
      Bool -> "T:Boolean"

instance JSON.ToJSON BoxRelType where
  toJSON ty =
    case ty of
      EnableRel -> "T:Enable"
      EventRel  -> "T:Event"
      RateRel   -> "T:Rate"
      EffectRel -> "T:Effect"
      PrTNet    -> "PrTNet"   -- for some reason this has no T: at the front?

instance JSON.ToJSON PortUid where
  toJSON (PortUid uid) = JSON.toJSON ("P:" <> uid)

instance JSON.ToJSON WireUid where
  toJSON (WireUid uid) = JSON.toJSON ("W:" <> uid)

instance JSON.ToJSON BoxUid where
  toJSON (BoxUid uid) = JSON.toJSON ("B:" <> uid)

instance JSON.ToJSON JunctionId where
  toJSON (JunctionId uid) = JSON.toJSON ("J:" <> uid)

instance JSON.ToJSON WirePort where
  toJSON p =
    case p of
      JPort x -> JSON.toJSON x
      PPort x -> JSON.toJSON x

instance JSON.ToJSON PortType where
  toJSON pt =
    case pt of
      ParameterPort  -> "T:Parameter"
      InputPort      -> "T:Input"
      OutputPort     -> "T:Output"

instance JSON.ToJSON WireType where
  toJSON wt =
    case wt of
      Directed   -> "T:Directed"
      Undirected -> "T:Undirected"

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



