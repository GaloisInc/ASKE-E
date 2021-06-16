{-# Language OverloadedStrings, BlockArguments #-}
module Language.ASKEE.Gromet.Common where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import Data.Aeson ((.=))



type Uid = Text

newtype PortUid = PortUid Uid
  deriving (Show, Eq, Ord)

newtype BoxUid = BoxUid Uid
  deriving (Show, Eq, Ord)

newtype WireUid = WireUid Uid
  deriving (Show, Eq, Ord)

newtype JunctionUid = JunctionUid Uid
  deriving (Show, Eq, Ord)

data ValueType =
    Bool
  | Real
  | Integer
    deriving (Show,Eq)

data Literal =
    LitReal    Double
  | LitInteger Integer
  | LitBool    Bool
    deriving (Eq,Show)

--------------------------------------------------------------------------------
-- JSON instances

-- | Helper to resolve overloaded string
jsText :: Text -> JSON.Value
jsText = JSON.toJSON

jsShow :: Show a => a -> JSON.Value
jsShow x = jsText (Text.pack (show x))



instance JSON.ToJSON PortUid where
  toJSON (PortUid uid) = JSON.toJSON uid

instance JSON.FromJSON PortUid where
  parseJSON = JSON.withText "PORT_ID" (pure . PortUid)


instance JSON.ToJSON WireUid where
  toJSON (WireUid uid) = JSON.toJSON uid

instance JSON.FromJSON WireUid where
  parseJSON = JSON.withText "WIRE_ID" (pure . WireUid)


instance JSON.ToJSON BoxUid where
  toJSON (BoxUid uid) = JSON.toJSON uid

instance JSON.FromJSON BoxUid where
  parseJSON = JSON.withText "BOX_ID" (pure . BoxUid)


instance JSON.ToJSON JunctionUid where
  toJSON (JunctionUid uid) = JSON.toJSON uid

instance JSON.FromJSON JunctionUid where
  parseJSON = JSON.withText "JUNCTION_ID" (pure . JunctionUid)



instance JSON.ToJSON ValueType where
  toJSON ty =
    case ty of
      Real    -> "Real"
      Bool    -> "Boolean"
      Integer -> "Integer"

-- We are being permissive here
instance JSON.FromJSON ValueType where
  parseJSON = JSON.withText "VALUE_TYPE" \txt ->
    case txt of
      "Real"      -> pure Real
      "Float"     -> pure Real
      "T:Real"    -> pure Real
      "T:Float"   -> pure Real
      "Boolean"   -> pure Bool
      "T:Boolean" -> pure Bool
      "Integer"   -> pure Integer
      "T:Integer" -> pure Integer
      _           -> fail ("Unknown VALUE_TYPE: " <> Text.unpack txt)



instance JSON.ToJSON Literal where
  toJSON l =
    case l of
      LitReal dbl   -> lit Real    (jsShow dbl)
      LitInteger i  -> lit Integer (jsShow i)
      LitBool b     -> lit Bool    (jsShow b)

    where
    lit ty val = JSON.object [ "type"     .= ty
                             , "value"    .= val
                             , "metadata" .= JSON.Null
                             ]

