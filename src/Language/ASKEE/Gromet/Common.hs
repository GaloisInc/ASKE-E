{-# Language OverloadedStrings, BlockArguments #-}
module Language.ASKEE.Gromet.Common where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import Text.Read(readMaybe)
import Data.Aeson ((.=),(.:))
import Data.Scientific(toRealFloat,floatingOrInteger)



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
    Boolean
  | Real
  | Integer
    deriving (Show, Eq)

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
      Boolean -> "Boolean"
      Integer -> "Integer"

-- We are being permissive here
instance JSON.FromJSON ValueType where
  parseJSON = JSON.withText "VALUE_TYPE" \txt ->
    case txt of
      "Real"      -> pure Real
      "Float"     -> pure Real
      "T:Real"    -> pure Real
      "T:Float"   -> pure Real
      "Boolean"   -> pure Boolean
      "T:Boolean" -> pure Boolean
      "Integer"   -> pure Integer
      "T:Integer" -> pure Integer
      _           -> fail ("Unknown VALUE_TYPE: " <> Text.unpack txt)


instance JSON.ToJSON Literal where
  toJSON l =
    case l of
      LitReal dbl   -> lit Real    (jsShow dbl)
      LitInteger i  -> lit Integer (jsShow i)
      LitBool b     -> lit Boolean (jsShow b)

    where
    lit ty val = JSON.object [ "type"     .= ty
                             , "syntax"   .= jsText "Literal"
                             , "value"    .=
                                 JSON.object [ "syntax" .= jsText "Val"
                                             , "val"    .= val
                                             ]
                             , "metadata" .= JSON.Null
                             ]


instance JSON.FromJSON Literal where
  parseJSON = JSON.withObject "LITERAL" \o ->
    do ty     <- o      .: "type"
       vaWrap <- o      .: "value"
       va     <- vaWrap .: "val"
       case ty of

         Real
           | JSON.Number x <- va -> pure (LitReal (toRealFloat x))
           | JSON.String txt <- va ->
             case readMaybe (Text.unpack txt) of
               Just n -> pure (LitReal n)
               Nothing -> fail ("Invalid REAL_LITERAL: " <> Text.unpack txt)

         Integer
           | JSON.Number x <- va ->
             case floatingOrInteger x of
               Right i -> pure (LitInteger i)
               Left r ->
                    fail ("Invalid INTEGER_LITERAL: " ++ show (r :: Double))
           | JSON.String txt <- va ->
             case readMaybe (Text.unpack txt) of
               Just n -> pure (LitInteger n)
               Nothing -> fail ("Invalid INTEGER_LITERAL: " <> Text.unpack txt)

         Boolean
           | JSON.Bool b <- va -> pure (LitBool b)
           | JSON.String txt <- va ->
             if      Text.toLower txt == "true" then pure (LitBool True)
             else if Text.toLower txt == "false" then pure (LitBool False)
             else fail ("Invalid BOOL_LITERAL: " <> Text.unpack txt)

         _ -> fail "Invalid LITERAL"





