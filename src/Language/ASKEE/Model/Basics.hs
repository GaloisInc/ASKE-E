{-# Language OverloadedStrings, BlockArguments #-}
module Language.ASKEE.Model.Basics where

import Data.Text(Text)
import qualified Data.Text as Text
import Text.Read(readMaybe)
import Control.Monad(msum)

import qualified Data.Aeson as JSON
import Data.Aeson(ToJSON(..),FromJSON(..))
import Data.Scientific(floatingOrInteger)

data ValueType =
    Integer
  | Real
  | Boolean
    deriving (Show,Read,Eq,Ord)

parseValueType :: Text -> Maybe ValueType
parseValueType txt =
  case Text.toLower txt of
    "integer" -> pure Integer
    "real"    -> pure Real
    "float"   -> pure Real
    "bool"    -> pure Boolean
    "boolean" -> pure Boolean
    _         -> Nothing

-- Note that these match Gromet
describeValueType :: ValueType -> Text
describeValueType vt =
  case vt of
    Integer -> "Integer"
    Real    -> "Real"
    Boolean -> "Boolean"



instance ToJSON ValueType where
  toJSON = toJSON . describeValueType

instance FromJSON ValueType where
  parseJSON = JSON.withText "VALUE_TYPE" \txt ->
    case parseValueType txt of
      Just v -> pure v
      Nothing -> fail "Invalid VALUE_TYPE"


--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
data Value =
    VInteger Integer
  | VReal    Double
  | VBool    Bool
    deriving (Show,Eq,Ord)

parseVBool :: Text -> Maybe Value
parseVBool x =
  case Text.toLower x of
    "true"  -> Just (VBool True)
    "false" -> Just (VBool False)
    _       -> Nothing

parseVInteger :: Text -> Maybe Value
parseVInteger = fmap VInteger . readMaybe . Text.unpack

parseVReal :: Text -> Maybe Value
parseVReal = fmap VReal . readMaybe . Text.unpack

parseValue :: Text -> Maybe Value
parseValue txt = msum [ parseVBool txt, parseVInteger txt, parseVReal txt ]

describeValue :: Value -> Text
describeValue v = Text.pack
  case v of
    VInteger x -> show x
    VReal x    -> show x
    VBool x    -> show x

instance ToJSON Value where
  toJSON x = case x of
               VInteger v -> toJSON v
               VReal v    -> toJSON v
               VBool v    -> toJSON v

instance FromJSON Value where
  parseJSON v =
    case v of
      JSON.Bool b -> pure (VBool b)
      JSON.Number n ->
        case floatingOrInteger n of
          Left d -> pure (VReal d)
          Right i -> pure (VInteger i)
      _ -> fail "Invalid VALUE"




--------------------------------------------------------------------------------
data ModelType =
    EaselType
  | DeqType
  | RNetType
  | CoreType
  | GrometPrtType
  | GrometPncType
  | GrometFnetType
  | SBMLType
  deriving(Eq, Ord, Show, Enum, Bounded)

allModelTypes :: [ModelType]
allModelTypes = [ minBound .. maxBound ]

parseModelType :: Text -> Maybe ModelType
parseModelType t =
  case t of
    "easel"       -> Just EaselType
    "diff-eq"     -> Just DeqType
    "rnet"        -> Just RNetType
    "core"        -> Just CoreType
    "gromet-prt"  -> Just GrometPrtType
    "gromet-pnc"  -> Just GrometPncType
    "gromet-fnet" -> Just GrometFnetType
    _ -> Nothing

parseModelType' :: String -> Maybe ModelType
parseModelType' = parseModelType . Text.pack

describeModelType :: ModelType -> Text
describeModelType t =
  case t of
    EaselType -> "easel"
    DeqType -> "diff-eq"
    RNetType -> "rnet"
    CoreType -> "core"
    GrometPrtType -> "gromet-prt"
    GrometPncType -> "gromet-pnc"
    GrometFnetType -> "gromet-fnet"
    SBMLType -> "sbml"

describeModelType' :: ModelType -> String
describeModelType' = Text.unpack . describeModelType

--------------------------------------------------------------------------------


