{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.ModelType where
import Data.Text(Text, pack, unpack)

data ModelType = 
    EaselType 
  | DeqType 
  | CoreType
  | GrometPrtType
  deriving(Eq, Ord, Show, Enum)

parseModelType :: Text -> Maybe ModelType
parseModelType t =
  case t of
    "easel"   -> Just EaselType
    "diff-eq" -> Just DeqType
    "core"    -> Just CoreType
    _         -> Nothing

parseModelType' :: String -> Maybe ModelType
parseModelType' = parseModelType . pack

describeModelType :: ModelType -> Text
describeModelType t =
  case t of
    EaselType -> "easel"
    DeqType -> "diff-eq"
    CoreType -> "core"
    GrometPrtType -> "gromet-prt"

describeModelType' :: ModelType -> String
describeModelType' = unpack . describeModelType
