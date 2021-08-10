{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.DataSet
( DataSet(..)
, DataSetColumn(..)
, dataSetFromDataSeries
) where

import qualified Data.Aeson as JSON
import Data.Aeson((.=))
import qualified Data.Aeson.Types as JSONT
import qualified Data.HashMap.Lazy as HM
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map

import qualified Language.ASKEE.DataSeries as Series
import Language.ASKEE.DataSeries(DataSeries)

-- DataSet is a data format that includes some metadata
data DataSet = DataSet
  { dataSetName :: Text
  , dataSetDescription :: Maybe Text
  , dataSetColumns :: [DataSetColumn]
  }
  deriving(Show, Eq, Ord)

data DataSetColumn = DataSetColumn
  { dataSetColumnName :: Text
  , dataSetColumnDescription :: Maybe Text
  , dataSetColumnValues :: [Double]
  }
  deriving(Show, Eq, Ord)

instance JSON.FromJSON DataSet where
  parseJSON v = JSON.withObject "dataset" parseDS v
    where
      parseDS obj =
        DataSet <$> reqField obj "name"
                <*> optField obj "description"
                <*> reqField obj "columns"

instance JSON.ToJSON DataSet where
  toJSON ds =
    JSON.object
      [ "name" .= dataSetName ds
      , "description" .= dataSetDescription ds
      , "columns" .= dataSetColumns ds
      ]

instance JSON.FromJSON DataSetColumn where
  parseJSON v =
    JSON.withObject "column" parseCol v
    where
      parseCol obj =
        DataSetColumn <$> reqField obj "name"
                      <*> optField obj "description"
                      <*> reqField obj "values"

instance JSON.ToJSON DataSetColumn where
  toJSON dsc =
    JSON.object
      [ "name" .= dataSetColumnName dsc
      , "description" .= dataSetColumnDescription dsc
      , "values" .= dataSetColumnValues dsc
      ]

dataSetFromDataSeries :: Text -> DataSeries Double -> DataSet
dataSetFromDataSeries name series =
    DataSet  { dataSetName = name
             , dataSetDescription = Nothing
             , dataSetColumns =
                (mkCol <$> Map.toList (Series.values series)) ++
                [mkCol ("time", Series.times series)]
             }
  where
    mkCol (n, vs) =
      DataSetColumn { dataSetColumnName = n
                    , dataSetColumnDescription = Nothing
                    , dataSetColumnValues = vs
                    }


-------------------------------------------------------------------------------

optField :: JSON.FromJSON a => JSON.Object -> Text -> JSONT.Parser (Maybe a)
optField obj n =
  case HM.lookup n obj of
    Nothing -> pure Nothing
    Just a -> Just <$> JSON.parseJSON a

reqField :: JSON.FromJSON a => JSON.Object -> Text -> JSONT.Parser a
reqField obj n =
  case HM.lookup n obj of
    Nothing -> JSONT.parseFail ("Could not find '" <> Text.unpack n <> "' key in object")
    Just a -> JSON.parseJSON a


