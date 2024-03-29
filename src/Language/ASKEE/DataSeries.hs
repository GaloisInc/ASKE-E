{-# Language OverloadedStrings, ParallelListComp, BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.ASKEE.DataSeries
  ( -- * Basics
    DataSeries(..)
  , emptyDataSeries
  , dataSeries
    -- * Saving an loading
  , parseDataSeries
  , parseDataSeriesFromFile
  -- , MalformedDataSeries(..)
  , dataSeriesAsCSV
  , dataSeriesAsJSON
  , saveDataSeries
    -- * Plotting utility
  , gnuPlotScript
    -- * Manipulation
  , zipAlignedWithTimeAndLabel
  , zipAlignedWithTime
  , zipAligned
    -- * Queires
  , dsColumns
  , foldDataSeries
  , foldDataSeriesWithTime
  , dsLookup
    -- * Labelled
  , LabeledDataSeries(..)
  , ldsFromDs
    -- * Data points
  , DataPoint(..)
  , toDataPoints
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text(Text)
import Data.Text.Encoding(decodeUtf8',encodeUtf8)
import qualified Data.Vector as Vector
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(sortBy,transpose,foldl')
import Data.Function(on)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import Language.ASKEE.Panic(panic)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT
import Data.Aeson((.=))
import Language.ASKEE.Error (ASKEEError(DataSeriesError), throwLeft)

-- XXX: We could use a representation that allows for easier access to
-- particular data points.
data DataSeries a = DataSeries
  { times  :: [Double]
  , values :: Map Text [a]
  }
  deriving (Show, Eq, Ord, Generic, NFData)

instance Functor DataSeries where
  fmap f ds = ds { values = fmap f <$> values ds }

emptyDataSeries :: [Text] -> DataSeries a
emptyDataSeries keys = DataSeries
  { times  = []
  , values = Map.fromList [ (k,[]) | k <- keys ]
  }

-- | Build a dataseries out of the given data.
dataSeries :: [Text] -> [(Double,[a])] -> DataSeries a
dataSeries labs ds = DataSeries
  { times  = map fst ds
  , values = Map.fromList [ (k,vs) | vs <- transpose (map snd ds)
                                   | k  <- labs ]
  }


zipAlignedWithTimeAndLabel ::
  (Text -> Double -> a -> b -> c) ->
  DataSeries a -> DataSeries b -> DataSeries c
zipAlignedWithTimeAndLabel f ds1 ds2 =
  ds1 { values = Map.intersectionWithKey (\k -> zipWith3 (f k) (times ds1))
                                      (values ds1) (values ds2) }


zipAlignedWithTime ::
  (Double -> a -> b -> c) ->
  DataSeries a -> DataSeries b -> DataSeries c
zipAlignedWithTime f = zipAlignedWithTimeAndLabel \_ -> f

zipAligned ::
  (a -> b -> c) ->
  DataSeries a -> DataSeries b -> DataSeries c
zipAligned f = zipAlignedWithTime (\_ -> f)

dsColumns :: DataSeries a -> Int
dsColumns ds = 1 + Map.size (values ds)

foldDataSeriesWithTime ::
  (Double -> a -> b -> b) -> Map Text b -> DataSeries a -> Map Text b
foldDataSeriesWithTime f start ds = Map.mapWithKey doFold (values ds)
  where
  doFold k xs = foldl' step (start Map.! k) (zip (times ds) xs)
  step b (t,a) = f t a b

foldDataSeries ::
  (a -> b -> b) -> Map Text b -> DataSeries a -> Map Text b
foldDataSeries f = foldDataSeriesWithTime (\_ -> f)



dsLookup :: DataSeries a -> Double -> Map Text a
dsLookup ds t = getVal <$> values ds
  where getVal vs = case [ v | (t',v) <- zip (times ds) vs, t' >= t ] of
                      v : _ -> v
                      []    -> last vs



--------------------------------------------------------------------------------

-- yikes
toDataPoints :: DataSeries Double -> [DataPoint]
toDataPoints xs =
  [ DataPoint { ptTime = t, ptValues = Map.fromList pts }
  | (t,pts) <- zip (times xs)
             $ transpose [ [ (l,d) | d <- ds ]
                         | (l,ds) <- Map.toList (values xs)
                         ]
  ]

data DataPoint = DataPoint
  { ptTime :: Double
  , ptValues :: Map Text Double
  }

addPoint :: DataPoint -> DataSeries Double -> DataSeries Double
addPoint p x =
  DataSeries { times = ptTime p : times x
             , values = Map.mergeWithKey (\_ a as -> Just (a:as)) nope id
                                        (ptValues p) (values x)
             }
  where
  nope = panic "addPoint" ["not enoguh values"]

parseDataSeries :: LBS.ByteString -> Either String (DataSeries Double)
parseDataSeries bs =
  do (hdr,pts) <- CSV.decodeByName bs
     keys <- case mapM decodeUtf8' (Vector.toList hdr) of
               Left _ -> Left "Malformed filed names"
               Right a -> pure a
     pure $ foldr addPoint (emptyDataSeries (filter (/= "time") keys))
          $ sortBy (compare `on` ptTime)
          $ Vector.toList pts

-- | Parser the data throws and exception on eror
parseDataSeriesFromFile :: FilePath -> IO (DataSeries Double)
parseDataSeriesFromFile file =
  do bs <- LBS.readFile file
     throwLeft DataSeriesError (parseDataSeries bs)

-- | Encode a data series to a lazy bytestring
dataSeriesAsCSV :: DataSeries Double -> LBS.ByteString
dataSeriesAsCSV xs = CSV.encodeByNameWith opts hdr (toDataPoints xs)
  where
  sep = toEnum (fromEnum ',')
  hdr = Vector.fromList ("time" : map encodeUtf8 (Map.keys (values xs)))
  opts = CSV.defaultEncodeOptions { CSV.encDelimiter = sep
                                  , CSV.encIncludeHeader = True
                                  }

-- XXX: how do we document this?
dataSeriesAsJSON :: DataSeries Double -> Aeson.Value
dataSeriesAsJSON ds = Aeson.object
  [ "times" .= times ds
  , "values" .= Aeson.object [ x Aeson..= ys | (x,ys) <- Map.toList (values ds) ]
  ]

instance Aeson.ToJSON (DataSeries Double) where
  toJSON = dataSeriesAsJSON





saveDataSeries :: FilePath -> DataSeries Double -> IO ()
saveDataSeries file xs = LBS.writeFile file (dataSeriesAsCSV xs)

gnuPlotScript :: DataSeries Double -> FilePath -> String
gnuPlotScript ds f =
  unlines
    [ "set key outside"
    , "set datafile separator \",\""
    , "plot for [col=2:" ++ show (dsColumns ds) ++ "] " ++
         show f ++ " using 1:col with lines title columnheader"
    ]


instance CSV.FromNamedRecord DataPoint where
  parseNamedRecord m =
    case HashMap.lookup "time" m of
      Just x  ->
        do t <- CSV.parseField x
           let getF (l,f) =
                 case decodeUtf8' l of
                   Right a ->
                      do d <- CSV.parseField f
                         pure (a,d)
                   Left _ -> fail ("Malformed heading: " ++ show x)

           hm <- traverse getF (HashMap.toList (HashMap.delete "time" m))
           pure DataPoint { ptTime = t, ptValues = Map.fromList hm }
      Nothing -> fail "Missing entry for `time`"

instance CSV.ToNamedRecord DataPoint where
  toNamedRecord a =
    CSV.namedRecord
      $ CSV.namedField "time" (ptTime a)
      : [ CSV.namedField (encodeUtf8 l) d | (l,d) <- Map.toList (ptValues a) ]

-------------------------------------------------------------------------------
-- Labelled data source (for donu)

-- DataSource labelled with name of the x-axis
data LabeledDataSeries a = LabeledDataSeries
  { ldsData  :: DataSeries a
  , ldsLabel :: Text
  }
  deriving Show

ldsFromDs :: DataSeries a -> LabeledDataSeries a
ldsFromDs ds = LabeledDataSeries ds "times"

instance Aeson.ToJSON (LabeledDataSeries Double) where
  toJSON lds =
    Aeson.object [ "values" .= (values . ldsData $ lds)
                 , ldsLabel lds .= (times . ldsData $ lds)
                 ]

instance Aeson.FromJSON (LabeledDataSeries Double) where
  parseJSON = Aeson.withObject "root" parseRoot
    where
      nonValuesKeys o = filter (\(n, _) -> n /= "values") (HashMap.toList o)

      parseLabel o =
        case nonValuesKeys o of
          [(nm,val)] -> (nm,) <$> Aeson.parseJSON val
          _ -> AT.parseFail "DataSeries does not have unique domain parameter"

      parseValues o =
        case HashMap.lookup "values" o of
          Just v -> Aeson.parseJSON v
          Nothing -> AT.parseFail "DataSeries does not have values"

      parseRoot o =
        do  (dpName, dpValue) <- parseLabel o
            vals <- parseValues o
            pure $ LabeledDataSeries
                    { ldsData = DataSeries dpValue vals
                    , ldsLabel = dpName
                    }
