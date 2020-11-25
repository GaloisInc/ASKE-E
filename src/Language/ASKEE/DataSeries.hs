{-# Language OverloadedStrings #-}
module Language.ASKEE.DataSeries
  ( -- * Basics
    DataSeries(..)
  , emptyDataSeries
    -- * Saving an loading
  , parseDataSeries
  , parseDataSeriesFromFile
  , MalformedDataSeries(..)
  , encodeDataSeries
  , saveDataSeries
    -- * Manipulation
  , zipAlignedWithTime
  , zipAligned
    -- * Queires
  , dsColumns
  , foldDataSeries
  , foldDataSeriesWithTime
  ) where

import Data.Word(Word8)
import Data.Text(Text)
import Data.Text.Encoding(decodeUtf8',encodeUtf8)
import qualified Data.Vector as Vector
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(sortBy,transpose,foldl')
import Data.Function(on)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy as LBS
import Control.Exception(Exception,throwIO)

import qualified Data.Csv as CSV
import Language.ASKEE.Panic(panic)

data DataSeries a = DataSeries
  { times  :: [Double]
  , values :: Map Text [a]
  }

emptyDataSeries :: [Text] -> DataSeries a
emptyDataSeries keys = DataSeries
  { times  = []
  , values = Map.fromList [ (k,[]) | k <- keys ]
  }

zipAlignedWithTime ::
  (Double -> a -> b -> c) ->
  DataSeries a -> DataSeries b -> DataSeries c
zipAlignedWithTime f ds1 ds2 =
  ds1 { values = Map.intersectionWith (zipWith3 f (times ds1))
                                      (values ds1) (values ds2) }

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
     pure $ foldr addPoint (emptyDataSeries keys)
          $ sortBy (compare `on` ptTime)
          $ Vector.toList pts

-- | Parser the data throws and exception on eror
parseDataSeriesFromFile :: FilePath -> IO (DataSeries Double)
parseDataSeriesFromFile file =
  do bs <- LBS.readFile file
     case parseDataSeries bs of
       Left err -> throwIO (MalformedDataSeries err)
       Right a  -> pure a

data MalformedDataSeries = MalformedDataSeries String deriving Show

instance Exception MalformedDataSeries

-- | Encode a data series to a lazy bytestring
encodeDataSeries :: Word8 -> DataSeries Double -> LBS.ByteString
encodeDataSeries sep xs = CSV.encodeByNameWith opts hdr (toDataPoints xs)
  where
  hdr = Vector.fromList ("time" : map encodeUtf8 (Map.keys (values xs)))
  opts = CSV.defaultEncodeOptions { CSV.encDelimiter = sep
                                  , CSV.encIncludeHeader = True
                                  }

saveDataSeries :: FilePath -> Word8 -> DataSeries Double -> IO ()
saveDataSeries file sep xs = LBS.writeFile file (encodeDataSeries sep xs)




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

