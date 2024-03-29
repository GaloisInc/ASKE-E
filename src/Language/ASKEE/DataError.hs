{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.DataError where

import Data.Text(Text)
import qualified Data.Aeson as JSON
import Data.Aeson((.=))


interpolateJoinPointsLinear :: [(Double, Double)] -> [(Double, Double)] -> [((Double, Double), Double)]
interpolateJoinPointsLinear d when =
  case (d, when) of
    ((d1,t1):d'@((d2, t2):_), (c1,t):t')
      | t1 == t -> ((d1,c1),t) : interpolateJoinPointsLinear d t'
      | t2 == t -> ((d2,c1),t) : interpolateJoinPointsLinear d t'
      | t1 <= t && t <= t2 && t1 /= t2 ->
        let p = d1 + (t - t1) * (d2 - d1) / (t2 - t1)
        in ((p,c1),t) : interpolateJoinPointsLinear d t'

      | otherwise -> interpolateJoinPointsLinear d' when
    _ -> []

parseErrorMeasurement :: Text -> Maybe ErrorMeasurement
parseErrorMeasurement t =
  case t of
    "L2" -> Just L2Norm
    _ -> Nothing

parseInterpolationMethod :: Text -> Maybe Interpolation
parseInterpolationMethod t =
  case t of
    "linear" -> Just Linear
    _ -> Nothing

measureError :: ErrorMeasurement -> [Double] -> Double
measureError m errs =
  case m of
    L2Norm -> sum $ sq <$> errs
  where
    sq a = a * a

computeError :: MeasureErrorRequest -> DataErrorSummary
computeError mer =
    DataErrorSummary { desMeasures = columns
                     , desTotal = totalError
                     }
  where
    totalError = summarize $ concat (cesError <$> columns)
    columns = column <$> merMeasures mer
    summarize = measureError (merErrorMeasurement mer)
    observed ed = medObserved ed `zip` medObservedTimes ed
    predicted ed = medPredicted ed `zip` medPredictedTimes ed
    column ed =
      let joinedData = fst <$> interpolateJoinPointsLinear (predicted ed) (observed ed)
          errors = uncurry (-) <$> joinedData
          byVar = summarize errors
      in ColumnErrorSummary (medName ed) byVar errors

data ErrorMeasurement =
  L2Norm
  deriving Show

data Interpolation =
  Linear
  deriving Show

data MeasureErrorData = MeasureErrorData
  { medName :: Text
  , medObserved :: [Double]
  , medObservedTimes :: [Double]
  , medPredicted :: [Double]
  , medPredictedTimes :: [Double]
  }
  deriving Show

data MeasureErrorRequest = MeasureErrorRequest
  { merErrorMeasurement :: ErrorMeasurement
  , merInterpolation :: Interpolation
  , merMeasures :: [MeasureErrorData]
  }
  deriving Show

data DataErrorSummary = DataErrorSummary
  { desMeasures :: [MeasureErrorSummary]
  , desTotal :: Double
  }
  deriving Show

data MeasureErrorSummary = ColumnErrorSummary
  { cesName :: Text
  , cesTotal :: Double
  , cesError :: [Double]
  }
  deriving Show

instance JSON.ToJSON DataErrorSummary where
  toJSON des =
    JSON.object [ "measures" .= desMeasures des
                , "error_total" .= desTotal des
                ]

instance JSON.ToJSON MeasureErrorSummary where
  toJSON ces =
    JSON.object [ "uid" .= cesName ces
                , "error_ind" .= cesError ces
                , "error_total" .= cesTotal ces
                ]


