{-# Language OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.ASKEE.Exposure.Plot where

import GHC.Generics (Generic)
import Control.DeepSeq(NFData)
import qualified Data.Aeson as JS
import Data.Text(Text)

data Plot a = Plot
  { plotTitle  :: Text
  , plotSeries :: [PlotSeries a]
  , plotVs     :: [Double]
  , plotVsLabel :: Text
  }
  deriving (Show, Eq, Ord, Generic, NFData)

data PlotSeries a = PlotSeries
  { plotSeriesLabel :: Text
  , plotSeriesData  :: a
  , plotSeriesStyle :: PlotStyle
  , plotColor       :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, NFData)

data PlotStyle
  = Points
  | Circles
  | Squares
  | Line
  deriving (Show, Eq, Ord, Generic, NFData)

instance JS.ToJSON PlotStyle where
  toJSON Points  = "point"
  toJSON Circles = "circle"
  toJSON Squares = "square"
  toJSON Line    = "line"
