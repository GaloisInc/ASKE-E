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
  , plotColor       :: Maybe PlotColor
  }
  deriving (Show, Eq, Ord, Generic, NFData)

data PlotStyle
  = Points
  | Circles
  | Squares
  | Line
  deriving (Show, Eq, Ord, Generic, NFData)

data PlotColor
  = ColorScheme Text
    -- ^ A color scheme, as described in
    --   <https://vega.github.io/vega/docs/schemes/>.
  | ColorRange [Text]
    -- ^ A range of colors, as described in
    --   <https://vega.github.io/vega-lite/docs/scale.html#2-setting-the-range-property-to-an-array-of-valid-css-color-strings>.
  deriving (Show, Eq, Ord, Generic, NFData)

instance JS.ToJSON PlotStyle where
  toJSON Points  = "point"
  toJSON Circles = "circle"
  toJSON Squares = "square"
  toJSON Line    = "line"

instance JS.ToJSON PlotColor where
  toJSON (ColorScheme scheme) = JS.toJSON scheme
  toJSON (ColorRange range)   = JS.toJSON range
