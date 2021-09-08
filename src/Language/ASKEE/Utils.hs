module Language.ASKEE.Utils where

import Data.Text(Text)
import Prettyprinter
import Prettyprinter.Render.Text
import System.Random as Random

render :: Doc ann -> Text
render = renderStrict . layoutPretty defaultLayoutOptions

withUniformNoise :: Random.StdGen -> [Double] -> Double -> [Double]
withUniformNoise gen ns i =
  zipWith modVal (Random.randomRs (-i, i) gen) ns
  where
    modVal delta n = n + n * delta
