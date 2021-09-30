module Main where

import qualified ASKEE
import qualified Exposure as Exposure
import qualified ModelCheck
import qualified SBMLL2V3
import qualified SBMLL3V2
import qualified Storage

import Test.Tasty as Tasty ( testGroup, defaultMain )

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup "All tests"
      [ ModelCheck.tests
      , ASKEE.tests
      , Storage.tests
      , Exposure.tests
      , SBMLL2V3.tests
      , SBMLL3V2.tests
      ]
