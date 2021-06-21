module Main where

import Test.Tasty as Tasty
import qualified ModelCheck as MC
import qualified ASKEE as ASKEE
import qualified Exposure as Exposure

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup "All tests"
      [ Tasty.testGroup "Model check tests" MC.tests
      , ASKEE.tests
      , Exposure.tests
      ]
