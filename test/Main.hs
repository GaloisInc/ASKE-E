module Main where

import Test.Tasty as Tasty
import qualified ModelCheck as MC

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup "All tests"
      [ Tasty.testGroup "Model check tests" MC.tests
      ]