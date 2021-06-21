module Exposure (tests) where

import qualified Data.Text as Text
import Test.Tasty.HUnit
import qualified Test.Tasty as Tasty

import Language.ASKEE.Exposure.GenLexer
import Language.ASKEE.Exposure.GenParser
import Language.ASKEE.Exposure.Interpreter
import Language.ASKEE.Exposure.Syntax

exprShouldEvalTo :: String -> Value -> Assertion
exprShouldEvalTo actualExprStr expectedVal =
  case lexExposure actualExprStr >>= parseExposureExpr of
    Left err         -> fail err
    Right actualExpr -> do
      res <- runEval initialEnv $ interpretExpr actualExpr
      case res of
        Left err             -> fail $ Text.unpack err
        Right (actualVal, _) -> actualVal @?= expectedVal

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "Exposure API Tests"
    [ Tasty.testGroup "Interpreter tests"
      [ testCase "Addition" $
          "4 + 2" `exprShouldEvalTo` VDouble 6
      , testCase "Subtraction" $
          "4 - 2" `exprShouldEvalTo` VDouble 2
      , testCase "Multiplication" $
          "4 * 2" `exprShouldEvalTo` VDouble 8
      , testCase "Division" $
          "4 / 2" `exprShouldEvalTo` VDouble 2
      , testCase "Greater than" $
          "4 > 2" `exprShouldEvalTo` VBool True
      , testCase "Greater than or equal to" $
          "4 >= 2" `exprShouldEvalTo` VBool True
      , testCase "Less than" $
          "4 < 2" `exprShouldEvalTo` VBool False
      , testCase "Less than or equal to" $
          "4 <= 2" `exprShouldEvalTo` VBool False
      ]
    ]
