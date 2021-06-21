module Exposure (tests) where

import qualified Data.Text as Text
import System.FilePath ((</>))
import Test.Tasty.HUnit
import qualified Test.Tasty as Tasty

import Language.ASKEE.Exposure.GenLexer
import Language.ASKEE.Exposure.GenParser
import Language.ASKEE.Exposure.Interpreter
import Language.ASKEE.Exposure.Syntax
import Paths_aske_e (getDataDir)

exprShouldEvalTo :: String -> Value -> Assertion
exprShouldEvalTo actualExprStr expectedVal =
  exprAssertion actualExprStr $ \actualVal ->
    actualVal @?= expectedVal

exprAssertion :: String -> (Value -> Assertion) -> Assertion
exprAssertion actualExprStr k = do
  case lexExposure actualExprStr >>= parseExposureExpr of
    Left err         -> fail err
    Right actualExpr -> do
      res <- runEval initialEnv $ interpretExpr actualExpr
      case res of
        Left err             -> fail $ Text.unpack err
        Right (actualVal, _) -> k actualVal

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
      , testCase "Equal to" $
          "4 == 2" `exprShouldEvalTo` VBool False
      , testCase "Not equal to" $
          "4 != 2" `exprShouldEvalTo` VBool True
      , testCase "loadESL should return a VModelExpr" $ do
          dataDir <- getDataDir
          exprAssertion ("loadESL(\"" ++ (dataDir </> "modelRepo/easel/sir.easel") ++ "\")") $ \modelVal ->
            case modelVal of
              VModelExpr (EVal (VModel _)) -> pure ()
              _                            -> fail "Not a VModelExpr"
      ]
    ]
