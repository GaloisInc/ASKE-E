{-# LANGUAGE OverloadedStrings #-}
module Exposure (tests) where

import Data.Bifunctor (Bifunctor(..))
import qualified Data.Text as Text
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Tasty.HUnit
import qualified Test.Tasty as Tasty

import Language.ASKEE.Exposure.GenLexer (lexExposure)
import Language.ASKEE.Exposure.GenParser (parseExposureExpr, parseExposureStmt)
import Language.ASKEE.Exposure.Interpreter
import Language.ASKEE.Exposure.Syntax
import Paths_aske_e (getDataDir)

assertRightStr :: Either String b -> IO b
assertRightStr (Right b)  = pure b
assertRightStr (Left err) = assertFailure err

assertRightText :: Either Text b -> IO b
assertRightText = assertRightStr . first Text.unpack

lexAndParseExpr :: String -> Either String Expr
lexAndParseExpr str = do
  lexed <- lexExposure str
  parseExposureExpr lexed

lexAndParseStmt :: String -> Either String Stmt
lexAndParseStmt str = do
  lexed <- lexExposure str
  parseExposureStmt lexed

exprShouldEvalTo :: String -> Value -> Assertion
exprShouldEvalTo actualExprStr expectedVal =
  exprAssertion actualExprStr $ \actualVal ->
    actualVal @?= expectedVal

exprAssertion :: String -> (Value -> Assertion) -> Assertion
exprAssertion = exprAssertionWithStmts []

exprAssertionWithStmts :: [String] -> String -> (Value -> Assertion) -> Assertion
exprAssertionWithStmts stmtStrs actualExprStr k = do
  stmts          <- assertRightStr $ traverse lexAndParseStmt stmtStrs
  actualExpr     <- assertRightStr $ lexAndParseExpr actualExprStr
  errOrEnv       <- evalStmts stmts initialEnv
  env            <- assertRightText errOrEnv
  errOrActualVal <- runEval env $ interpretExpr actualExpr
  (actualVal, _) <- assertRightText errOrActualVal
  k actualVal

getLoadSirEaselExpr :: IO String
getLoadSirEaselExpr = do
  dataDir <- getDataDir
  pure $ "loadESL(\"" ++ (dataDir </> "modelRepo/easel/sir.easel") ++ "\")"

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
      , testCase "Logical and" $
          "true and false" `exprShouldEvalTo` VBool False
      , testCase "Logical or" $
          "true or false" `exprShouldEvalTo` VBool True
      , testCase "Logical negation" $
          "not true" `exprShouldEvalTo` VBool False
      , testCase "Variable lookup" $
          exprAssertionWithStmts
            [ "x = 42"
            , "y = 22"
            ] "x + y" $ \actualVal ->
            actualVal @?= VDouble 64
      , testCase "loadESL should return a VModel" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          exprAssertion loadSirEaselExpr $ \modelVal ->
            case modelVal of
              VModelExpr (EVal (VModel _)) -> pure ()
              _                            -> assertFailure "Not a VModel"
      , testCase "Dot syntax should return an EMember" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          exprAssertion (loadSirEaselExpr ++ ".S") $ \modelVal ->
            case modelVal of
              VModelExpr (EMember _ "S") -> pure ()
              _                          -> assertFailure "Not an EMember"
      ]
    ]
