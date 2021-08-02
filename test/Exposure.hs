{-# LANGUAGE OverloadedStrings #-}
module Exposure (tests) where

import qualified Data.ByteString.Lazy as LBS
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

assertLeft :: Either a b -> IO ()
assertLeft (Left _)  = pure ()
assertLeft (Right _) = assertFailure "Expected Left"

assertRightStr :: Either String b -> IO b
assertRightStr (Right b)  = pure b
assertRightStr (Left err) = assertFailure err

assertRightText :: Either Text b -> IO b
assertRightText = assertRightStr . first Text.unpack

lexAndParseExpr :: String -> Either String Expr
lexAndParseExpr s = do
  lexed <- lexExposure s
  parseExposureExpr lexed

lexAndParseStmt :: String -> Either String Stmt
lexAndParseStmt s = do
  lexed <- lexExposure s
  parseExposureStmt lexed

exprShouldEvalTo :: String -> Value -> Assertion
exprShouldEvalTo actualExprStr expectedVal =
  exprAssertion actualExprStr $ \actualVal ->
    actualVal @?= expectedVal

exprAssertion :: String -> (Value -> Assertion) -> Assertion
exprAssertion = exprAssertionWithStmts []

emptyEvalRead :: EvalRead
emptyEvalRead = mkEvalReadEnv LBS.readFile LBS.writeFile

exprAssertionWithStmts :: [String] -> String -> (Value -> Assertion) -> Assertion
exprAssertionWithStmts stmtStrs actualExprStr k = do
  stmts                  <- assertRightStr $ traverse lexAndParseStmt stmtStrs
  actualExpr             <- assertRightStr $ lexAndParseExpr actualExprStr
  (lr, env)              <- evalStmts stmts emptyEvalRead initialEnv
  (_, _)                 <- assertRightText lr
  (errOrActualVal, _, _) <- runEval emptyEvalRead env $ interpretExpr actualExpr
  actualVal              <- assertRightText errOrActualVal
  k actualVal

exprAssertionWithFailingStmts :: [String] -> String -> (Value -> Assertion) -> Assertion
exprAssertionWithFailingStmts stmtStrs actualExprStr k = do
  stmts      <- assertRightStr $ traverse lexAndParseStmt stmtStrs
  actualExpr <- assertRightStr $ lexAndParseExpr actualExprStr
  (lr, env)  <- evalStmts stmts emptyEvalRead initialEnv
  assertLeft lr
  (errOrActualVal, _, _) <- runEval emptyEvalRead env $ interpretExpr actualExpr
  actualVal              <- assertRightText errOrActualVal
  k actualVal

exprAssertion2 :: String -> String -> (Value -> Value -> Assertion) -> Assertion
exprAssertion2 = exprAssertion2WithStmts []

exprAssertion2WithStmts :: [String] -> String -> String -> (Value -> Value -> Assertion) -> Assertion
exprAssertion2WithStmts stmtStrs exprStr1 exprStr2 k = do
  stmts             <- assertRightStr $ traverse lexAndParseStmt stmtStrs
  expr1             <- assertRightStr $ lexAndParseExpr exprStr1
  expr2             <- assertRightStr $ lexAndParseExpr exprStr2
  (lr, env)         <- evalStmts stmts initialEnv
  (_, _)            <- assertRightText lr
  (errOrVal1, _, _) <- runEval emptyEvalRead env $ interpretExpr expr1
  (errOrVal2, _, _) <- runEval emptyEvalRead env $ interpretExpr expr2
  val1              <- assertRightText errOrVal1
  val2              <- assertRightText errOrVal2
  k val1 val2

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
      , testCase "Basic sampling" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          exprAssertionWithStmts
            [ "sir = " <> loadSirEaselExpr
            , "evt = sample(sir.I > 15 at 125.0, 5)"
            ] "P(evt) + P(not evt)" $ \actualVal ->
            actualVal @?= VDouble 1
      , testCase "Basic simulation" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          exprAssertionWithStmts
            [ "sir = " <> loadSirEaselExpr ]
            "simulate(sir.I at [1..10 by 1])" $ \simResults ->
            case simResults of
              VArray (VTimed _ _:_) -> pure ()
              _ -> assertFailure "Not an array of timed values"
      , testCase "Basic simulation (single point)" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          exprAssertionWithStmts
            [ "sir = " <> loadSirEaselExpr ]
            "simulate(sir.I at 125.0)" $ \simResults ->
            case simResults of
              VDouble _ -> pure ()
              _ -> assertFailure "Not a VDouble"
      , testCase "environment should update even upon failure" $
          exprAssertionWithFailingStmts
            [ "x = 42"
            , "notDefined"
            ] "x" $ \actualVal ->
            actualVal @?= VDouble 42
      , testCase "filter" $
          "filter([1 .. 10 by 1]) { x => x > 8 }" `exprShouldEvalTo` VArray [VDouble 9, VDouble 10]
      , testCase "MSE/MAE of identical results" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          exprAssertion2WithStmts
            [ "sir = " <> loadSirEaselExpr
            , "evt = sample(sir.I at 125.0, 1)"
            ] "mse(evt, evt)" "mae(evt, evt)" $ \val1 val2 ->
            do val1 @=? val2
               val1 @=? VDouble 0
      , testCase "MSE lifting" $ do
          exprAssertion2 "mse([1, 2, 3], [4, 4, 4])"
                         "mse([1, 2, 3], 4)"         $ \val1 val2 -> val1 @=? val2
          exprAssertion2 "mse(1,         [4, 5, 6])"
                         "mse([1, 1, 1], [4, 5, 6])" $ \val1 val2 -> val1 @=? val2
      , testCase "MAE lifting" $ do
          exprAssertion2 "mae([1, 2, 3], [4, 4, 4])"
                         "mae([1, 2, 3], 4)"         $ \val1 val2 -> val1 @=? val2
          exprAssertion2 "mae(1,         [4, 5, 6])"
                         "mae([1, 1, 1], [4, 5, 6])" $ \val1 val2 -> val1 @=? val2
      ]
    ]
