{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
module Exposure (tests) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as LBS
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (traverse_)
import Data.Functor ( void )
import qualified Data.Text as Text
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Tasty.HUnit
import qualified Test.Tasty as Tasty

import Language.ASKEE.Exposure.GenLexer (lexExposure)
import Language.ASKEE.Exposure.GenParser (parseExposureExpr, parseExposureStmt)
import Language.ASKEE.Exposure.Interpreter
import Language.ASKEE.Exposure.Syntax

import Language.ASKEE.Exposure.Python (newPythonHandle, closePythonHandle, PythonHandle)
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

exprShouldEvalTo ::
  (?getEvalRead :: IO EvalRead) => String -> Value -> Assertion
exprShouldEvalTo actualExprStr expectedVal =
  exprAssertion actualExprStr $ \actualVal ->
    actualVal @?= expectedVal

exprAssertion ::
  (?getEvalRead :: IO EvalRead) => String -> (Value -> Assertion) -> Assertion
exprAssertion = exprAssertionWithStmts []


exprAssertionWithStmts ::
  (?getEvalRead :: IO EvalRead) => [String] -> String -> (Value -> Assertion) -> Assertion
exprAssertionWithStmts stmtStrs actualExprStr k = do
  er                     <- ?getEvalRead
  stmts                  <- assertRightStr $ traverse lexAndParseStmt stmtStrs
  actualExpr             <- assertRightStr $ lexAndParseExpr actualExprStr
  (lr, env)              <- evalLoop er initialEnv stmts
  (_, _)                 <- assertRightText lr
  (errOrActualVal, _, _) <- runEval er env $ interpretExpr actualExpr
  actualVal              <- assertRightText errOrActualVal
  k actualVal

exprAssertionWithFailingStmts ::
  (?getEvalRead :: IO EvalRead) => [String] -> String -> (Value -> Assertion) -> Assertion
exprAssertionWithFailingStmts stmtStrs actualExprStr k = do
  er         <- ?getEvalRead
  actualExpr <- assertRightStr $ lexAndParseExpr actualExprStr
  env        <- assertStmtsFail stmtStrs
  (errOrActualVal, _, _) <- runEval er env $ interpretExpr actualExpr
  actualVal              <- assertRightText errOrActualVal
  k actualVal

assertStmtsFail :: (?getEvalRead :: IO EvalRead) => [String] -> IO Env
assertStmtsFail stmtStrs = do
  er         <- ?getEvalRead
  stmts      <- assertRightStr $ traverse lexAndParseStmt stmtStrs
  (lr, env)  <- evalLoop er initialEnv stmts
  assertLeft lr
  return env

emptyEvalRead :: PythonHandle -> EvalRead
emptyEvalRead = mkEvalReadEnv rd wr
  where
    wr f d =
      do res <- try (LBS.writeFile f d)
         case res of
           Left (_ :: SomeException) ->
             pure $ Left "write failed"
           Right t -> pure $ Right t

    rd f =
      do res <- try (LBS.readFile f)
         case res of
           Left (ex :: SomeException) -> do
             print ex
             pure $ Left "read failed"
           Right t -> pure $ Right t

exprAssertion2 ::
  (?getEvalRead :: IO EvalRead) =>
  String -> String -> (Value -> Value -> Assertion) -> Assertion
exprAssertion2 = exprAssertion2WithStmts []

exprAssertion2WithStmts ::
  (?getEvalRead :: IO EvalRead) =>
  [String] -> String -> String -> (Value -> Value -> Assertion) -> Assertion
exprAssertion2WithStmts stmtStrs exprStr1 exprStr2 k = do
  er                <- ?getEvalRead
  stmts             <- assertRightStr $ traverse lexAndParseStmt stmtStrs
  expr1             <- assertRightStr $ lexAndParseExpr exprStr1
  expr2             <- assertRightStr $ lexAndParseExpr exprStr2
  (lr, env)         <- evalLoop er initialEnv stmts
  (_, _)            <- assertRightText lr
  (errOrVal1, _, _) <- runEval er env $ interpretExpr expr1
  (errOrVal2, _, _) <- runEval er env $ interpretExpr expr2
  val1              <- assertRightText errOrVal1
  val2              <- assertRightText errOrVal2
  k val1 val2

getLoadSirEaselExpr :: IO String
getLoadSirEaselExpr = do
  dataDir <- getDataDir
  pure $ "loadESL(\"" ++ (dataDir </> "modelRepo/easel/sir.easel") ++ "\")"

assertDouble :: Value -> IO ()
assertDouble VDouble{} = pure ()
assertDouble v         = assertFailure $ "Expected a double, received: " ++ show v

assertTimedArray :: Value -> IO ()
assertTimedArray (VTimed VArray{} _) = pure ()
assertTimedArray v                    = assertFailure $ "Expected a timed array, received: " ++ show v

tests :: Tasty.TestTree
tests = Tasty.withResource acquire release $ \getEvalRead ->
          let ?getEvalRead = getEvalRead in makeTests
  where
    acquire = emptyEvalRead <$> newPythonHandle []
    release = closePythonHandle . erPyHandle

makeTests :: (?getEvalRead :: IO EvalRead) => Tasty.TestTree
makeTests =
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
      , testCase "String with whitespace" $ do
          exprAssertion "\"Hello World\"" $ \actualVal ->
            actualVal @?= VString "Hello World"
      , testCase "Name shadowing" $ do
          exprAssertionWithStmts
            [ "x = 3.0"
            , "x = 5.0"
            , "x"
            ] "x" $ \v -> v @?= VDouble 5.0
      , testCase "Param fitting (success)" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          exprAssertion2WithStmts
            [ "sir = " <> loadSirEaselExpr
            , "series = simulate(sir at [0..120 by 30])"
            , "T = time(series)"
            , "vs = value(series)"
            , "ps = fit(sir, "
                    ++ "{{time=T, S=vs.S, I=vs.I, R=vs.R}}, "
                    ++ "[\"i_initial\"])"
            ] "ps.values.i_initial" "ps.errors.i_initial" $ \val1 val2 ->
            case (val1, val2) of
              (VDouble _, VDouble _) -> pure ()
              _ -> assertFailure "fit did not return a point with expected shape"
      , testCase "Param fitting (failure)" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          const () <$>
            assertStmtsFail
            [ "sir = " <> loadSirEaselExpr
            , "series = simulate(sir at [0..120 by 30])"
            , "T = time(series)"
            , "vs = value(series)"
            , "ps = fit(sir, "
                    ++ "{{time=T, S=vs.S, I=vs.I, R=vs.R}}, "
                    ++ "[\"I_initial\"])"
            ]
      , testCase "Join (bad model args)" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          void $ assertStmtsFail
            [ "sir = "<>loadSirEaselExpr
            , "join(sir, sir, [])" ]
      , testCase "Join (bad share args)" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          void $ assertStmtsFail
            [ "sir = "<>loadSirEaselExpr
            , "join([\"_1\", sir], [\"_2\", sir], [\"S\", \"S\"])" ]
      , testCase "Join (success)" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          exprAssertionWithStmts
            [ "sir = "<>loadSirEaselExpr
            ] "join([\"_1\", sir], [\"_2\", sir], [[\"S\", \"S\"]])" $ \v ->
                case v of
                  VModel _ -> pure ()
                  VModelExpr (EVal (VModel _)) -> pure ()
                  x -> assertFailure $ "joining didn't produce a model, instead a "<>show x
      , testCase "Model Skill Ranking" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          exprAssertionWithStmts
           [ "sir = "<>loadSirEaselExpr
           , "sir1 = withParams(sir, {{beta=0.5}})"
           , "sir2 = withParams(sir, {{beta=0.6}})"
           , "times = [50.0, 100.0, 150.0]"
           , "ground_truth = value(sample(sir.I at times, 100))"
           , "sir1_samples = value(sample(sir1.I at times, 100))"
           , "sir2_samples = value(sample(sir2.I at times, 100))"
           , "ranking = modelSkillRank([sir1_samples, ground_truth, sir2_samples], [0.02, 0.1, 0.8], mean(ground_truth))"
           ] "ranking" $ \v ->
            case v of
              VArray [r1, r2, r3] ->
                do assertBool "Ground Truth should be better than sir1" (r2 > r1)
                   assertBool "sir1 should be better than sir2" (r1 > r3)
              _ -> assertFailure $ "modelSkillRank didn't produce a three element array"
      , testCase "Linear combination of samples" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr
          let failure = assertFailure "Linear combination of samples did not return an array"

          exprAssertionWithStmts
            [ "sir = " <> loadSirEaselExpr
            , "is1 = sample(sir.I at [0, 30, 60], 1)"
            , "is2 = sample(sir.I at [0, 30, 60], 1)"
            ] "3 * is1 + 5 * is2" $ \v ->
            case v of
              VArray vs -> traverse_ assertTimedArray vs
              _         -> failure

          exprAssertionWithStmts
            [ "sir = " <> loadSirEaselExpr
            , "is1 = sample(sir.I at 60, 1)"
            , "is2 = sample(sir.I at 60, 1)"
            ] "3 * is1 + 5 * is2" $ \v ->
            case v of
              VArray vs -> traverse_ assertDouble vs
              _         -> failure
      , testCase "Expression indexing" $ do
          loadSirEaselExpr <- getLoadSirEaselExpr

          exprAssertion "[0.0, 1.0][1]" $ \v ->
            v @?= VDouble 1.0

          exprAssertionWithStmts
            [ "sir = " <> loadSirEaselExpr ]
            "simulate(sir[\"I\"] at 1.0)" $ \v ->
            case v of
              VDouble _ -> pure ()
              _ -> assertFailure "Simulate of indexed expr returned wrong type"

          exprAssertionWithStmts
            [ "sir = " <> loadSirEaselExpr
            , "r = simulate(sir at 1.0)"
            ] "r[\"I\"]" $ \v ->
            case v of
              VDouble _ -> pure ()
              _ -> assertFailure "Index of simulate returned wrong type"


      , testCase "Load missing files" $ do
          void $ assertStmtsFail ["loadCSV(\"path/to/nothing.csv\")"]
          void $ assertStmtsFail ["loadESL(\"path/to/nothing.easel\")"]

      , testCase "Call Python extension" $ do
          exprAssertion "@id(5.0 + 5.0)" $ \v ->
            v @?= VDouble 10.0
      ]
    ]
