{-# Language OverloadedStrings #-}
module Language.ASKEE.Experiments where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import qualified Language.ASKEE.DiffEq.GenLexer as DL
import qualified Language.ASKEE.DiffEq.GenParser as DP
import           Language.ASKEE.DiffEq.DiffEq (DiffEq, EqGen)
import qualified Language.ASKEE.GenLexer as AL
import qualified Language.ASKEE.GenParser as AP
import           Language.ASKEE.Lexer (Token, Located)
import           Language.ASKEE.Syntax (Model)
import qualified Language.ASKEE.SimulatorGen as SG
import qualified Language.ASKEE.Measure as M
import qualified Language.ASKEE.MeasureToCPP as MG
import qualified Language.ASKEE.Core as Core
import           Language.ASKEE.Core.ImportASKEE(modelAsCore)


testLexModel :: FilePath -> IO [Located Token]
testLexModel fp =
  do txt <- readFile fp
     case AL.lexModel txt of
       Right toks -> pure toks
       Left err -> fail err

testParseModel :: FilePath -> IO Model
testParseModel fp = AP.parse <$> testLexModel fp

coreModel :: FilePath -> IO Core.Model
coreModel fp =
  do mb <- modelAsCore <$> testParseModel fp
     case mb of
       Right a  -> pure a
       Left err -> fail err

dump :: EqGen (Map Text [Double]) -> FilePath -> IO ()
dump ~(Right m) fp = writeFile fp $ show $ Map.toList m 

sir, sirs, sirVD :: [Char]
sir = "examples/askee/sir.askee"
sirs = "examples/askee/sirs.askee"
sirVD = "examples/askee/sir-vd.askee"

p :: String -> [DiffEq]
p = DP.parse . DL.alexScanTokens

genCppModel :: FilePath -> FilePath -> IO ()
genCppModel fp output =
  do compiled <- coreModel fp
     let rendered = show (SG.genModel compiled)
     writeFile output rendered
     putStrLn "compiled!"

m1 :: M.Measure
m1 = M.EventBased
   $ M.When (M.TimeLT 120.0)
   $ M.Do
   $ M.Accumulate "m_sum" 1.0
   $ Core.Op2 Core.Add (Core.Var "m_sum") (Core.Literal (Core.Num 1.0))

m2 :: M.Measure
m2 = M.EventBased
   $ M.When (M.TimeLT 120.0)
   $ M.Do
   $ M.TraceExpr "i_trace" (Core.Var "I")

m3 :: M.Measure
m3 = M.EventBased
   $ M.When (M.TimeLT 120.0)
   $ M.Do
   $ M.TraceExpr "i_trace" (Core.Var "time")

m4 :: M.Measure
m4 = M.TimeBased [M.AtTimes 1 15 120, M.AtTime 137]
   $ M.Do
   $ M.TraceExpr "i_trace" (Core.Var "time")



exMeasure :: M.Measure
exMeasure = m1 M.:+: m2

genCppRunner :: FilePath -> IO ()
genCppRunner fp =
  do compiled <- coreModel fp
     putStrLn $ show $ MG.genSimulationRunnerCpp compiled 100.0 m4
