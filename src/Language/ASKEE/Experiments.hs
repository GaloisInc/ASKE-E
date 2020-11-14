{-# Language OverloadedStrings #-}
module Language.ASKEE.Experiments where

import Data.Map (toList, Map)
import Data.Text (Text)

import qualified Language.ASKEE.DiffEq.GenLexer as DL
import qualified Language.ASKEE.DiffEq.GenParser as DP
import           Language.ASKEE.DiffEq.DiffEq (DiffEq, EqGen)
import qualified Language.ASKEE.GenLexer as AL
import qualified Language.ASKEE.GenParser as AP
import           Language.ASKEE.Lexer (Token, Located)
import           Language.ASKEE.Syntax (Model)
import qualified Language.ASKEE.SimulatorGen as SG
import qualified Language.ASKEE.ExprTransform as Transform
import qualified Text.PrettyPrint as PP
import qualified Language.ASKEE.Measure as M
import qualified Language.ASKEE.Core as Core


import System.IO.Unsafe (unsafePerformIO)


testLexModel :: FilePath -> [Located Token]
testLexModel fp = 
  let model = unsafePerformIO $ readFile fp
      Right toks = AL.lexModel model
  in  toks

testParseModel :: FilePath -> Model
testParseModel fp =
  let toks = testLexModel fp
  in  AP.parse toks

dump :: EqGen (Map Text [Double]) -> FilePath -> IO ()
dump (Right m) fp = writeFile fp $ show $ toList m 

sir, sirs, sirVD :: [Char]
sir = "examples/askee/sir.askee"
sirs = "examples/askee/sirs.askee"
sirVD = "examples/askee/sir-vd.askee"

deqs :: String
deqs = unsafePerformIO $ readFile "/Users/sam/Desktop/projects/aske-e/aske-e/equations.txt"

p :: String -> [DiffEq]
p = DP.parse . DL.alexScanTokens

genCppModel :: FilePath -> FilePath -> IO ()
genCppModel fp output =
  do let mdl = testParseModel fp
     case Transform.modelAsCore mdl of
       Left err       -> putStrLn ("Failed to compile model: " <> err)
       Right compiled ->
          do  let rendered = PP.render (SG.genModel compiled)
              writeFile output rendered
              putStrLn "compiled!"


genCppRunner :: FilePath -> IO ()
genCppRunner fp =
  do let mdl = testParseModel fp
     let ms = [ M.Measure (M.TraceExpr "n" undefined) (M.TimeLT 120.0)
              , M.Measure (M.Accumulate "m" (Core.Op2 Core.Add (Core.Var "m") (Core.Literal $ Core.Num 1.0)) 1.0) (M.TimeLT 120.0)
              ]

     case Transform.modelAsCore mdl of
       Left err       -> putStrLn ("Failed to compile model: " <> err)
       Right compiled ->
         putStrLn $ PP.render (M.genSimulationRunnerCpp compiled 100.0 ms)