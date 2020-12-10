{-# Language OverloadedStrings #-}

module Language.ASKEE ( lexFile
                      , parseFile
                      , checkFile
                      , checkPrint
                      , genCppRunner
                      , genCoreModel ) where
  
import           Control.Monad ((>=>))

import           Data.Text(Text)

import qualified Language.ASKEE.Check as Check
import qualified Language.ASKEE.Core as Core
import           Language.ASKEE.Core.ImportASKEE (modelAsCore)
import qualified Language.ASKEE.GenLexer as AL
import qualified Language.ASKEE.GenParser as AP
import           Language.ASKEE.Lexer (Token, Located)
import qualified Language.ASKEE.Measure as M
import qualified Language.ASKEE.MeasureToCPP as MG
import qualified Language.ASKEE.SimulatorGen as SG
import qualified Language.ASKEE.Syntax as Syntax

lexFile :: FilePath -> IO (Either String [Located Token])
lexFile = readFile >=> pure . AL.lexModel

parseFile :: FilePath -> IO (Either String Syntax.Model)
parseFile file = 
  do  toks <- lexFile file
      pure $ AP.parse =<< toks

checkFile :: FilePath -> IO (Either String Syntax.Model)
checkFile file =
  do  model <- parseFile file
      pure $ Check.checkModel =<< model

checkPrint :: FilePath -> IO ()
checkPrint file = 
  do  result <- checkFile file
      either putStrLn (const (putStrLn "no issues found!")) result

-- | Don't bother failing gracefully on lex or parse errors
unsafeParseFile :: FilePath -> IO Syntax.Model
unsafeParseFile file =
  do  Right toks <- lexFile file
      let Right model = AP.parse toks
      pure model

genCoreModel :: FilePath -> [Text] -> IO (Either String Core.Model)
genCoreModel file ps =
  do  modelE <- parseFile file
      pure $ modelAsCore ps =<< modelE

genCppRunner :: FilePath -> IO ()
genCppRunner fp =
  do  compiledE <- genCoreModel fp []
      compiled <- either fail pure compiledE
      print $ MG.genSimulationRunnerCpp compiled 100.0 m4
  where
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

dumpCppModel :: FilePath -> FilePath -> IO ()
dumpCppModel file output =
  do  compiledE <- genCoreModel file []
      compiled <- either fail pure compiledE
      let rendered = show (SG.genModel compiled)
      writeFile output rendered
      putStrLn "compiled!"