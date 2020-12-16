{-# Language OverloadedStrings #-}

module Language.ASKEE ( lexModel
                      , parseModel
                      , loadModel
                      , loadEquations
                      , checkModel
                      , genCppRunner
                      , genCoreModel ) where
  
import           Control.Monad ((>=>))

import qualified Data.Map as Map
import           Data.Text (Text)

import qualified Language.ASKEE.Check as Check
import qualified Language.ASKEE.Core as Core
import           Language.ASKEE.Core.DiffEq (DiffEqs(..))
import           Language.ASKEE.Core.ImportASKEE (modelAsCore)
import qualified Language.ASKEE.GenLexer as AL
import qualified Language.ASKEE.GenParser as AP
import           Language.ASKEE.Lexer (Token, Located)
import qualified Language.ASKEE.Measure as M
import qualified Language.ASKEE.MeasureToCPP as MG
import qualified Language.ASKEE.SimulatorGen as SG
import qualified Language.ASKEE.Syntax as Syntax

-- | Just lex
lexModel :: FilePath -> IO (Either String [Located Token])
lexModel = readFile >=> pure . AL.lexModel

--  Just lex and parse
parseModel :: FilePath -> IO (Either String Syntax.Model)
parseModel file = 
  do  toks <- lexModel file
      pure $ AP.parseModel =<< toks

-- | Don't bother failing gracefully on lex or parse errors
unsafeparseModel :: FilePath -> IO Syntax.Model
unsafeparseModel file =
  do  Right toks <- lexModel file
      let Right model = AP.parseModel toks
      pure model

-- | Just lex, parse, and check
checkModel :: FilePath -> IO ()
checkModel file = 
  do  result <- loadModel file
      either putStrLn (const (putStrLn $ "no issues found in "<>file<>"!")) result

-- | The intended entrypoint for fetching a model
loadModel :: FilePath -> IO (Either String Syntax.Model)
loadModel file =
  do  model <- parseModel file
      pure $ Check.checkModel =<< model

-- | The intended entrypoint for fetching a system of differential equations
-- `params` are used to determine which `let`s to inline, for fitting purposes
loadEquations :: FilePath -> [Text] -> IO (Either String DiffEqs)
loadEquations file params =
  do  toksE <- lexModel file
      let deqs = do toks <- toksE
                    eqs <- AP.parseDEQs toks
                    -- Inline all `let`s that we haven't designated as parameters
                    let lets = foldr Map.delete (deqLet eqs) params
                        inlineLets = Core.substExpr lets
                        eqs' = eqs { deqInitial = Map.map inlineLets (deqInitial eqs)
                                   , deqState   = Map.map inlineLets (deqState eqs) 
                                   , deqParams  = params
                                   }
                    pure eqs'
      pure deqs

genCoreModel :: FilePath -> [Text] -> IO (Either String Core.Model)
genCoreModel file ps =
  do  modelE <- loadModel file
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