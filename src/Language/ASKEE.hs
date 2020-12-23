{-# Language OverloadedStrings #-}

module Language.ASKEE
  ( lexModel
  , parseModel
  , loadModel
  , loadCoreModel
  , loadEquations
  , genCppRunner
  , DataSource(..)
  ) where

import           Control.Exception(Exception(..),throwIO)

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text

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

data ParseError      = ParseError String deriving Show
data ValidationError = ValidationError String deriving Show

instance Exception ParseError
instance Exception ValidationError

data DataSource =
    FromFile FilePath
  | Inline Text
    deriving Show

loadString :: DataSource -> IO String
loadString source =
  case source of
    FromFile file -> readFile file
    Inline txt    -> pure (Text.unpack txt)

-- | Just lex
lexModel :: DataSource -> IO [Located Token]
lexModel file =
  do txt <- loadString file
     case AL.lexModel txt of
       Left err -> throwIO (ParseError err)
       Right a  -> pure a

--  | Just lex and parse, throws `ParseErrror`
parseModel :: DataSource -> IO Syntax.Model
parseModel file =
  do  toks <- lexModel file
      case AP.parseModel toks of
        Left err -> throwIO (ParseError err)
        Right a  -> pure a

-- | Lex, parse, and validate a model.
loadModel :: DataSource -> IO Syntax.Model
loadModel file =
  do  m <- parseModel file
      case Check.checkModel m of
        Left err -> throwIO (ValidationError err)
        Right m1 -> pure m1

-- | Load a model and translate it to core.
loadCoreModel :: DataSource -> [Text] -> IO Core.Model
loadCoreModel file ps =
  do  model <- loadModel file
      case modelAsCore ps model of
        Left err -> throwIO (ValidationError err)
        Right a  -> pure a


-- | The intended entrypoint for fetching a system of differential equations
-- `params` are the names of `lets` that should be treated as parameters
-- (i.e., their definitions are ignored)
loadEquations :: DataSource -> [Text] -> IO DiffEqs
loadEquations file params =
  do  toks <- lexModel file
      eqs <- case AP.parseDEQs toks of
               Left err -> throwIO (ParseError err)
               Right a  -> pure a
      let lets = foldr Map.delete (deqLet eqs) params
          inlineLets = Core.substExpr lets
      pure (Core.mapExprs inlineLets eqs)

genCppRunner :: DataSource -> IO ()
genCppRunner fp =
  do compiled <- loadCoreModel fp []
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

dumpCppModel :: DataSource -> FilePath -> IO ()
dumpCppModel file output =
  do  compiled <- loadCoreModel file []
      let rendered = show (SG.genModel compiled)
      writeFile output rendered
      putStrLn "compiled!"
