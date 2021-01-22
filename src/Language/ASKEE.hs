{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}

module Language.ASKEE
  ( lexModel
  , parseModel
  , loadModel
  , loadCoreModel
  , lexEquations
  , parseEquations
  , loadEquations
  , lexReactions
  , parseReactions
  , lexLatex
  , parseLatex
  , loadLatex
  , loadReactions
  , genCppRunner
  , DataSource(..)
  ) where

import           Control.Exception(Exception(..),throwIO)

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Language.ASKEE.Check as Check
import           Language.ASKEE.Convert
import qualified Language.ASKEE.Core as Core
import qualified Language.ASKEE.DEQ.GenLexer as DL
import qualified Language.ASKEE.DEQ.GenParser as DP
import           Language.ASKEE.DEQ.Syntax ( DiffEqs(..) )
import           Language.ASKEE.DEQ.Print ( ppDiffEqs )
import           Language.ASKEE.Core.ImportASKEE (modelAsCore)
import qualified Language.ASKEE.GenLexer as AL
import qualified Language.ASKEE.GenParser as AP
import qualified Language.ASKEE.Latex.GenLexer as LL
import qualified Language.ASKEE.Latex.GenParser as LP
import           Language.ASKEE.Lexer (Token, Located)
import qualified Language.ASKEE.Measure as M
import qualified Language.ASKEE.MeasureToCPP as MG
import qualified Language.ASKEE.RNet.GenLexer as RL
import qualified Language.ASKEE.RNet.GenParser as RP
import           Language.ASKEE.RNet.Syntax ( ReactionNet(..) )
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

-- | Lex a set of differential equations, throwing `ParseError` on error
lexEquations :: DataSource -> IO [Located DL.Token]
lexEquations file =
  do  txt <- loadString file
      case DL.lexDEQs txt of
        Right toks -> pure toks
        Left err -> throwIO (ParseError err)

-- | Lex and parse a set of differential equations, throwing `ParseError` on error
parseEquations :: DataSource -> IO DiffEqs
parseEquations file =
  do  toks <- lexEquations file
      case DP.parseDEQs toks of
        Right deqs -> pure deqs
        Left err -> throwIO (ParseError err)

-- | The intended entrypoint for fetching a system of differential equations
-- `params` are the names of `lets` that should be treated as parameters
-- (i.e., their definitions are ignored)
loadEquations :: DataSource -> [Text] -> IO DiffEqs
loadEquations file params =
  do  toks <- lexEquations file
      eqs <- case DP.parseDEQs toks of
               Left err -> throwIO (ParseError err)
               Right a  -> pure a { deqParams = params }
      let lets = foldr Map.delete (deqLets eqs) params
          inlineLets = Core.substExpr lets
      pure (Core.mapExprs inlineLets eqs)

-- | Lex a set of reactions, throwing `ParseError` on error
lexReactions :: DataSource -> IO [Located RL.Token]
lexReactions file = 
  do  txt <- loadString file
      case RL.lexRNet txt of
        Right toks -> pure toks
        Left err -> throwIO (ParseError $ "lexReactions: "<>err) 

-- | Lex and parse a set of reactions, throwing `ParseError` on error
parseReactions :: DataSource -> IO ReactionNet
parseReactions file =
  do  toks <- lexReactions file
      case RP.parseRNet toks of
        Right deqs -> pure deqs
        Left err -> throwIO (ParseError $ "parseReactions: "<>err)

loadReactions :: DataSource -> IO ReactionNet
loadReactions = parseReactions

lexLatex :: DataSource -> IO [Located LL.Token] 
lexLatex file =
  do  txt <- loadString file
      case LL.lexLatex txt of
        Right toks -> pure toks
        Left err -> throwIO (ParseError $ "lexLatex: "<>err)

parseLatex :: DataSource -> IO DiffEqs
parseLatex file =
  do  toks <- lexLatex file
      case LP.parseLatex toks of
        Right deqs -> pure deqs
        Left err -> throwIO (ParseError $ "parseLatex: "<>err)

loadLatex :: DataSource -> IO DiffEqs 
loadLatex = parseLatex

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

askeeStringToDiffEqAST :: String -> Either String DiffEqs
askeeStringToDiffEqAST = $(converter (tagOf @Syntax.Model Concrete) (tagOf @DiffEqs Abstract))

askeeStringToDiffEqString :: String -> Either String String
askeeStringToDiffEqString = $(converter (tagOf @Syntax.Model Concrete) (tagOf @DiffEqs Concrete))

-- $allConverters

diffEqStringToLatexString :: String -> Either String String
diffEqStringToLatexString = $(converter (tagOf @DiffEqs Concrete) (tagOf @Latex Concrete))