{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}

module Language.ASKEE where

import           Control.Exception(Exception(..),throwIO)

import qualified Data.Map as Map
import           Data.Text (Text, unpack)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson ( encode, decode, object, (.=) )

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
import qualified Language.ASKEE.ModelStratify.Syntax as MS
import Data.Word (Word8)


import System.Directory ( withCurrentDirectory, makeAbsolute, removeFile )
import System.Process ( readProcess )
import System.IO.Temp ( withSystemTempFile, writeSystemTempFile )
import System.Random ( randomIO )
import Language.ASKEE.ModelStratify.Topology (modelAsTopology, topologyAsParameterizedModel)
import System.IO (hPutStr, hSeek, SeekMode (AbsoluteSeek))
import Data.List (intercalate)

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
      pure eqs --(Core.mapExprs inlineLets eqs)

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

data StratificationType = Demographic | Spatial
  deriving Show

stratifyModel :: DataSource -> DataSource -> Maybe DataSource -> StratificationType -> IO (Syntax.Model, [Text])
stratifyModel mod connections states strat =
  do  topology <- modelAsTopology <$> loadModel mod
      onDisk (B.unpack $ encode @MS.Net topology) $ \topFile ->
        asFile connections $ \connFile ->
          asFileM states $ \stFileM ->
            do  
                result <- withCurrentDirectory "ASKE-E-Simulation-WG/AlgebraicPetri-Stratification" $
                  let params =  [ "top="<>topFile
                                , "conn="<>connFile
                                , "type="<>case strat of Demographic -> "dem"; Spatial -> "spat"
                                ] ++ case stFileM of {Nothing -> []; Just stFile -> ["states="<>stFile]}
                  in  readProcess "curl"  [ "-X", "GET"
                                          , "localhost:8001/?"++intercalate "&" params
                                          ] ""
                topology' <- case decode @MS.Net (B.pack result) of
                  Just t -> pure t
                  Nothing -> error $ "failed to parse JSON of returned topology "++result
                pure $ topologyAsParameterizedModel topology'

  where
    onDisk :: String -> (FilePath -> IO b) -> IO b
    onDisk thing = onDisk' thing id

    onDisk' :: String -> (FilePath -> b) -> (b -> IO c) -> IO c
    onDisk' thing translatePath action =
      do  file <- writeSystemTempFile "foo.whatever" thing
          result <- action $ translatePath file
          removeFile file
          pure result

    asFile :: DataSource -> (FilePath -> IO a) -> IO a
    asFile d action =
      case d of
        Inline t -> onDisk (unpack t) action
        FromFile f -> makeAbsolute f >>= action

    asFileM :: Maybe DataSource -> (Maybe FilePath -> IO a) -> IO a
    asFileM dm action =
      case dm of
        Nothing -> action Nothing
        Just d ->
          case d of
            FromFile f -> makeAbsolute f >>= action . Just
            Inline t -> onDisk' (unpack t) Just action

    mkJSON :: FilePath -> FilePath -> Maybe FilePath -> String
    mkJSON topFile connFile stFileM =
      B.unpack $ encode $ object [ "top" .= topFile
                                 , "conn" .= connFile
                                 , "states" .= stFileM
                                 , "type" .= (case strat of { Demographic -> "dem"; Spatial -> "spat" } :: String)
                                 ]
              

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
renderCppModel :: DataSource -> IO String
renderCppModel file =
  do  compiled <- loadCoreModel file []
      pure $ show (SG.genModel compiled)

