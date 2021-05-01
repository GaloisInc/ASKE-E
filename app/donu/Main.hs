{-# Language BlockArguments, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import Data.Text(Text)
-- import qualified Text.PrettyPrint as PP
import qualified Prettyprinter as PPr
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Aeson as JS
import Control.Monad.IO.Class(liftIO)
import Control.Exception(throwIO, try,SomeException, Exception(..))
import Snap.Core(Snap)
import qualified Snap.Core as Snap
import Snap.Http.Server (quickHttpServe)

import Language.ASKEE
import qualified Language.ASKEE.Core as Core
import           Language.ASKEE.DEQ.Syntax (DiffEqs(..) )
import qualified Language.ASKEE.Core.GSLODE as ODE
import qualified Language.ASKEE.Core.DiffEq as DiffEq
import qualified Language.ASKEE.DataSeries as DS
import qualified Language.ASKEE.Translate as Translate
import qualified Language.ASKEE.Experiment.CodeGen as GenExp
import qualified Language.ASKEE.Experiment.Syntax as Exp
import qualified Language.ASKEE.Experiment.Parser as ExpP
import Schema

import System.IO.Temp ( withSystemTempDirectory, writeTempFile )
import System.Process ( readProcess )
import System.FilePath

import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Language.ASKEE.Experiment.Typechecker as TC
import qualified Language.ASKEE.Experiment.TraverseType as TC

main :: IO ()
main = quickHttpServe
  do let limit = 8 * 1024 * 1024    -- 8 megs
     body <- Snap.readRequestBody limit
     case JS.eitherDecode body of
       Right a ->
         do r <- liftIO $ try $ handleRequest a
            case r of
              Right ok ->
                do Snap.modifyResponse (Snap.setResponseStatus 200 "OK")
                   Snap.writeLBS (JS.encode ok)
              Left err ->
                do Snap.modifyResponse
                              (Snap.setResponseStatus 400 "Bad request")
                   Snap.writeText $ Text.pack $ show (err :: SomeException)
       Left err ->
         do -- Snap.writeText $ Text.pack err
            -- Snap.modifyResponse (Snap.setResponseStatus 400 "Bad request")
            -- showHelp
            Snap.writeLBS helpHTML

--------------------------------------------------------------------------------

newtype ServerError = NotImplemented String
  deriving Show

instance Exception ServerError
notImplemented :: String -> IO a
notImplemented what = throwIO (NotImplemented what)

showHelp :: Snap ()
showHelp = Snap.writeLBS helpHTML

handleRequest :: Input -> IO Output
handleRequest r =
  print r >>
  case r of
    Simulate info ->
      do eqs <- loadDiffEqs (simModelType info)
                            (simModel info)
                            []
                            (simOverwrite info)
         let times = takeWhile (<= simEnd info)
                   $ iterate (+ simStep info)
                   $ simStart info
             res = ODE.simulate eqs Map.empty times

         print eqs
         pure (OutputData res)

    CheckModel cmd ->
      do  checkResult <- checkModel (checkModelModelType cmd) (checkModelModel cmd)
          case checkResult of
            Nothing  -> pure $ OutputResult (SuccessResult ())
            Just err -> pure $ OutputResult (FailureResult (Text.pack err))

    ConvertModel cmd ->
      do  eConverted <- convertModel (convertModelSourceType cmd)
                                     (convertModelModel cmd)
                                     (convertModelDestType cmd)

          pure $ OutputResult (asResult eConverted)

    Fit info ->
      do  eqs <- loadDiffEqs (fitModelType info)
                             (fitModel info)
                             (fitParams info)
                             Map.empty
          print eqs
          rawData <- pack (fitData info)
          dataSeries <- case DS.parseDataSeries rawData of
            Right d -> pure d
            Left err -> throwIO (DS.MalformedDataSeries err)
          let (res, _) = ODE.fitModel eqs dataSeries Map.empty (Map.fromList (zip (fitParams info) (repeat 0)))
          pure (FitResult res)

    GenerateCPP cmd ->
      OutputResult . asResult <$> generateCPP (generateCPPModelType cmd) (generateCPPModel cmd)
    GenerateCPPExperiment GenerateCPPExpCommand{..} ->
      do  -- deqs <- loadDiffEqs generateCPPExpModelType generateCPPExpModel [] Map.empty
          modelCpp' <- generateCPP generateCPPExpModelType generateCPPExpModel
          modelCpp <- case modelCpp' of
            Left err -> notImplemented (Text.unpack err)
            Right res -> pure res
          
          expCpp <- loadExperiment generateCPPExpExperiment

          res <- withSystemTempDirectory "cpp-gen" \dir ->
            do  modelFile <- writeTempFile dir "model" (Text.unpack modelCpp)
                let modelFName = takeFileName modelFile
                let expCpp' = unlines 
                      [ "#include \""++modelFName++"\""
                      , "#include \"ASKEE.hpp\""
                      ] ++ expCpp
                putStrLn expCpp'
                expFile <- writeTempFile dir "exp" expCpp'
                _ <- readProcess "cp" [ "cpp/ASKEE.hpp", dir ] ""
                readProcess "clang++"
                  [ "-Wall"
                  , "-Wextra"
                  , modelFile
                  , expFile
                  ] ""
          notImplemented res


          -- let stateVars = Map.keys deqRates
          --     modelSplice = unlines
          --       [ "model "]
          undefined
    Stratify info ->
      do  (model, params) <- stratifyModel'  (stratModel info)
                                    (stratConnections info)
                                    (stratStates info)
                                    (stratType info)
          pure $ StratificationResult model params

          
  where
    pack :: DataSource -> IO BS8.ByteString
    pack ds = 
      case ds of
        FromFile fp -> BS8.pack <$> readFile fp
        Inline s -> pure $ BS8.pack $ Text.unpack s

loadDiffEqs ::
  ModelType       {- ^ input file format -} ->
  DataSource      {- ^ where to get the data from -} ->
  [Text]          {- ^ parameters, if any -} ->
  Map Text Double {- ^ overwrite these parameters -} ->
  IO DiffEqs
loadDiffEqs mt src ps0 overwrite =
  fmap (DiffEq.applyParams (Core.NumLit <$> overwrite))
  case mt of
    Schema.DiffEqs    -> loadEquations src allParams
    AskeeModel        -> DiffEq.asEquationSystem <$> loadCoreModel src allParams
    ReactionNet       -> notImplemented "Reaction net simulation"
    LatexEqnarray     -> notImplemented "Latex eqnarray simulation"
  where
  allParams = Map.keys overwrite ++ ps0

checkModel :: ModelType -> DataSource -> IO (Maybe String)
checkModel mt src =
  case mt of
    Schema.DiffEqs     -> checkUsing parseEquations src
    Schema.AskeeModel  -> checkUsing parseModel src
    Schema.ReactionNet -> checkUsing parseReactions src
    Schema.LatexEqnarray -> checkUsing parseLatex src
  where
    checkUsing parser ds =
      do  mbMdl <- try (parser ds)
          case mbMdl of
            Left (ParseError err) -> pure $ Just err
            Right _               -> pure Nothing


convertModel :: ModelType -> DataSource -> ModelType -> IO (Either String String)
convertModel inputType source outputType =
  case (inputType, outputType) of
    (_, Schema.DiffEqs) ->
      do  src <- loadString source
          pure $ Translate.asDiffEqConcrete (translateSyntax inputType) src

    (_, Schema.AskeeModel) ->
      do  src <- loadString source
          pure $ Translate.asASKEEConcrete (translateSyntax inputType) src

    (_, _) ->
      pure $ Left "Conversion is not implemented"
  where
    translateSyntax t =
      case t of
        Schema.ReactionNet -> Translate.RNet
        Schema.AskeeModel -> Translate.ASKEE
        Schema.DiffEqs -> Translate.DiffEq
        Schema.LatexEqnarray -> Translate.Latex

generateCPP :: ModelType -> DataSource -> IO (Either Text Text)
generateCPP ty src =
  case ty of
    Schema.AskeeModel ->
      do  mdl <- renderCppModel src
          pure $ Right (Text.pack mdl)
    Schema.ReactionNet ->
      pure $ Left "Rendering reaction networks to C++ is not implemented"
    Schema.DiffEqs ->
      pure $ Left "Rendering diff-eq to C++ is not implemented"
    Schema.LatexEqnarray ->
      pure $ Left "Rendering latex eqnarray to C++ is not implemented"

-------------------------------------------------------------------------

loadExperiment :: DataSource -> IO String
loadExperiment ds =
  do  src <- loadString ds
      let experimentE = ExpP.parse ExpP.declsP "(donu)" (Text.pack src)
      experiment <- case experimentE of
        Left err -> throwIO err
        Right res -> pure res
      experiment' <- runTC (TC.inferDecls experiment)
      let cpp = show $ PPr.vcat $
            [ GenExp.compileMeasure m | Exp.DMeasure m <- experiment' ] ++
            [ GenExp.compileExperiment m | Exp.DExperiment m <- experiment' ] ++
            [ GenExp.compileMain m | Exp.DMain m <- experiment' ]
      pure cpp

data TCError = TCError Text deriving Show

instance Exception TCError

runTC :: TC.TraverseType a => TC.TC a -> IO a
runTC tc =
  case TC.runTC tc of
    Left err -> throwIO (TCError err)
    Right a  -> pure a

-- loadExperiment :: FilePath -> IO [E.Decl]
-- loadExperiment file =
--   do  syn <- P.parseFromFile P.declsP file
--       runTC (TC.inferDecls syn)