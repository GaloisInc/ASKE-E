{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main(main) where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Aeson as JS
import Data.Aeson((.=))
import Control.Monad.IO.Class(liftIO)
import Control.Exception(throwIO, try,SomeException, Exception(..))
import qualified Snap.Core as Snap
import Snap.Http.Server (quickHttpServe)

import Language.ASKEE
import           Language.ASKEE.Convert ( converter )
import qualified Language.ASKEE.Core as Core
import           Language.ASKEE.DEQ.Syntax (DiffEqs(..) )
import qualified Language.ASKEE.Core.GSLODE as ODE
import qualified Language.ASKEE.Core.DiffEq as DiffEq
import qualified Language.ASKEE.DataSeries as DS
import qualified Language.ASKEE.Core.Visualization as CoreViz
import qualified Language.ASKEE.Metadata as Meta
import qualified Language.ASKEE.Syntax as Easel
import           Language.ASKEE.Types
import Schema
import ModelStorage

import qualified Data.ByteString.Lazy.Char8 as BS8
import Control.Monad (void)
import System.Process ( readProcessWithExitCode )
import System.Exit ( ExitCode(..) )

main :: IO ()
main = 
  do  initStorage
      quickHttpServe $
        do  Snap.route [ ("/help", showHelp)
                       , ("/", endpoint)
                       ]
  where
    endpoint =
     do let limit = 8 * 1024 * 1024    -- 8 megs
        Snap.modifyResponse (Snap.setHeader "Access-Control-Allow-Origin" "*")
        body <- Snap.readRequestBody limit
        case JS.eitherDecode body of
          Right a ->
            do  r <- liftIO $ try $ handleRequest a
                case r of
                  Right out ->
                    do  let (code, msg) =
                              case out of
                                OutputResult (FailureResult _) -> (400, "Error")
                                _ -> (200, "OK")
                        Snap.modifyResponse (Snap.setResponseStatus code msg)
                        Snap.writeLBS (JS.encode out)
                  Left err ->
                    do  Snap.modifyResponse
                                  (Snap.setResponseStatus 400 "Bad request")
                        Snap.writeText $ Text.pack $ show (err :: SomeException)
          Left err ->
            do  Snap.writeText $ Text.pack err
                Snap.modifyResponse (Snap.setResponseStatus 400 "Bad request")
                -- showHelp

--------------------------------------------------------------------------------

newtype ServerError = NotImplemented String
  deriving Show

instance Exception ServerError
notImplemented :: String -> IO a
notImplemented what = throwIO (NotImplemented what)

showHelp :: Snap.Snap ()
showHelp = Snap.writeLBS helpHTML

handleRequest :: Input -> IO Output
handleRequest r =
  print r >>
  case r of
    Simulate info ->
      do eqs <- loadDiffEqs (modelDefType $ simModel info)
                            (modelDefSource $ simModel info)
                            []
                            (simParameterValues info)
         let times = takeWhile (<= simEnd info)
                   $ iterate (+ simStep info)
                   $ simStart info
             res = ODE.simulate eqs Map.empty times

         print eqs
         pure (OutputData res)

    CheckModel cmd ->
      do  checkResult <- checkModel (modelDefType $ checkModelModel cmd)
                                    (modelDefSource $ checkModelModel cmd)
          case checkResult of
            Nothing  -> pure $ OutputResult (SuccessResult ())
            Just err -> pure $ OutputResult (FailureResult (Text.pack err))

    ConvertModel cmd ->
      do  eConverted <- convertModel (modelDefType $ convertModelSource cmd)
                                     (modelDefSource $ convertModelSource cmd)
                                     (convertModelDestType cmd)

          case eConverted of
            Right converted ->
              pure $ OutputResult (SuccessResult $ ModelDef (Inline $ Text.pack converted) (convertModelDestType cmd))
            Left err ->
              pure $ OutputResult (FailureResult $ Text.pack err)

    Fit info ->
      do  eqs <- loadDiffEqs (modelDefType $ fitModel info)
                             (modelDefSource $ fitModel info)
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
      OutputResult . asResult <$> generateCPP (modelDefType $ generateCPPModel cmd)
                                              (modelDefSource $ generateCPPModel cmd)
    Stratify info ->
      do  modelInfo <- stratifyModel  (stratModel info)
                                      (stratConnections info)
                                      (stratStates info)
                                      (stratType info)
          pure $ StratificationResult modelInfo

    ListModels _ -> OutputModelList <$> listAllModels

    ModelSchemaGraph cmd ->
      case modelDefType $ modelSchemaGraphModel cmd of
        ESL _ ->
          do  modelSource <- loadCoreModel' (modelDefSource $ modelSchemaGraphModel cmd)
              case CoreViz.asSchematicGraph modelSource of
                Nothing -> pure $ OutputResult (FailureResult "model cannot be rendered as a schematic")
                Just g -> pure $ OutputResult (SuccessResult g)

        _ -> pure $ OutputResult (FailureResult "model type not supported")

    UploadModel UploadModelCommand{..} ->
      do  res <- try
            do  checkModel' uploadModelType (Inline uploadModelSource)
                loc <- storeModel uploadModelName uploadModelType uploadModelSource
                let mdef = ModelDef (FromFile loc) uploadModelType
                pure $ OutputResult (SuccessResult mdef)
          case res of
            Left err -> pure $ OutputResult $ FailureResult $ Text.pack $ show (err :: SomeException)
            Right ok -> pure ok

    GetModelSource cmd ->
      do  models <- listAllModels
          if getModelSource cmd `elem` models then
            do  d <- loadString (modelDefSource $ getModelSource cmd)
                let result = (getModelSource cmd) { modelDefSource = Inline (Text.pack d) }
                pure $ OutputResult (SuccessResult result)
          else
            pure $ OutputResult (FailureResult "model does not exist")

    DescribeModelInterface cmd ->
      case modelDefType (describeModelInterfaceSource cmd) of
        ESL _ ->
          do  mdl <- parseMetaModel . modelDefSource $ describeModelInterfaceSource cmd

              let stateVars =
                    [(n, Meta.metaMap md) | md <- Easel.modelMetaDecls mdl
                                          , (Easel.State n _) <- [Meta.metaValue md]
                                          ]
                  params =
                    [(n, d, Meta.metaMap md) | md <- Easel.modelMetaDecls mdl
                                             , (Easel.Parameter n d) <- [Meta.metaValue md]
                                             ]

                  descParam (n, d, mp) =
                    JS.object [ "name" .= n
                              , "defaultValue" .= d
                              , "metadata" .= mp
                              ]
                  descState (n, mp) =
                    JS.object [ "name" .= n
                              , "metadata" .= mp
                              ]
                  desc =
                    JS.object [ "parameters" .= (descParam <$> params)
                              , "stateVars"  .= (descState <$> stateVars)
                              ]

              pure $ OutputResult (SuccessResult desc)

        _ -> pure $ OutputResult (FailureResult "model type not supported")
          


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
    DEQ _    -> loadEquations src allParams
    ESL _        -> DiffEq.asEquationSystem <$> loadCoreModel src overwrite
    RNET _       -> notImplemented "Reaction net simulation"
    LATEX _     -> notImplemented "Latex eqnarray simulation"
    GROMET _            -> notImplemented "Gromet simulation"
    TOPO _ -> notImplemented "Topology simulation"
  where
  allParams = Map.keys overwrite ++ ps0

checkModel :: ModelType -> DataSource -> IO (Maybe String)
checkModel mt src =
  do  res <- try (checkModel' mt src)
      case res of
        Left err -> pure $ Just (show (err :: SomeException))
        Right _ -> pure Nothing

-- Just throw an exception on failure
checkModel' :: ModelType -> DataSource -> IO ()
checkModel' format model =
  case format of
    DEQ _    -> void $ parseEquations model
    ESL _    -> void $ parseModel model
    RNET _   -> void $ parseReactions model
    LATEX _  -> void $ parseLatex model
    GROMET _ -> 
      do  m <- loadString model
          (code, _out, _err) <- readProcessWithExitCode "jq" [] m
          case code of
            ExitSuccess -> pure ()
            ExitFailure _ -> throwIO $ ParseError "invalid gromet"
    TOPO _ -> notImplemented "topology checking"

convertModel :: ModelType -> DataSource -> ModelType -> IO (Either String String)
convertModel inputType source outputType =
  do  model <- loadString source
      conv <- 
        case inputType of
          ESL Concrete ->
            case outputType of
              ESL Concrete    -> pure $(converter (ESL Concrete) (ESL Concrete))
              DEQ Concrete    -> pure $(converter (ESL Concrete) (DEQ Concrete))
              RNET Concrete   -> pure $(converter (ESL Concrete) (LATEX Concrete))
              LATEX Concrete  -> pure $(converter (ESL Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (ESL Concrete) (GROMET Concrete))
              TOPO Concrete   -> pure $(converter (ESL Concrete) (TOPO Concrete))
              _ -> notImplemented (show outputType)
          DEQ Concrete ->
            case outputType of
              -- ESL Concrete    -> pure $(converter (DEQ Concrete) (ESL Concrete))
              DEQ Concrete    -> pure $(converter (DEQ Concrete) (DEQ Concrete))
              -- RNET Concrete   -> pure $(converter (DEQ Concrete) (RNET Concrete))
              LATEX Concrete  -> pure $(converter (DEQ Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (DEQ Concrete) (GROMET Concrete))
              -- TOPO Concrete   -> pure $(converter (DEQ Concrete) (TOPO Concrete))
              _ -> notImplemented (show outputType)
          RNET Concrete ->
            case outputType of
              ESL Concrete    -> pure $(converter (RNET Concrete) (ESL Concrete))
              DEQ Concrete    -> pure $(converter (RNET Concrete) (DEQ Concrete))
              RNET Concrete   -> pure $(converter (RNET Concrete) (RNET Concrete))
              LATEX Concrete  -> pure $(converter (RNET Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (RNET Concrete) (GROMET Concrete))
              TOPO Concrete   -> pure $(converter (RNET Concrete) (TOPO Concrete))
              _ -> notImplemented (show outputType)
          LATEX Concrete ->
            case outputType of
              -- ESL Concrete    -> pure $(converter (LATEX Concrete) (ESL Concrete))
              DEQ Concrete    -> pure $(converter (LATEX Concrete) (DEQ Concrete))
              RNET Concrete   -> pure $(converter (LATEX Concrete) (LATEX Concrete))
              LATEX Concrete  -> pure $(converter (LATEX Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (LATEX Concrete) (GROMET Concrete))
              -- TOPO Concrete   -> pure $(converter (LATEX Concrete) (TOPO Concrete))
              _ -> notImplemented (show outputType)
          GROMET Concrete ->
            case outputType of
              -- ESL Concrete    -> pure $(converter (GROMET Concrete) (ESL Concrete))
              -- DEQ Concrete    -> pure $(converter (GROMET Concrete) (DEQ Concrete))
              -- RNET Concrete   -> pure $(converter (GROMET Concrete) (LATEX Concrete))
              -- LATEX Concrete  -> pure $(converter (GROMET Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (GROMET Concrete) (GROMET Concrete))
              -- TOPO Concrete   -> pure $(converter (GROMET Concrete) (TOPO Concrete))
              _ -> notImplemented (show outputType)
          TOPO Concrete ->
            case outputType of
              ESL Concrete    -> pure $(converter (TOPO Concrete) (ESL Concrete))
              DEQ Concrete    -> pure $(converter (TOPO Concrete) (DEQ Concrete))
              RNET Concrete   -> pure $(converter (TOPO Concrete) (LATEX Concrete))
              LATEX Concrete  -> pure $(converter (TOPO Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (TOPO Concrete) (GROMET Concrete))
              TOPO Concrete   -> pure $(converter (TOPO Concrete) (TOPO Concrete))
              _ -> notImplemented (show outputType)
          _ -> notImplemented (show inputType)
          -- would be really really great to avoid this, but c'est la vie
      pure $ conv model

generateCPP :: ModelType -> DataSource -> IO (Either Text Text)
generateCPP ty src =
  case ty of
    ESL _ ->
      do  mdl <- renderCppModel src
          pure $ Right (Text.pack mdl)
    RNET _ ->
      pure $ Left "Rendering reaction networks to C++ is not implemented"
    DEQ _ ->
      pure $ Left "Rendering diff-eq to C++ is not implemented"
    LATEX _ ->
      pure $ Left "Rendering latex eqnarray to C++ is not implemented"
    GROMET _ ->
      pure $ Left "Rendering gromet to C++ is not implemented"
    TOPO _ ->
      pure $ Left "Rendering topology to C++ is not implemented"

-------------------------------------------------------------------------
