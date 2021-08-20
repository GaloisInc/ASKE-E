{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main ( main ) where

import Control.Monad.State

import           Control.Exception.Lifted (try, SomeException)
import           Control.Applicative ((<|>))
import qualified Data.Map as Map

import qualified Data.Text as Text
import qualified Data.Aeson as JS

import qualified Snap
import qualified Snap.Util.GZip as SnapGzip

import           Language.ASKEE

import           Schema
import           ExposureSession

-------------------------------------------------------------------------------

runDonu :: IO ()
runDonu =
  Snap.quickHttpServe $
    Snap.route [ ("/help", showHelp)
               , ("/:exposure", exposureHandler)
               , ("/", endpoint)
               ]
  where
    endpoint =
     do let limit = 8 * 1024 * 1024    -- 8 megs
        Snap.modifyResponse (Snap.setHeader "Access-Control-Allow-Origin" "*")
        body <- Snap.readRequestBody limit
        case JS.eitherDecode body of
          Right a -> SnapGzip.withCompression (runHandler a) <|> errorWith 400 "Bad request"
          Left err -> errorWith 400 ("Didn't recognize request: "<>err)

    runHandler r =
      do result <- try $ handleRequest r
         case result of
          Right out ->
            do  let (code, msg) =
                        case out of
                          (FailureResult _) -> (400, "Error")
                          _ -> (200, "OK")
                Snap.modifyResponse (Snap.setHeader "Content-Type" "application/json")
                Snap.modifyResponse (Snap.setResponseStatus code msg)
                Snap.writeLBS (JS.encode out)
          Left ex -> errorWith 500 (show (ex :: SomeException))



errorWith :: Int -> String -> Snap.Snap ()
errorWith code err =
  do  Snap.modifyResponse (Snap.setResponseStatus code "Error")
      let response = JS.object
            [ "status" JS..= ("error" :: String)
            , "error" JS..= err
            ]
      Snap.writeLBS (JS.encode response)

main :: IO ()
main =
  do initStorage
     initDataStorage
     initComparisonStorage
     runDonu

showHelp :: Snap.Snap ()
showHelp = Snap.writeLBS helpHTML

--------------------------------------------------------------------------------
exposureHandler :: Snap.Snap ()
exposureHandler = exposureServer

handleRequest :: Input -> Snap.Snap Result
handleRequest r =
  liftIO (print r) >>
  case r of
    Simulate SimulateCommand{..} ->
      do  let params =
                case simDomainParam of
                  Nothing -> simParameterValues
                  Just domainParamName -> Map.insert domainParamName 0 simParameterValues

          ifaceErrs <- liftIO $ checkSimArgs (modelDefType simModel) (modelDefSource simModel) params simOutputs
          case ifaceErrs of
            [] ->
              do  res <- liftIO $ simulateModel
                    simType
                    (modelDefType simModel)
                    (modelDefSource simModel)
                    simStart
                    simEnd
                    simStep
                    simParameterValues
                    simOutputs
                    simSeed
                    simDomainParam
                    1
                  succeed' res
            errs -> pure (FailureResult (Text.unlines errs))

    CheckModel CheckModelCommand{..} ->
      do  model <- liftIO $ loadModelText (modelDefType checkModelModel) (modelDefSource checkModelModel)
          checkResult <- liftIO $ checkModel' (modelDefType checkModelModel) model
          case checkResult of
            Nothing  -> succeed' ()
            Just err -> pure (FailureResult (Text.pack err))

    ConvertModel ConvertModelCommand{..} ->
      do  converted <-
            liftIO $
            convertModelString
              (modelDefType convertModelSource)
              (modelDefSource convertModelSource)
              convertModelDestType
          pure $ asResult converted

    Fit FitCommand{..} ->
      do ifaceErrs <- liftIO $ checkFitArgs (modelDefType fitModel) (modelDefSource fitModel) fitParams
         case ifaceErrs of
           [] ->
             do (res, _) <-
                  liftIO $
                  fitModelToData
                    (modelDefType fitModel)
                    fitData
                    fitParams
                    mempty
                    (modelDefSource fitModel)
                succeed' (FitResult res)
           errs -> pure (FailureResult (Text.unlines errs))

    GenerateCPP (GenerateCPPCommand ModelDef{..}) ->
      do  cpp <- liftIO $ loadCPPFrom modelDefType modelDefSource
          succeed' (Text.pack $ show cpp)

    Stratify StratifyCommand{..} ->
        do  res <- liftIO $ stratifyModel (modelDefType stratModel) (modelDefSource stratModel) stratConnections stratStates stratType
            succeed' res

    ListModels _ -> succeed <$> liftIO listAllModelsWithMetadata

    ModelSchemaGraph (ModelSchemaGraphCommand ModelDef{..}) ->
      do  modelSource <- liftIO $ loadCoreFrom modelDefType modelDefSource
          case asSchematicGraph modelSource of
            Nothing -> pure (FailureResult "model cannot be rendered as a schematic")
            Just g -> succeed' g

    UploadModel UploadModelCommand{..} ->
      do  mdef <- liftIO $ storeModel uploadModelType uploadModelName uploadModelSource
          succeed' mdef

    GetModelSource GetModelSourceCommand{..} ->
      do  modelString <- liftIO $ loadModelText (modelDefType getModelSource) (modelDefSource getModelSource)
          let result = getModelSource { modelDefSource = Inline modelString }
          succeed' result

    DescribeModelInterface (DescribeModelInterfaceCommand ModelDef{..}) ->
      do  model <- liftIO $ loadModel modelDefType modelDefSource
          let res = describeModelInterface model
          succeed' res

    QueryModels QueryModelsCommand {..} ->
      succeed <$> liftIO (queryModels queryText)

    ListDataSets ListDataSetsCommand ->
      succeed . fmap dataSetDescToJSON <$> liftIO listDataSets

    GetDataSet GetDataSetCommand {..} ->
      succeed <$> liftIO (loadDataSet getDataSetCommandDataSource)

    FitMeasures FitMeasuresCommand { .. } ->
      succeed <$> liftIO (fitModelToMeasureData (modelDefType fitMeasureModel)
                                                (modelDefSource fitMeasureModel)
                                                fitMeasureParams
                                                fitMeasureData)
    
    CompareModels ServeComparisonCommand { .. } ->
      succeed <$> liftIO (compareModels (modelDefType compModelSource) 
                                        (modelDefSource compModelSource)
                                        (modelDefType compModelTarget) 
                                        (modelDefSource compModelTarget))
                                        
    MeasureError (MeasureErrorCommand req) ->
      succeed' (computeError req)

  where
    succeed :: (JS.ToJSON a, Show a) => a -> Result
    succeed = SuccessResult

    succeed' :: (JS.ToJSON a, Show a) => a -> Snap.Snap Result
    succeed' = pure . succeed
