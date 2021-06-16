{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main ( main ) where

import Control.Monad.IO.Class ( liftIO )
import Control.Exception      ( try, SomeException )

import qualified Data.Text as Text
import qualified Data.Aeson as JS

import Language.ASKEE
import Language.ASKEE.Jupyter.GenLexer (lexJupyter)
import Language.ASKEE.Jupyter.GenParser (parseJupyterStmt)
import qualified Language.ASKEE.Jupyter.Interpreter as Jupyter

import Schema

import qualified Snap.Core as Snap
import           Snap.Http.Server ( quickHttpServe )

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
                                (FailureResult _) -> (400, "Error")
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

showHelp :: Snap.Snap ()
showHelp = Snap.writeLBS helpHTML

--------------------------------------------------------------------------------


handleRequest :: Input -> IO Result
handleRequest r =
  print r >>
  case r of
    Simulate SimulateCommand{..} ->
      do  res <-
            simulateModel
              (modelDefType simModel)
              (modelDefSource simModel)
              simStart
              simEnd
              simStep
              simParameterValues
          succeed' res

    CheckModel CheckModelCommand{..} ->
      do  model <- loadModel (modelDefType checkModelModel) (modelDefSource checkModelModel)
          checkResult <- checkModel' (modelDefType checkModelModel) (Text.pack model)        
          case checkResult of
            Nothing  -> succeed' ()
            Just err -> pure (FailureResult (Text.pack err))

    ConvertModel ConvertModelCommand{..} ->
      do  converted <-
            convertModelString
              (modelDefType convertModelSource)
              (modelDefSource convertModelSource)
              convertModelDestType
          pure $ asResult converted

    Fit FitCommand{..} ->
      do  (res, _) <-
            fitModelToData
              (modelDefType fitModel)
              fitData
              fitParams
              mempty
              (modelDefSource fitModel)
          succeed' (FitResult res)

    GenerateCPP (GenerateCPPCommand ModelDef{..}) ->
      do  cpp <- loadCPPFrom modelDefType modelDefSource
          succeed' (Text.pack $ show cpp)

    Stratify StratifyCommand{..} ->
        do  res <- stratifyModel (modelDefType stratModel) (modelDefSource stratModel) stratConnections stratStates stratType
            succeed' res

    ListModels _ -> succeed <$> listAllModelsWithMetadata

    ModelSchemaGraph (ModelSchemaGraphCommand ModelDef{..}) ->
      do  modelSource <- loadCoreFrom modelDefType modelDefSource
          case asSchematicGraph modelSource of
            Nothing -> pure (FailureResult "model cannot be rendered as a schematic")
            Just g -> succeed' g

    UploadModel UploadModelCommand{..} ->
      do  mdef <- storeModel uploadModelType uploadModelName uploadModelSource
          succeed' mdef

    GetModelSource GetModelSourceCommand{..} ->
      do  modelString <- loadModel (modelDefType getModelSource) (modelDefSource getModelSource)
          let result = getModelSource { modelDefSource = Inline (Text.pack modelString) }
          succeed' result

    DescribeModelInterface (DescribeModelInterfaceCommand ModelDef{..}) ->
      do  model <- loadESLFrom modelDefType modelDefSource
          let res = describeModelInterface model
          succeed' res

    ExecuteJupyterCode (ExecuteJupyterCodeCommand code) ->
      pure $
      case lexJupyter (Text.unpack code) >>= parseJupyterStmt of
        Left err   -> FailureResult $ Text.pack err
        Right stmt -> succeed $ Jupyter.interpretStmt stmt
  where
    succeed :: (JS.ToJSON a, Show a) => a -> Result
    succeed = SuccessResult

    succeed' :: (JS.ToJSON a, Show a) => a -> IO Result
    succeed' = pure . succeed
