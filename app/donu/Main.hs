{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main ( main ) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Aeson as JS
import Control.Monad.IO.Class(liftIO)
import Control.Exception( try, SomeException )
import qualified Snap.Core as Snap
import Snap.Http.Server (quickHttpServe)

import Language.ASKEE2
import Language.ASKEE.Types
import Schema
import Language.ASKEE.Storage

import Control.Monad (void)

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

showHelp :: Snap.Snap ()
showHelp = Snap.writeLBS helpHTML

--------------------------------------------------------------------------------


handleRequest :: Input -> IO Output
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
          pure (OutputData res)

    CheckModel CheckModelCommand{..} ->
      do  checkResult <- 
            checkModel 
              (modelDefType checkModelModel)
              (modelDefSource checkModelModel)
          case checkResult of
            Nothing  -> pure $ OutputResult (SuccessResult ())
            Just err -> pure $ OutputResult (FailureResult (Text.pack err))

    ConvertModel ConvertModelCommand{..} ->
      do  eConverted <- 
            convertConcrete 
              (modelDefType convertModelSource)
              (modelDefSource convertModelSource)
              convertModelDestType
          case eConverted of
            Right converted ->
              pure $ OutputResult (SuccessResult $ ModelDef (Inline $ Text.pack converted) convertModelDestType)
            Left err ->
              pure $ OutputResult (FailureResult $ Text.pack err)

    Fit FitCommand{..} ->
      do  res <- 
            fitModelToData
              (modelDefType fitModel)
              fitData
              fitParams
              (modelDefSource fitModel)
          pure (FitResult res)

    GenerateCPP (GenerateCPPCommand ModelDef{..}) ->
      do  cpp <- loadCPPFrom modelDefType modelDefSource
          pure $ OutputResult $ asResult (Right cpp :: Either Text Text)

    Stratify StratifyCommand{..} ->
        do  res <- stratifyModel stratModel stratConnections stratStates stratType
            pure $ StratificationResult res

    ListModels _ -> OutputModelList <$> listAllModels

    ModelSchemaGraph (ModelSchemaGraphCommand ModelDef{..}) ->
      do  modelSource <- loadCoreFrom modelDefType modelDefSource
          case asSchematicGraph modelSource of
            Nothing -> pure $ OutputResult (FailureResult "model cannot be rendered as a schematic")
            Just g -> pure $ OutputResult (SuccessResult g)

    UploadModel UploadModelCommand{..} ->
      do  let check m = void $ checkModel uploadModelType (Inline m)
          loc <- storeModel uploadModelName uploadModelType check uploadModelSource
          let mdef = ModelDef (FromFile loc) uploadModelType
          pure $ OutputResult (SuccessResult mdef)

    GetModelSource GetModelSourceCommand{..} ->
      do  modelString <- loadModel (modelDefType getModelSource) (modelDefSource getModelSource)
          let result = getModelSource { modelDefSource = Inline (Text.pack modelString) }
          pure $ OutputResult (SuccessResult result)

    DescribeModelInterface (DescribeModelInterfaceCommand ModelDef{..}) -> 
      do  res <- describeModelInterface modelDefType modelDefSource
          pure $ OutputResult (SuccessResult res)


-- checkModel :: ModelType -> DataSource -> IO (Maybe String)
-- checkModel mt src =
--   do  res <- try (checkModel' mt src)
--       case res of
--         Left err -> pure $ Just (show (err :: SomeException))
--         Right _ -> pure Nothing

-- -- Just throw an exception on failure
-- checkModel' :: ModelType -> DataSource -> IO ()
-- checkModel' format model =
--   case format of
--     DEQ _    -> void $ loadDiffEqs Map.empty [] model
--     ESL _    -> void $ loadESL model
--     RNET _   -> void $ loadReactions model
--     LATEX _  -> void $ loadLatex model
--     ESLMETA _ -> void $ loadMetaESL model
--     GROMET _ -> void $ loadGromet model
--     TOPO _ -> notImplemented "topology checking"


-- generateCPP :: ModelType -> DataSource -> IO (Either Text Text)
-- generateCPP format source =
--   do  coreModel <- loadCoreFrom format source
--       let mdl = renderCppModel coreModel
--       pure $ Right (Text.pack mdl)

-------------------------------------------------------------------------
