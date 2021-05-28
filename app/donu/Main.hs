{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main ( main ) where

import Control.Monad          ( void )
import Control.Monad.IO.Class ( liftIO )
import Control.Exception      ( try, SomeException )

import           Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Aeson as JS

import Language.ASKEE2
import Language.ASKEE.Types
import Language.ASKEE.Storage

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
      do  converted <- 
            convertModel
              (modelDefType convertModelSource) 
              (modelDefSource convertModelSource) 
              convertModelDestType
          pure $ OutputResult $ asResult converted

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