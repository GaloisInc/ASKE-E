{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main ( main ) where

import           Control.Applicative ((<|>))
import           Control.Lens.TH
import           Control.Lens (view)
import           Control.Monad.IO.Class ( liftIO )
import qualified Web.ClientSession as ClientSession

import qualified Data.Text as Text
import qualified Data.Aeson as JS
import           Data.IORef (IORef, newIORef, readIORef)
import           Data.Map.Strict (Map, findWithDefault)
import           Data.Text (Text)

import qualified Snap
import qualified Snap.Snaplet.Session as Session
import qualified Snap.Snaplet.Session.Backends.CookieSession as Session

import           Language.ASKEE
import           Language.ASKEE.Exposure.GenLexer (lexExposure)
import           Language.ASKEE.Exposure.GenParser (parseExposureStmt)
import qualified Language.ASKEE.Exposure.Interpreter as Exposure

import           Schema

-- Snaplet Definition ---------------------------------------------------------

data Donu sim = Donu
 { _simulatorSessions :: Snap.Snaplet Session.SessionManager
 , _simulatorState    :: IORef (Map Text sim)
 }

makeLenses ''Donu

exposureState :: s -> Snap.Handler (Donu s) (Donu s) s
exposureState emp =
  do tk    <- Snap.with simulatorSessions Session.csrfToken
     stRef <- view simulatorState
     findWithDefault emp tk <$> liftIO (readIORef stRef)

-------------------------------------------------------------------------------

initDonu :: Snap.SnapletInit (Donu ()) (Donu ())
initDonu =
  Snap.makeSnaplet "donu" "donu" Nothing $
    do sess <- Snap.nestSnaplet "sessions" simulatorSessions $
                 Session.initCookieSessionManager
                   ClientSession.defaultKeyFile -- TODO: Do we care about this?
                   "exposure-env-id"
                   Nothing -- TODO: Do we need to set a domain?
                   Nothing -- TODO: We should probably set a timeout
       Snap.addRoutes [ ("/help", showHelp)
                      , ("/", endpoint)
                      ]
       m <- liftIO $ newIORef mempty
       return $ Donu sess m
  where
    endpoint =
     do let limit = 8 * 1024 * 1024    -- 8 megs
        Snap.modifyResponse (Snap.setHeader "Access-Control-Allow-Origin" "*")
        body <- Snap.readRequestBody limit
        case JS.eitherDecode body of
          Right a ->
               runHandler a
           <|> do Snap.modifyResponse
                   (Snap.setResponseStatus 400 "Bad request")
                  -- Snap.writeText $ Text.pack "ASDF")
          Left err ->
            do  Snap.writeText $ Text.pack err
                Snap.modifyResponse (Snap.setResponseStatus 400 "Bad request")

    runHandler r =
      do out <- handleRequest r
         let (code, msg) =
               case out of
                 (FailureResult _) -> (400, "Error")
                 _ -> (200, "OK")
         Snap.modifyResponse (Snap.setResponseStatus code msg)
         Snap.writeLBS (JS.encode out)


main :: IO ()
main =
  do initStorage
     Snap.serveSnaplet Snap.defaultConfig initDonu

showHelp :: Snap.Handler (Donu sim) (Donu sim) ()
showHelp = Snap.writeLBS helpHTML

--------------------------------------------------------------------------------


handleRequest :: Input -> Snap.Handler (Donu ()) (Donu ()) Result
handleRequest r =
  liftIO (print r) >>
  case r of
    SimulateGSL SimulateGSLCommand{..} ->
      do  res <-
            liftIO $ simulateModelGSL
              (modelDefType simModelGSL)
              (modelDefSource simModelGSL)
              simStartGSL
              simEndGSL
              simStepGSL
              simParameterValuesGSL
          succeed' res

    SimulateAJ SimulateAJCommand{..} ->
      do  res <-
            liftIO $ simulateModelAJ
              (modelDefType simModelAJ)
              (modelDefSource simModelAJ)
              simStartAJ
              simEndAJ
              simStepAJ
              simParameterValuesAJ
          succeed' res

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
      do  (res, _) <-
            liftIO $
            fitModelToData
              (modelDefType fitModel)
              fitData
              fitParams
              mempty
              (modelDefSource fitModel)
          succeed' (FitResult res)

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

    ExecuteExposureCode (ExecuteExposureCodeCommand code) ->
      case lexExposure (Text.unpack code) >>= parseExposureStmt of
        Left err   ->
          pure $ FailureResult $ Text.pack err
        Right stmt ->
          Session.withSession simulatorSessions $
            do _state <- exposureState ()
               return $ undefined -- succeed $ Exposure.interpretStmt stmt
  where
    succeed :: (JS.ToJSON a, Show a) => a -> Result
    succeed = SuccessResult

    succeed' :: (JS.ToJSON a, Show a) => a -> Snap.Handler x x Result
    succeed' = pure . succeed
