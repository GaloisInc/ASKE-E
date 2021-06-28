{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main ( main ) where

import Control.Monad.State

import           Control.Applicative ((<|>))
import           Control.Lens.TH
import qualified Web.ClientSession as ClientSession

import qualified Data.Text as Text
import qualified Data.Aeson as JS

import qualified Snap

import           Language.ASKEE
import           Language.ASKEE.Exposure.GenLexer (lexExposure)
import           Language.ASKEE.Exposure.GenParser (parseExposureStmt)

import           Schema
import ExposureSession (initExposureSessionManager, ExposureSessionManager, putExposureSessionState, getExposureSessionState, cullOldSessions)

-- Snaplet Definition ---------------------------------------------------------

newtype Donu sim = Donu
 { _exposureSessions :: Snap.Snaplet (ExposureSessionManager sim)
 }

makeLenses ''Donu

-- getExposureState :: s -> Snap.Handler (Donu s) (Donu s) s
-- getExposureState emp =
--   do tk    <- Snap.with exposureSessions Session.csrfToken
--      stRef <- view exposureState
--      findWithDefault emp tk <$> liftIO (readIORef stRef)

-- setExposureState :: s -> Snap.Handler (Donu s) (Donu s) ()
-- setExposureState v =
--   do tk    <- Snap.with exposureSessions Session.csrfToken
--      stRef <- view exposureState
--      liftIO $ modifyIORef stRef $ insert tk v

-- deleteExposureState :: Snap.Handler (Donu s) (Donu s) ()
-- deleteExposureState =
--   do tk    <- Snap.with exposureSessions Session.csrfToken
--      stRef <- view exposureState
--      liftIO $ modifyIORef stRef $ delete tk

-------------------------------------------------------------------------------

type ExposureState st = (Int, st)
type DonuApp = Donu (ExposureState ())

initDonu :: Snap.SnapletInit DonuApp DonuApp
initDonu =
  Snap.makeSnaplet "donu" "donu" Nothing $
    do sess <- Snap.nestSnaplet "sessions" exposureSessions $
                 initExposureSessionManager
                   ClientSession.defaultKeyFile -- TODO: Do we care about this?
                   "exposure-env-id"
                   1800
       Snap.addRoutes [ ("/help", showHelp)
                      , ("/", endpoint)
                      ]
       return $ Donu sess
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


handleRequest :: Input -> Snap.Handler DonuApp DonuApp Result
handleRequest r =
  liftIO (print r) >>
  Snap.with exposureSessions cullOldSessions  >>
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
        Right _stmt ->
          do x <- Snap.with exposureSessions $
               do st  <- getExposureSessionState
                  case st of
                    Just (i, ()) ->
                     do  putExposureSessionState (i+1, ())
                         return (i+1, ())
                    Nothing ->
                      do putExposureSessionState (0, ())
                         return (0, ())
             succeed' x

    ResetExposureState _ ->
      do Snap.with exposureSessions $
           putExposureSessionState (0, ())
         succeed' (0::Int, ())
  where
    succeed :: (JS.ToJSON a, Show a) => a -> Result
    succeed = SuccessResult

    succeed' :: (JS.ToJSON a, Show a) => a -> Snap.Handler x x Result
    succeed' = pure . succeed
