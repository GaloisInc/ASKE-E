{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main ( main ) where

import Control.Monad.State

import           Control.Exception.Lifted (try, SomeException)
import           Control.Applicative ((<|>))
import           Control.Lens.TH
import qualified Web.ClientSession as ClientSession

import qualified Data.Text as Text
import qualified Data.Aeson as JS
import           Data.Maybe (fromMaybe)

import qualified Snap

import           Language.ASKEE
import qualified Language.ASKEE.Exposure.Interpreter as Exposure
import           Language.ASKEE.Exposure.GenLexer (lexExposure)
import           Language.ASKEE.Exposure.GenParser (parseExposureStmts)

import           Schema
import           ExposureSession
import qualified Language.ASKEE.Exposure.Syntax as Exposure

-- Snaplet Definition ---------------------------------------------------------

newtype Donu sim = Donu
 { _exposureSessions :: Snap.Snaplet (ExposureSessionManager sim)
   -- ^ Snaplet implementing Exposure REPL sessions
 }

makeLenses ''Donu

-------------------------------------------------------------------------------

type ExposureState = Exposure.Env
type DonuApp = Donu ExposureState

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
          Right a -> runHandler a <|> errorWith 400 "Bad request"
          Left err -> errorWith 400 ("Didn't recognize request: "<>err)

    runHandler r =
      do result <- try $ handleRequest r
         case result of
          Right out ->
            do  let (code, msg) =
                        case out of
                          (FailureResult _) -> (400, "Error")
                          _ -> (200, "OK")
                Snap.modifyResponse (Snap.setResponseStatus code msg)
                Snap.writeLBS (JS.encode out)
          Left ex -> errorWith 500 (show (ex :: SomeException))



errorWith :: Int -> String -> Snap.Handler DonuApp DonuApp ()
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
     Snap.serveSnaplet Snap.defaultConfig initDonu

showHelp :: Snap.Handler (Donu sim) (Donu sim) ()
showHelp = Snap.writeLBS helpHTML

--------------------------------------------------------------------------------


handleRequest :: Input -> Snap.Handler DonuApp DonuApp Result
handleRequest r =
  liftIO (print r) >>
  Snap.with exposureSessions cullOldSessions  >>
  case r of
    Simulate SimulateCommand{..} ->
      do  ifaceErrs <- liftIO $ checkSimArgs (modelDefType simModel) (modelDefSource simModel) simParameterValues simOutputs
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

    QueryModels QueryModelsCommand {..} ->
      succeed <$> liftIO (queryModels queryText)

    ExecuteExposureCode (ExecuteExposureCodeCommand code) ->
      case lexExposure (Text.unpack code) >>= parseExposureStmts of
        Left err ->
          pure $ FailureResult $ Text.pack err
        Right stmts ->
          stepExposureSession stmts

    ResetExposureState _ ->
      do Snap.with exposureSessions $
           putExposureSessionState Exposure.initialEnv
         succeed' ()
  where
    succeed :: (JS.ToJSON a, Show a) => a -> Result
    succeed = SuccessResult

    succeed' :: (JS.ToJSON a, Show a) => a -> Snap.Handler x x Result
    succeed' = pure . succeed

-- Driving Exposure -----------------------------------------------------------

-- | Run the given exposure statements in the current session and return the
-- resulting DisplayValues as a (JSON) list (or any error messages)
stepExposureSession :: [Exposure.Stmt] -> Snap.Handler DonuApp DonuApp Result
stepExposureSession stmts =
  do env <- Snap.with exposureSessions $
       fromMaybe Exposure.initialEnv <$> getExposureSessionState
     (res, env') <- liftIO $ Exposure.evalStmts stmts env
     case res of
       Left err ->
         do Snap.with exposureSessions $
              putExposureSessionState env'
            pure $ FailureResult err
       Right (displays, _effs) -> -- TODO: Do we want to do anything with _effs?
         do Snap.with exposureSessions $
              putExposureSessionState env'
            return . SuccessResult $ fmap (DonuValue . Exposure.unDisplayValue) displays
