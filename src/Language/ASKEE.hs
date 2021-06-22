{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE
  ( loadDiffEqs
  , loadDiffEqsFrom
  , loadESL
  , loadESLFrom
  , loadGrometPrt
  , loadGrometPrtFrom
  , loadCPPFrom
  , loadCore
  , loadCoreFrom
  , loadModel
  
  , checkModel
  , checkModel'
    
  , simulateModel
  , stratifyModel
  , fitModelToData
  , Core.asSchematicGraph
  , convertModelString

  , gnuPlotScript
  , dataSeriesAsCSV
  , dataSeriesAsJSON
  , parseDataSeriesFromFile

  , initStorage
  , listAllModels
  , listAllModelsWithMetadata
  , loadModelString
  , storeModel

  , describeModelType

  , DataSource(..)
  , DataSeries(..)
  , DEQ.DiffEqs
  , MetaAnn(..)
  , ModelDef(..)
  , ModelType(..)
  , Stratify.StratificationInfo(..)
  , Stratify.StratificationType(..)


    -- * Model Interface
  , ModelInterface(..)
  , Port(..)
  , describeModelInterface
  ) where

import Control.Exception ( try, SomeException(..) )
import Control.Monad ( forM )

import           Data.Aeson                 ( decode )
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Map                   ( Map )
import qualified Data.Map                   as Map
import           Data.Text                  ( Text )
import qualified Data.Text                  as Text

import qualified Language.ASKEE.ESL                    as ESL
import           Language.ASKEE.C                      ( Doc )
import qualified Language.ASKEE.Core                   as Core
import           Language.ASKEE.DataSeries             ( dataSeriesAsCSV
                                                       , dataSeriesAsJSON
                                                       , gnuPlotScript
                                                       , parseDataSeries
                                                       , parseDataSeriesFromFile
                                                       , DataSeries(..) )
import qualified Language.ASKEE.DEQ                    as DEQ
import           Language.ASKEE.Gromet                 ( Gromet )
import           Language.ASKEE.Error                  ( ASKEEError(..)
                                                       , throwLeft
                                                       , die )
import           Language.ASKEE.Metadata               ( MetaAnn(..) )
import           Language.ASKEE.Model                  ( parseModel
                                                       , printModel
                                                       , toDeqs
                                                       , toEasel
                                                       , toCore
                                                       , toGrometPrc
                                                       , toGrometPrt
                                                       , toGrometFnet
                                                       , Model (..) )
import           Language.ASKEE.Model.Basics           ( ModelType(..)
                                                       , describeModelType )
import           Language.ASKEE.Model.Interface        ( ModelInterface(..)
                                                       , Port(..)
                                                       , emptyModelInterface
                                                       )

import qualified Language.ASKEE.ModelStratify.GeoGraph as GG
import qualified Language.ASKEE.ModelStratify.Stratify as Stratify
import qualified Language.ASKEE.SimulatorGen           as SimulatorGen
import           Language.ASKEE.Storage                ( initStorage
                                                       , listAllModels
                                                       , loadModelString
                                                       , DataSource(..)
                                                       , ModelDef(..) )
import qualified Language.ASKEE.Storage                as Storage

import System.Process ( readProcessWithExitCode )
import System.Exit    ( ExitCode(..) )

loadModel :: ModelType -> DataSource -> IO Model
loadModel format source =
  do modelString <- loadModelString format source
     throwLeft ParseError (parseModel format modelString)

-------------------------------------------------------------------------------
-- ESL

loadESL :: DataSource -> IO ESL.Model 
loadESL = loadESLFrom EaselType

loadESLFrom :: ModelType -> DataSource -> IO ESL.Model
loadESLFrom format source =
  do  model <- loadModel format source
      esl <- throwLeft ConversionError (toEasel model)
      _ <- throwLeft ValidationError (ESL.checkModel esl)
      pure esl

-------------------------------------------------------------------------------
-- Core

loadCore :: DataSource -> IO Core.Model
loadCore = loadCoreFrom EaselType

loadCoreFrom :: ModelType -> DataSource -> IO Core.Model
loadCoreFrom format source =
  do  model <- loadModel format source
      throwLeft ConversionError (toCore model)

-------------------------------------------------------------------------------
-- DEQs

loadDiffEqs :: DataSource -> IO DEQ.DiffEqs
loadDiffEqs = loadDiffEqsFrom DeqType

loadDiffEqsFrom :: ModelType -> DataSource -> IO DEQ.DiffEqs
loadDiffEqsFrom format source =
  do  model <- loadModel format source
      throwLeft ConversionError (toDeqs model)

-------------------------------------------------------------------------------
-- Gromet

loadGrometPrt :: DataSource -> IO Gromet
loadGrometPrt = loadGrometPrtFrom GrometPrtType

loadGrometPrtFrom :: ModelType -> DataSource -> IO Gromet
loadGrometPrtFrom format source =
  do  model <- loadModel format source
      throwLeft ConversionError (toGrometPrt model)

-------------------------------------------------------------------------------
-- Data



-------------------------------------------------------------------------------
-- TODO: Reactions

-- loadReactions :: DataSource -> IO RNet.ReactionNet
-- loadReactions = loadReactionsFrom (RNET Concrete)

-- loadReactionsFrom :: ModelType -> DataSource -> IO RNet.ReactionNet
-- loadReactionsFrom format source =
--   do  modelString <- loadModel format source
--       let conv =
--             case format of
--               RNET Concrete  -> $(converter (RNET Concrete) (RNET Abstract))
--       toIO "loadReactionsFrom" $ conv modelString

-------------------------------------------------------------------------------
-- TODO: Latex

-- loadLatex :: DataSource -> IO Latex.Latex
-- loadLatex = loadLatexFrom (LATEX Concrete)

-- loadLatexFrom :: ModelType -> DataSource -> IO Latex.Latex
-- loadLatexFrom format source =
--   do  modelString <- loadModel format source
--       let conv =
--             case format of
--               ESL Concrete   -> $(converter (ESL Concrete) (LATEX Abstract))
--               DEQ Concrete   -> $(converter (DEQ Concrete) (LATEX Abstract))
--               RNET Concrete  -> $(converter (RNET Concrete) (LATEX Abstract))
--               LATEX Concrete -> $(converter (LATEX Concrete) (LATEX Abstract))
--       toIO "loadLatexFrom" $ conv modelString

-- loadConnectionGraph :: String -> IO (Value, Map Int Text)
-- loadConnectionGraph s = 
--   do  result <- case GG.asConnGraph s of
--         Right (graph, nodeName) -> pure graph
--         Left err -> throwIO $ ParseError err
--       let (vertices, edges, mapping) = GG.intGraph result
--           mapping' = Map.fromList [(i, Text.pack $ mapping i) | i <- [1..vertices]]
--       pure (GG.gtriJSON vertices edges, mapping')

loadCPPFrom :: ModelType -> DataSource -> IO Doc
loadCPPFrom format source =
  do  coreModel <- loadCoreFrom format source
      pure $ SimulatorGen.genModel coreModel

-------------------------------------------------------------------------------
-- Storage

storeModel :: ModelType -> Text -> Text -> IO ModelDef
storeModel mt =
  case mt of
    EaselType -> storeESL
    DeqType -> storeDEQ
    GrometPrtType -> storeGrometPrt
    _ -> \_ _ -> die (StorageError $ "don't know how to store model type "<>show mt)

storeESL :: Text -> Text -> IO ModelDef
storeESL name model = 
  do  loc <- Storage.storeModel name EaselType checkESL model
      pure $ ModelDef (FromFile loc) EaselType

storeDEQ :: Text -> Text -> IO ModelDef
storeDEQ name model =
  do  loc <- Storage.storeModel name DeqType checkDEQ model
      pure $ ModelDef (FromFile loc) DeqType

storeGrometPrt :: Text -> Text -> IO ModelDef
storeGrometPrt name model =
  do  loc <- Storage.storeModel name GrometPrtType checkGrometPrt model
      pure $ ModelDef (FromFile loc) GrometPrtType

-------------------------------------------------------------------------------
-- Validation

checkModel' :: ModelType -> Text -> IO (Maybe String)
checkModel' format source = 
  do  res <- try (checkModel format source)
      case res of
        Left err -> pure $ Just (show (err :: SomeException))
        Right _ -> pure Nothing

checkModel :: ModelType -> Text -> IO ()
checkModel mt =
  case mt of
    EaselType -> checkESL
    DeqType -> checkDEQ
    GrometPrtType -> checkGrometPrt
    _ -> \_ -> die (StorageError $ "don't know how to check model type "<>show mt)

checkESL :: Text -> IO ()
checkESL t =
  do  Easel esl <- throwLeft ParseError (parseModel EaselType (Text.unpack t))
      _ <- throwLeft ValidationError (ESL.checkModel esl)
      pure ()

checkDEQ :: Text -> IO ()
checkDEQ t =
  do  Deq _ <- throwLeft ParseError (parseModel DeqType (Text.unpack t))
      pure ()

checkGrometPrt :: Text -> IO ()
checkGrometPrt t =
  do  (code, _out, _err) <- readProcessWithExitCode "jq" [] (Text.unpack t)
      case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> die (ValidationError "invalid gromet")



-------------------------------------------------------------------------------

stratifyModel ::
  ModelType ->
  DataSource ->
  String ->
  Maybe String ->
  Stratify.StratificationType ->
  IO Stratify.StratificationInfo
stratifyModel format source connectionGraph statesJSON stratificationType =
  do  model <- loadESLFrom format source
      (connGraph, vertexNamer) <- throwLeft ParseError (GG.asConnGraph connectionGraph)
      let vertexMap = Map.map Text.pack (GG.asMap connGraph vertexNamer)
      states <- 
        case decode . B.pack <$> statesJSON of 
          Just (Just s) -> pure $ Just s
          Just Nothing -> die (ParseError "invalid states JSON")
          Nothing -> pure Nothing
      Stratify.stratifyModel model connGraph vertexMap states stratificationType
  


fitModelToData ::
  ModelType {- ^ the model's type -}-> 
  DataSource {- ^ the data as ASKEE-produced CSV -} ->
  [Text] {- ^ parameters to fit -} -> 
  Map Text Double {- ^ ??? -} ->
  DataSource {- ^ the model -} -> 
  IO (Map Text (Double, Double), [Map Text Double])
  -- IO (Map Text (Double, Double))
fitModelToData format fitData fitParams fitScale source = 
  do  eqs <- loadDiffEqsFrom format source
      rawData <- 
        case fitData of
          Inline s -> pure s
          FromFile f -> Text.pack <$> readFile f
      dataSeries <- throwLeft DataSeriesError (parseDataSeries (B.pack $ Text.unpack rawData))
      pure $ DEQ.fitModel eqs dataSeries fitScale
           $ Map.fromList (zip fitParams (repeat 0))
      

simulateModel :: 
  ModelType -> 
  DataSource -> 
  Double -> 
  Double ->
  Double ->
  Map Text Double ->
  IO (DataSeries Double)
simulateModel format source start end step parameters =
  do  equations <- loadDiffEqsFrom format source
      let times' = takeWhile (<= end)
                 $ iterate (+ step) start
      pure $ DEQ.simulate equations parameters times'

convertModelString :: ModelType -> DataSource -> ModelType -> IO (Either String String)
convertModelString srcTy src destTy =
  do  modelString <- loadModelString srcTy src
      let model = parseModel srcTy modelString
      pure 
        case destTy of
          EaselType -> model >>= toEasel >>= (printModel . Easel)
          DeqType -> model >>= toDeqs >>= (printModel . Deq)
          -- If there are errors converting to core, we _might_ want to
          -- see them more than we want to see the printing error? Maybe?
          CoreType -> model >>= toCore >>= const (Left "cannot print core")
          GrometPrtType -> model >>= toGrometPrt >>= (printModel . GrometPrt)
          GrometPrcType -> model >>= toGrometPrc >>= (printModel . GrometPrc)
          GrometFnetType -> model >>= toGrometFnet >>= (printModel . GrometFnet)


          
listAllModelsWithMetadata :: IO [MetaAnn ModelDef]
listAllModelsWithMetadata =
  do  models <- listAllModels
      let meta n = [("name", n), ("description", "No description.")]
      forM models \m@ModelDef{..} ->
        case modelDefType of
          EaselType -> 
            do  ESL.Model{..} <- loadESL modelDefSource
                pure $ MetaAnn { metaData = meta modelName, metaValue = m }
          _ -> pure @IO $ pure @MetaAnn m


--------------------------------------------------------------------------------

describeModelInterface :: Model -> ModelInterface
describeModelInterface model =
  case toCore model of
    Right core -> Core.modelInterface core
    Left {} -> emptyModelInterface -- XXX: FN



