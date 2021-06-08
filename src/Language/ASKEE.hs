{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE
  ( loadDiffEqs
  , loadDiffEqsFrom
  , loadESL
  , loadESLMeta
  , loadESLMetaFrom
  -- , loadReactionsFrom
  -- , loadLatexFrom
  , loadCPPFrom
  , loadCoreFrom
  
  , checkModel
    
  , simulateModel
  , stratifyModel
  , fitModelToData
  , Core.asSchematicGraph
  , convertModelString
  , ESL.describeModelInterface

  , gnuPlotScript
  , dataSeriesAsCSV
  , dataSeriesAsJSON
  , parseDataSeriesFromFile

  , initStorage
  , listAllModels
  , loadModel
  , storeModel

  , DataSource(..)
  , DataSeries(..)
  , DEQ.DiffEqs
  , ModelDef(..)
  , ModelType(..)
  , Stratify.StratificationInfo(..)
  , Stratify.StratificationType(..)
  ) where

import Control.Exception (throwIO, try, SomeException(..) )
import Control.Monad ( void )

import           Data.Aeson                 ( Value
                                            , decode )
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
import           Language.ASKEE.Error                  ( ASKEEError(..)
                                                       , throwLeft )
import           Language.ASKEE.Model                  ( parseModel
                                                       , printModel
                                                       , toDeqs
                                                       , toEasel
                                                       , toCore
                                                       , Model (..) )
import           Language.ASKEE.ModelType              ( ModelType(..) )
import qualified Language.ASKEE.ModelStratify.GeoGraph as GG
import qualified Language.ASKEE.ModelStratify.Stratify as Stratify
import qualified Language.ASKEE.SimulatorGen           as SimulatorGen
import           Language.ASKEE.Storage                ( initStorage
                                                       , listAllModels
                                                       , loadModel
                                                       , storeModel
                                                       , DataSource(..)
                                                       , ModelDef(..) )

-------------------------------------------------------------------------------
-- ESL with Metadata

loadESLMeta :: DataSource -> IO ESL.ModelMeta 
loadESLMeta = loadESLMetaFrom EaselType

loadESLMetaFrom :: ModelType -> DataSource -> IO ESL.ModelMeta
loadESLMetaFrom format source =
  do  modelString <- loadModel format source
      model <- throwLeft ParseError (parseModel format modelString)
      esl <- throwLeft ConversionError (toEasel model)
      _ <- throwLeft ValidationError (ESL.checkModel $ ESL.stripMeta esl)
      pure esl

-------------------------------------------------------------------------------
-- Plain ESL

loadESL :: DataSource -> IO ESL.Model
loadESL = loadESLFrom EaselType

loadESLFrom :: ModelType -> DataSource -> IO ESL.Model
loadESLFrom format source = ESL.stripMeta <$> loadESLMetaFrom format source

-------------------------------------------------------------------------------
-- Core

loadCore :: DataSource -> IO Core.Model
loadCore = loadCoreFrom EaselType

loadCoreFrom :: ModelType -> DataSource -> IO Core.Model
loadCoreFrom format source =
  do  modelString <- loadModel format source
      model <- throwLeft ParseError (parseModel format modelString)
      throwLeft ConversionError (toCore model)

-------------------------------------------------------------------------------
-- DEQs

loadDiffEqs :: DataSource -> IO DEQ.DiffEqs
loadDiffEqs = loadDiffEqsFrom DeqType

loadDiffEqsFrom :: ModelType -> DataSource -> IO DEQ.DiffEqs
loadDiffEqsFrom format source =
  do  modelString <- loadModel format source
      model <- throwLeft ParseError (parseModel format modelString)
      throwLeft ConversionError (toDeqs model)

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


-------------------------------------------------------------------------------
-- TODO: Gromet

-- loadGromet :: DataSource -> IO String
-- loadGromet source = 
--   do  model <- loadModel (GROMET Concrete) source
--       (code, _out, _err) <- readProcessWithExitCode "jq" [] model
--       case code of
--         ExitSuccess -> pure model
--         ExitFailure _ -> throwIO $ ParseError "invalid gromet"



checkModel :: ModelType -> DataSource -> IO (Maybe String)
checkModel format source =
  do  result <- try
        case format of
          EaselType -> void $ loadESL source
          CoreType -> void $ loadCore source
          DeqType -> void $ loadDiffEqs source
      case result of
        Left err -> pure $ Just (show (err :: SomeException))
        Right _ -> pure Nothing

loadConnectionGraph :: String -> IO (Value, Map Int Text)
loadConnectionGraph s = 
  do  result <- case GG.parseGeoGraph s of
        Right res -> pure res
        Left err -> throwIO $ ParseError err
      let (vertices, edges, mapping) = GG.intGraph result
          mapping' = Map.fromList [(i, Text.pack $ mapping i) | i <- [1..vertices]]
      pure (GG.gtriJSON vertices edges, mapping')

loadCPPFrom :: ModelType -> DataSource -> IO Doc
loadCPPFrom format source =
  do  coreModel <- loadCoreFrom format source
      pure $ SimulatorGen.genModel coreModel

-------------------------------------------------------------------------------

stratifyModel ::
  DataSource ->
  String ->
  Maybe String ->
  Stratify.StratificationType ->
  IO Stratify.StratificationInfo
stratifyModel modelFile connectionGraph statesJSON stratificationType =
  do  model <- loadESL modelFile
      (connections, vertices) <- loadConnectionGraph connectionGraph
      states <- 
        case statesJSON of 
          Just d -> pure $ decode @Stratify.States $ B.pack d
          Nothing -> pure Nothing
      Stratify.stratifyModel model connections vertices states stratificationType


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
      pure $ DEQ.fitModel eqs dataSeries fitScale (Map.fromList (zip fitParams (repeat 0)))
      

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
  do  modelString <- loadModel srcTy src
      let model = parseModel srcTy modelString
      pure 
        case destTy of
          EaselType -> model >>= toEasel >>= (printModel . Easel)
          DeqType -> model >>= toDeqs >>= (printModel . Deq)
          -- If there are errors converting to core, we _might_ want to
          -- see them more than we want to see the printing error? Maybe?
          CoreType -> model >>= toCore >>= const (Left "cannot print core")
