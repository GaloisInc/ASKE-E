{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE
  ( loadDiffEqsFrom
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

  , initStorage
  , listAllModels
  , loadModel
  , storeModel
  , DataSource(..)
  , DataSeries(..)
  , ModelDef(..)
  , ModelType(..)
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
import qualified Language.ASKEE.Core.Syntax            as Core
import           Language.ASKEE.DataSeries             ( DataSeries(..)
                                                       , parseDataSeries
                                                       , MalformedDataSeries(..) )
import qualified Language.ASKEE.DEQ                    as DEQ
import           Language.ASKEE.Error                  ( ASKEEError(..)
                                                       , throwLeft
                                                       , die )
import           Language.ASKEE.Model                  --( convertModelString )
import           Language.ASKEE.ModelType              ( ModelType(..) )
import qualified Language.ASKEE.ModelStratify.GeoGraph as GG
import qualified Language.ASKEE.ModelStratify.Stratify as Stratify
-- import           Language.ASKEE.Simulate               ( simulateModel )
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
  do  model <- loadESLFrom format source
      throwLeft ValidationError (ESL.modelAsCore model)

-------------------------------------------------------------------------------
-- DEQs

loadDiffEqs :: DataSource -> IO DEQ.DiffEqs
loadDiffEqs = loadDiffEqsFrom DeqType

loadDiffEqsFrom :: ModelType -> DataSource -> IO DEQ.DiffEqs
loadDiffEqsFrom format source =
  do  modelString <- loadModel format source
      model <- throwLeft ParseError (parseModel format modelString)
      throwLeft ConversionError (toDeqs model)

-- loadDiffEqs :: Map Text Double -> [Text] -> DataSource -> IO DEQ.DiffEqs
-- loadDiffEqs = loadDiffEqsFrom DeqType


-- loadDiffEqsFrom :: 
--   ModelType ->
--   Map Text Double -> 
--   [Text] -> 
--   DataSource -> 
--   IO DEQ.DiffEqs
-- loadDiffEqsFrom format overwrite params source = 
--   do  modelString <- loadModel format source
--       model <- throwLeft ParseError (parseModel format modelString)
--       undefined
--       -- Core core <- throwLeft ConversionError (toCore model)
--       -- let model' = Core.parameterize overwrite core
--       -- equations <- throwLeft ConversionError (toDeqs model')
--       -- -- let eqns' = equations { deqParams = params }
--       -- -- let replaceParams = applyParams (Map.map NumLit overwrite)
--       -- pure equations

-------------------------------------------------------------------------------
-- Loaders for "first class" models

-- loadReactions :: DataSource -> IO RNet.ReactionNet
-- loadReactions = loadReactionsFrom (RNET Concrete)

-- loadReactionsFrom :: ModelType -> DataSource -> IO RNet.ReactionNet
-- loadReactionsFrom format source =
--   do  modelString <- loadModel format source
--       let conv =
--             case format of
--               RNET Concrete  -> $(converter (RNET Concrete) (RNET Abstract))
--       toIO "loadReactionsFrom" $ conv modelString

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
  DataSource {- ^ the model -} -> 
  IO (Map Core.Ident (Double, Double))
fitModelToData format fitData fitParams source = 
  do  eqs <- loadDiffEqsFrom format source
      rawData <- 
        case fitData of
          Inline s -> pure s
          FromFile _ -> die (NotImplementedError "reading fit data from file")
      dataSeries <- 
        case parseDataSeries (B.pack $ Text.unpack rawData) of
          Right d -> pure d
          Left err -> throwIO (MalformedDataSeries err)
      let (res, _) = DEQ.fitModel eqs dataSeries Map.empty (Map.fromList (zip fitParams (repeat 0)))
      pure res

simulateModel :: 
  ModelType -> 
  DataSource -> 
  Double -> 
  Double ->
  Double ->
  Map Text Double ->
  IO (DataSeries Double)
simulateModel format source start end step overwrite =
  do  equations <- loadDiffEqsFrom format source
      let equations' = DEQ.overwriteParameters overwrite equations
      let times' = takeWhile (<= end)
                 $ iterate (+ step) start
      pure $ DEQ.simulate equations' Map.empty times'