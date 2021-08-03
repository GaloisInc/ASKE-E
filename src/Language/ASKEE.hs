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
  , loadGrometPnc
  , loadGrometPncFrom
  , loadGrometFnet
  , loadGrometFnetFrom
  , loadCPPFrom
  , loadCore
  , loadCoreFrom
  , loadModel
  
  , checkModel
  , checkModel'
    
  , checkSimArgs
  , checkFitArgs
  , checkInterfaceRequirements
  , paramsNotExistErrors
  , outputsNotExistErrors
  , unspecifiedValueErrors
  , simulateModelGSL
  , simulateModelAJ
  , simulateModelDiscrete
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
  , loadModelText
  , storeModel
  , queryModels

  , describeModelType

  , DataSource(..)
  , DataSeries(..)
  , DEQ.DiffEqs
  , MetaAnn(..)
  , ModelDef(..)
  , ModelType(..)
  , SimulationType(..)
  , Stratify.StratificationInfo(..)
  , Stratify.StratificationType(..)


    -- * Model Interface
  , ModelInterface(..)
  , Port(..)
  , describeModelInterface
  ) where

import Control.Exception ( try, SomeException(..) )
import Control.Monad     ( forM, filterM )

import           Data.Aeson                 ( decode )
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Builder    as Builder
import qualified Data.Char                  as Char
import           Data.Set                   ( Set )
import qualified Data.Set                   as Set
import           Data.Map                   ( Map )
import qualified Data.Map                   as Map
import           Data.Maybe                 ( fromMaybe )
import           Data.Text                  ( Text )
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import qualified Data.Text.Encoding         as Text

import qualified Language.ASKEE.ESL                    as ESL
import           Language.ASKEE.ESL.Manipulate         ( compose
                                                       , join
                                                       , ensemble
                                                       , CombinationStrategy(..) )
import           Language.ASKEE.CPP.Pretty             ( Doc )
import qualified Language.ASKEE.Core                   as Core
import           Language.ASKEE.DataSeries             ( dataSeriesAsCSV
                                                       , dataSeriesAsJSON
                                                       , gnuPlotScript
                                                       , parseDataSeries
                                                       , parseDataSeriesFromFile
                                                       , DataSeries(..) )
import qualified Language.ASKEE.DataSeries             as DS
import qualified Language.ASKEE.DEQ                    as DEQ
import           Language.ASKEE.Gromet                 ( Gromet, PetriNetClassic)
import           Language.ASKEE.Error                  ( ASKEEError(..)
                                                       , throwLeft
                                                       , die )
import           Language.ASKEE.Metadata               ( MetaAnn(..) )
import           Language.ASKEE.Model                  ( parseModel
                                                       , printModel
                                                       , toDeqs
                                                       , toEasel
                                                       , toCore
                                                       , toGrometPnc
                                                       , toGrometPrt
                                                       , toGrometFnet
                                                       , Model (..) )
import           Language.ASKEE.Model.Basics           ( ModelType(..)
                                                       , describeModelType )
import           Language.ASKEE.Model.Interface        ( ModelInterface(..)
                                                       , Port(..)
                                                       , emptyModelInterface
                                                       )
import           Language.ASKEE.Panic                  ( panic )
import qualified Language.ASKEE.AlgebraicJulia.Simulate as AJ
import qualified Language.ASKEE.AlgebraicJulia.GeoGraph as GG
import qualified Language.ASKEE.AlgebraicJulia.Stratify as Stratify
import qualified Language.ASKEE.Gromet.FunctionNetwork  as FNet
import qualified Language.ASKEE.CPP                     as CPP
import           Language.ASKEE.Storage                ( initStorage
                                                       , listAllModels
                                                       , loadModelText
                                                       , DataSource(..)
                                                       , ModelDef(..)
                                                       , doesModelExist
                                                       )
import qualified Language.ASKEE.Storage                as Storage
import qualified Language.ASKEE.Automates.Client       as Automates

loadModel :: ModelType -> DataSource -> IO Model
loadModel format source =
  do yes <- doesModelExist format source
     if yes
       then doLoadModel format source

       -- Special case for "virtual" PrtGromet
       else case format of
              GrometPrtType -> Easel <$> loadESL source
              _ -> doLoadModel format source
  where
  doLoadModel fmt src =
    do modelString <- loadModelText fmt src
       throwLeft ParseError (parseModel fmt modelString)

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
-- PNC
loadGrometPnc :: DataSource -> IO PetriNetClassic
loadGrometPnc = loadGrometPncFrom GrometPncType 

loadGrometPncFrom :: ModelType -> DataSource -> IO PetriNetClassic
loadGrometPncFrom format source =
  do  model <- loadModel format source
      throwLeft ConversionError (toGrometPnc model)


-------------------------------------------------------------------------------
-- FNET
loadGrometFnet :: DataSource -> IO JSON.Value
loadGrometFnet = loadGrometFnetFrom GrometFnetType

loadGrometFnetFrom :: ModelType -> DataSource -> IO (FNet.FunctionNetwork)
loadGrometFnetFrom format source =
  do  model <- loadModel format source
      throwLeft ConversionError (toGrometFnet model)


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
      pure $ CPP.genModel coreModel

-------------------------------------------------------------------------------
-- Storage

storeModel :: ModelType -> Text -> Text -> IO ModelDef
storeModel mt name modelText =
  do checkModel mt modelText
     Storage.storeModel name mt modelText
     pure ModelDef { modelDefSource = FromStore name
                   , modelDefType   = mt
                   }


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
    _ -> \_ -> pure ()  -- We don't know how to validate this

checkESL :: Text -> IO ()
checkESL t =
  do  Easel esl <- throwLeft ParseError (parseModel EaselType t)
      _ <- throwLeft ValidationError (ESL.checkModel esl)
      pure ()

checkDEQ :: Text -> IO ()
checkDEQ t =
  do  Deq _ <- throwLeft ParseError (parseModel DeqType t)
      pure ()

-- XXX: not validated for the moment.
checkGrometPrt :: Text -> IO ()
checkGrometPrt _ = pure ()
{-
  do  (code, _out, _err) <- readProcessWithExitCode "jq" [] (Text.unpack t)
      case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> die (ValidationError "invalid gromet")
-}



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
        case decode . LBS8.pack <$> statesJSON of 
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
          FromFile f -> Text.readFile f
          FromStore _ -> panic "fitModelToData" ["reading data from store is not yet supported"]
      let bytes = Builder.toLazyByteString (Text.encodeUtf8Builder rawData)
      dataSeries <- throwLeft DataSeriesError (parseDataSeries bytes)
      pure $ DEQ.fitModel eqs dataSeries fitScale
           $ Map.fromList (zip fitParams (repeat 0))

-------------------------------------------------------------------------------
-- Simulation

data SimulationType = 
    AJ 
  | Discrete 
  | GSL 
  | AutomatesSvc
  deriving (Show)


-- check some of the arguments to simulate functions
-- against the described interface, possibly returning
-- a list of human-readable problems
checkSimArgs ::
  ModelType ->
  DataSource ->
  Map Text Double {- ^ parameterization -} ->
  Set Text {- ^ variables to measure -} ->
  IO [Text]
checkSimArgs mt ds params outs =
  checkInterfaceRequirements mt ds
      [ paramsNotExistErrors (Map.keysSet params)
      , outputsNotExistErrors outs
      , unspecifiedValueErrors (Map.keysSet params)
      ]

-- check that the params to fit are a subset of the model's inputs.
checkFitArgs ::
  ModelType ->
  DataSource ->
  [Text] ->
  IO [Text]
checkFitArgs mt ds params =
  checkInterfaceRequirements mt ds
    [ paramsNotExistErrors (Set.fromList params) ]

paramsNotExistErrors :: Set Text -> ModelInterface -> [Text]
paramsNotExistErrors params iface =
  requireSubset params (ifaceParamNames iface)
    (\v -> "'" <> v <> "' is not a parameter of the specified model")

outputsNotExistErrors :: Set Text -> ModelInterface -> [Text]
outputsNotExistErrors outs iface =
  requireSubset outs (ifaceOutputNames iface)
                (\v -> "'" <> v <> "' is not a measurable quantity of the specified model")

unspecifiedValueErrors :: Set Text -> ModelInterface -> [Text]
unspecifiedValueErrors params iface =
  requireSubset (ifaceParamsNoDefault iface) params
                (\v -> "'" <> v <> "' has no default and must be specified")

ifaceParamNames :: ModelInterface -> Set Text
ifaceParamNames iface =
  Set.fromList (portName <$> modelInputs iface)

ifaceParamsNoDefault :: ModelInterface -> Set Text
ifaceParamsNoDefault iface =
  Set.fromList [portName p | p <- modelInputs iface, portDefault p == Nothing]

ifaceOutputNames :: ModelInterface -> Set Text
ifaceOutputNames iface =
  Set.fromList (portName <$> modelOutputs iface)

checkInterfaceRequirements :: ModelType -> DataSource -> [ModelInterface -> [Text]] -> IO [Text]
checkInterfaceRequirements mt ds reqs =
  do model <- loadModel mt ds
     let iface = describeModelInterface model
     pure $ concat [r iface | r <- reqs]

requireSubset :: Ord a => Set a -> Set a -> (a -> Text) -> [Text]
requireSubset a b err = err <$> Set.toList (Set.difference a b)


simulateModel :: 
  Maybe SimulationType ->
  ModelType -> 
  DataSource ->
  Double {- ^ start -} ->
  Double {- ^ end -} -> 
  Double {- ^ step -} ->
  Map Text Double {- ^ parameterization -} -> 
  Set Text {- ^ variables to measure (empty to measure all) -} -> 
  Maybe Int {- ^ seed (for discrete event simulation) -} -> 
  Maybe Text {- ^ domain parameter (for function networks) -} -> 
  Int ->
  IO [DS.LabeledDataSeries Double]
simulateModel sim format source start end step parameters outputs seed dp iterations =
  case simType of
    GSL -> (:[]) . DS.ldsFromDs . filterDS <$> simulateModelGSL format source start end step parameters outputs
    Discrete ->  fmap (DS.ldsFromDs . filterDS) <$> simulateModelDiscrete format source start end step parameters seed iterations
    AJ -> (:[]) . DS.ldsFromDs . filterDS <$> simulateModelAJ format source start end step parameters
    AutomatesSvc -> (:[]) <$> simulateModelAutomates format source start end step dp parameters outputs
  where
    simType = fromMaybe defaultSimulationType sim
    defaultSimulationType =
      case format of
        EaselType -> GSL
        GrometPncType -> AJ
        GrometPrtType -> GSL
        GrometFnetType -> AutomatesSvc
        RNetType -> GSL
        DeqType -> GSL
        CoreType -> GSL
    filterDS ds 
      | null outputs = ds
      | otherwise = ds { DS.values = Map.restrictKeys (DS.values ds) outputs }

simulateModelAutomates ::
  ModelType ->
  DataSource ->
  Double ->
  Double ->
  Double ->
  Maybe Text ->
  Map Text Double ->
  Set Text ->
  IO (DS.LabeledDataSeries Double)
simulateModelAutomates format source start end step domainParamMb params outVars =
  do  fnet <- loadGrometFnetFrom format source
      domainParam <-
        throwLeft HttpCallException
          case domainParamMb of
            Nothing -> Left "Domain parameter was not specified"
            Just p -> Right p

      let simReq =
            Automates.SimulationRequest fnet start end step domainParam params outVars

      Automates.simulateFnet simReq

simulateModelGSL :: 
  ModelType -> 
  DataSource -> 
  Double {- ^ start -} ->
  Double {- ^ stop -} -> 
  Double {- ^ step -} -> 
  Map Text Double {- ^ parameters -} ->
  Set Text {- ^ Variables to observe, empty for everything -} ->
  IO (DataSeries Double)
simulateModelGSL format source start end step parameters vars =
  do  equations <- loadDiffEqsFrom format source
      let times' = takeWhile (<= end)
                 $ iterate (+ step) start
      pure $ DEQ.simulate equations parameters vars times'

simulateModelDiscrete ::
  ModelType ->
  DataSource ->
  Double {- ^ start time -} ->
  Double {- ^ end time -} -> 
  Double {- ^ time step -} -> 
  Map Text Double ->
  Maybe Int {- ^ seed -} ->
  Int {- ^ number of iterations -}->
  IO [DataSeries Double]
simulateModelDiscrete format source start end step parameters seed iterations =
  do  model <- loadCoreFrom format source
      let parameterizedModel = Core.addParams parameters model
          (withLegalNames, newNames) = Core.legalize parameterizedModel
      dss <- CPP.simulate withLegalNames start end step seed iterations
      mapM (\DataSeries{..} -> pure DataSeries { values = adjust newNames values, ..}) dss

  where
    adjust vs = Map.fromList . map (\(v, e) -> (vs Map.! v, e)) . Map.toList
      
simulateModelAJ ::
  ModelType -> 
  DataSource -> 
  Double {- ^ start -} ->
  Double {- ^ stop -} -> 
  Double {- ^ step -} -> 
  Map Text Double ->
  IO (DataSeries Double)
simulateModelAJ format source start stop step parameters =
  do  pnc <- loadGrometPncFrom format source
      let newParameters = Map.mapKeys demangle parameters
      AJ.simulate pnc start stop step newParameters
  where
    demangle t = 
      case (Text.stripSuffix "_init" t, Text.stripSuffix "_rate" t) of
        (Just t', _) -> t'
        (_, Just t') -> t'
        (Nothing, Nothing) -> t

-------------------------------------------------------------------------------

convertModelString ::
  ModelType -> DataSource -> ModelType -> IO (Either String String)
convertModelString srcTy src destTy =
  do  bytes <- loadModelText srcTy src
      let model = parseModel srcTy bytes
      pure
        case destTy of
          EaselType -> model >>= toEasel >>= (printModel . Easel)
          DeqType -> model >>= toDeqs >>= (printModel . Deq)
          -- If there are errors converting to core, we _might_ want to
          -- see them more than we want to see the printing error? Maybe?
          CoreType -> model >>= toCore >>= const (Left "cannot print core")
          GrometPrtType -> model >>= toGrometPrt >>= (printModel . GrometPrt)
          GrometPncType -> model >>= toGrometPnc >>= (printModel . GrometPnc)
          GrometFnetType -> model >>= toGrometFnet >>= (printModel . GrometFnet)
          RNetType  -> Left "Don't know how to convert to RNet yet"


          
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


queryModels :: Text -> IO [MetaAnn ModelDef]
queryModels query = listAllModelsWithMetadata >>= filterM match_model
  where
    match_model metaAnnModel = do
      (toplevelMetaData, model) <- loadModelFromDef metaAnnModel
      let mInterface = describeModelInterface model
          match_result = match_metadata_values (map snd toplevelMetaData) ||
                         match_metadata_values (portMetaDataValues $ modelInputs mInterface) ||
                         match_metadata_values (portMetaDataValues $ modelOutputs mInterface)
      return match_result
    match_metadata_values values = any (match_wildcard query) values
    loadModelFromDef metaAnnModel = do
      let ModelDef {..} = metaValue metaAnnModel
      model <- loadModel modelDefType modelDefSource
      return (metaData metaAnnModel, model)
    portMetaDataValues ports = concat $ concatMap (Map.elems . portMeta) ports
    match_wildcard pat s
      | Text.null pat        = Text.null s
      | Text.head pat == '*' = handleStar
      | Text.head pat == '?' = handleQM
      | otherwise            = handleChar
      where
        handleStar =
          match_wildcard (Text.tail pat) s
          || (not (Text.null s) && match_wildcard pat (Text.tail s))
        handleQM =
          not (Text.null s) && match_wildcard (Text.tail pat) (Text.tail s)
        handleChar =
          not (Text.null s) &&
          Char.toLower (Text.head s) == Char.toLower (Text.head pat) &&
          match_wildcard (Text.tail pat) (Text.tail s)

--------------------------------------------------------------------------------

describeModelInterface :: Model -> ModelInterface
describeModelInterface model = asCore `orElse` (asFnet `orElse` emptyModelInterface)
  where
    eitherToMaybe (Left _) = Nothing
    eitherToMaybe (Right a) = Just a

    orElse (Just a) _ = a
    orElse Nothing b = b

    asCore = Core.modelInterface <$> eitherToMaybe (toCore model)
    asFnet =
      case model of
        GrometFnet json -> eitherToMaybe (FNet.fnetInterface json)
        _ -> Nothing
