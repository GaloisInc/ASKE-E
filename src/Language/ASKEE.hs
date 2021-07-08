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
  , loadCPPFrom
  , loadCore
  , loadCoreFrom
  , loadModel
  
  , checkModel
  , checkModel'
    
  , simulateModelGSL
  , simulateModelAJ
  , simulateModelDiscrete
  
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
  , Stratify.StratificationInfo(..)
  , Stratify.StratificationType(..)


    -- * Model Interface
  , ModelInterface(..)
  , Port(..)
  , describeModelInterface
  ) where

import Control.Exception ( try, SomeException(..) )
import Control.Monad     ( forM )

import           Data.Aeson                 ( decode )
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Builder    as Builder
import           Data.Set                   ( Set )
import           Data.Map                   ( Map )
import qualified Data.Map                   as Map
import           Data.Maybe                 ( fromMaybe )
import           Data.Text                  ( Text )
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import qualified Data.Text.Encoding         as Text

import qualified Language.ASKEE.ESL                    as ESL
import           Language.ASKEE.CPP.Pretty             ( Doc )
import qualified Language.ASKEE.Core                   as Core
import           Language.ASKEE.DataSeries             ( dataSeriesAsCSV
                                                       , dataSeriesAsJSON
                                                       , gnuPlotScript
                                                       , parseDataSeries
                                                       , parseDataSeriesFromFile
                                                       , DataSeries(..) )
import qualified Language.ASKEE.DEQ                    as DEQ
import           Language.ASKEE.Gromet                 ( Gromet, PetriNetClassic )
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
import qualified Language.ASKEE.CPP                     as CPP
import           Language.ASKEE.Storage                ( initStorage
                                                       , listAllModels
                                                       , loadModelText
                                                       , DataSource(..)
                                                       , ModelDef(..) )
import qualified Language.ASKEE.Storage                as Storage

loadModel :: ModelType -> DataSource -> IO Model
loadModel format source =
  do modelString <- loadModelText format source
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
loadGrometPnc :: DataSource -> IO PetriNetClassic
loadGrometPnc = loadGrometPncFrom GrometPncType 

loadGrometPncFrom :: ModelType -> DataSource -> IO PetriNetClassic
loadGrometPncFrom format source =
  do  model <- loadModel format source
      throwLeft ConversionError (toGrometPnc model)

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
  Maybe Int {- ^ seed -} ->
  IO (DataSeries Double)
simulateModelDiscrete format source start end step seed =
  do  model <- loadCoreFrom format source
      CPP.simulate model start end step seed
      
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


queryModels :: [(Text, Text)] -> IO [MetaAnn ModelDef]
queryModels query = 
  filter match_model <$> listAllModelsWithMetadata
  where
    match_model annModel =
      all (match_metadata annModel) query    
    match_metadata annModel (key, pattern) =
      let mData = metaData annModel
          mValue = fromMaybe "" $ lookup key mData
      in match_wildcard (Text.toLower mValue) (Text.toLower pattern)
    match_wildcard s pat
      | Text.null pat        = Text.null s
      | Text.head pat == '*' = handleStar
      | Text.head pat == '?' = handleQM
      | otherwise            = handleChar
      where
        handleStar =
          match_wildcard s (Text.tail pat) 
          || (not (Text.null s) && match_wildcard (Text.tail s) pat)
        handleQM =
          not (Text.null s) && match_wildcard (Text.tail s) (Text.tail pat)
        handleChar =
          not (Text.null s) && Text.head s == Text.head pat
          && match_wildcard (Text.tail s) (Text.tail pat)



--------------------------------------------------------------------------------

describeModelInterface :: Model -> ModelInterface
describeModelInterface model =
  case toCore model of
    Right core -> Core.modelInterface core
    Left {} -> emptyModelInterface -- XXX: FN



