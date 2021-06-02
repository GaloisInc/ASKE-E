{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE
  ( loadESLFrom
  , loadDiffEqsFrom
  , loadReactionsFrom
  , loadLatexFrom
  , loadCPPFrom
  , loadCoreFrom
  
  , checkModel
  , simulateModel
  , stratifyModel
  , fitModelToData
  , asSchematicGraph
  , describeModelInterface
  , convertModelString

  , initStorage
  , listAllModels
  , loadModel
  , storeModel
  , DataSource(..)
  , DataSeries(..)
  , ModelType(..)
  , Representation(..)
  ) where

import Control.Exception (throwIO, try, SomeException(..))

import           Data.Aeson                 ( Value
                                            , decode
                                            , object
                                            , (.=) )
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Map                   ( Map )
import qualified Data.Map                   as Map
import           Data.Text                  ( Text )
import qualified Data.Text                  as Text

import Control.Monad ( void )

import qualified Language.ASKEE.Check                  as Check
import           Language.ASKEE.Convert                ( converter )
import qualified Language.ASKEE.Core                   as Core
import           Language.ASKEE.Core.DiffEq            ( applyParams )
import qualified Language.ASKEE.Core.GSLODE            as ODE
import           Language.ASKEE.Core.ImportASKEE       ( modelAsCore )
import qualified Language.ASKEE.Core.Visualization     as Viz
import           Language.ASKEE.DataSeries             ( DataSeries(..)
                                                       , parseDataSeries
                                                       , MalformedDataSeries(..) )
import qualified Language.ASKEE.DEQ.Syntax             as DEQ
import qualified Language.ASKEE.Latex.Syntax           as Latex
import qualified Language.ASKEE.RNet.Syntax            as RNet
import qualified Language.ASKEE.SimulatorGen           as SimulatorGen
import qualified Language.ASKEE.Syntax                 as ESL
import           Language.ASKEE.Storage                ( initStorage
                                                       , listAllModels
                                                       , loadModel
                                                       , storeModel )
import qualified Language.ASKEE.Metadata               as Meta
import qualified Language.ASKEE.ModelStratify.GeoGraph as GG
import qualified Language.ASKEE.ModelStratify.Stratify as Stratify
import           Language.ASKEE.Translate              ( ParseModel, parseModel, SerializeModel (serializeModel) )
import           Language.ASKEE.Types                  ( Representation(..)
                                                       , ModelType(..)
                                                       , DataSource(..)
                                                       , ParseError(..)
                                                       , ValidationError(..)
                                                       , NotImplementedError(..) )

import System.Exit    ( ExitCode(..) )
import System.Process ( readProcessWithExitCode )

parse :: ParseModel a => String -> String -> IO a
parse why modelString = 
  case parseModel modelString of
    Left err -> throwIO (ParseError $ why <> err)
    Right m -> pure m

toIO :: String -> Either String a -> IO a
toIO why modelE =
  case modelE of
    Left err -> throwIO (ParseError $ why <> " " <> err)
    Right m -> pure m 

-------------------------------------------------------------------------------
-- Loaders for "first class" models

loadESL :: DataSource -> IO ESL.Model
loadESL = loadESLFrom (ESL Concrete)

loadESLFrom :: ModelType -> DataSource -> IO ESL.Model
loadESLFrom format source =
  do  modelString <- loadModel format source
      let conv =
            case format of
              ESL Concrete -> $(converter (ESL Concrete) (ESL Abstract))
              RNET Concrete -> $(converter (RNET Concrete) (ESL Abstract))
      model <- toIO "loadESLFrom" $ conv modelString
      case Check.checkModel model of
        Left err -> throwIO (ValidationError err)
        Right m -> pure m

loadDiffEqs :: Map Text Double -> [Text] -> DataSource -> IO DEQ.DiffEqs
loadDiffEqs = loadDiffEqsFrom (DEQ Concrete)

loadDiffEqsFrom :: 
  ModelType ->
  Map Text Double -> 
  [Text] -> 
  DataSource -> 
  IO DEQ.DiffEqs
loadDiffEqsFrom format overwrite params source = 
  do  modelString <- loadModel format source
      let conv =
            case format of
              ESL Concrete   -> $(converter (ESL Concrete) (DEQ Abstract))
              DEQ Concrete   -> $(converter (DEQ Concrete) (DEQ Abstract))
              RNET Concrete  -> $(converter (RNET Concrete) (DEQ Abstract))
              LATEX Concrete -> $(converter (LATEX Concrete) (DEQ Abstract))
      eqns <- toIO "loadDiffEqsFrom" $ conv modelString
      -- eqns <- parse "loadDiffEqs" modelString
      let eqns' = eqns { DEQ.deqParams = params }
      let replaceParams = applyParams (Map.map Core.NumLit overwrite)
      pure $ replaceParams eqns'

loadReactions :: DataSource -> IO RNet.ReactionNet
loadReactions = loadReactionsFrom (RNET Concrete)

loadReactionsFrom :: ModelType -> DataSource -> IO RNet.ReactionNet
loadReactionsFrom format source =
  do  modelString <- loadModel format source
      let conv =
            case format of
              RNET Concrete  -> $(converter (RNET Concrete) (RNET Abstract))
      toIO "loadReactionsFrom" $ conv modelString

loadLatex :: DataSource -> IO Latex.Latex
loadLatex = loadLatexFrom (LATEX Concrete)

loadLatexFrom :: ModelType -> DataSource -> IO Latex.Latex
loadLatexFrom format source =
  do  modelString <- loadModel format source
      let conv =
            case format of
              ESL Concrete   -> $(converter (ESL Concrete) (LATEX Abstract))
              DEQ Concrete   -> $(converter (DEQ Concrete) (LATEX Abstract))
              RNET Concrete  -> $(converter (RNET Concrete) (LATEX Abstract))
              LATEX Concrete -> $(converter (LATEX Concrete) (LATEX Abstract))
      toIO "loadLatexFrom" $ conv modelString

loadGromet :: DataSource -> IO String
loadGromet source = 
  do  model <- loadModel (GROMET Concrete) source
      (code, _out, _err) <- readProcessWithExitCode "jq" [] model
      case code of
        ExitSuccess -> pure model
        ExitFailure _ -> throwIO $ ParseError "invalid gromet"

loadMetaESL :: DataSource -> IO ESL.ModelMeta
loadMetaESL source =
  do  modelString <- loadModel (ESLMETA Concrete) source
      parse "loadMetaESL" modelString

-------------------------------------------------------------------------------

checkModel :: ModelType -> DataSource -> IO (Maybe String)
checkModel format source =
  do  result <- try
        case format of
          ESL Concrete -> void $ loadESL source
          ESLMETA Concrete -> void $ loadMetaESL source
          DEQ Concrete -> void $ loadDiffEqs mempty mempty source
          RNET Concrete -> void $ loadReactions source
          LATEX Concrete -> void $ loadLatex source
          GROMET Concrete -> void $ loadGromet source
          _ -> throwIO (NotImplementedError "")
      case result of
        Left err -> pure $ Just (show (err :: SomeException))
        Right _ -> pure Nothing

-------------------------------------------------------------------------------
-- Loaders for "second class" models and other entities

loadCoreFrom' :: ModelType -> DataSource -> Map Text Double -> IO Core.Model
loadCoreFrom' format source parameters =
  do  model <- loadESLFrom format source
      let psExpr = Map.map Core.NumLit parameters'
          parameters' = parameters `Map.union` ESL.parameterMap model
      case modelAsCore model of
        Left err -> throwIO (ValidationError err)
        Right a  -> pure $ Core.applyParams psExpr a

loadCoreFrom :: ModelType -> DataSource -> IO Core.Model
loadCoreFrom format source = loadCoreFrom' format source Map.empty

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

convertModelString :: ModelType -> DataSource -> ModelType -> IO (Either String String)
convertModelString srcTy src destTy =
  case destTy of
    ESL Concrete -> serializeModel <$> loadESLFrom srcTy src
    DEQ Concrete -> serializeModel <$> loadDiffEqsFrom srcTy mempty mempty src
    -- RNET Concrete -> serializeModel <$> loadReactionsFrom srcTy src
    LATEX Concrete -> serializeModel <$> loadLatexFrom srcTy src


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

describeModelInterface :: ModelType -> DataSource -> IO Value
describeModelInterface modelType modelSource = undefined
  case modelType of
    ESL _ ->
      do  mdl <- loadMetaESL modelSource
          let stateVars =
                [(n, Meta.metaMap md) | md <- ESL.modelMetaDecls mdl
                                      , (ESL.State n _) <- [Meta.metaValue md]
                                      ]
              params =
                [(n, d, Meta.metaMap md) | md <- ESL.modelMetaDecls mdl
                                          , (ESL.Parameter n d) <- [Meta.metaValue md]
                                          ]

              descParam (n, d, mp) =
                object [ "name" .= n
                          , "defaultValue" .= d
                          , "metadata" .= mp
                          ]
              descState (n, mp) =
                object [ "name" .= n
                          , "metadata" .= mp
                          ]
              desc =
                object [ "parameters" .= (descParam <$> params)
                          , "stateVars"  .= (descState <$> stateVars)
                          ]
          pure desc
    _ -> undefined

simulateModel :: 
  ModelType -> 
  DataSource -> 
  Double -> 
  Double ->
  Double ->
  Map Text Double ->
  IO (DataSeries Double)
simulateModel modelType modelSource start end step overwrite =
  do  eqs <- 
        loadDiffEqsFrom 
          modelType
          overwrite
          []
          modelSource
      let times = takeWhile (<= end)
                   $ iterate (+ step) start
      pure $ ODE.simulate eqs Map.empty times

fitModelToData ::
  ModelType {- ^ the model's type -}-> 
  DataSource {- ^ the data as ASKEE-produced CSV -} ->
  [Text] {- ^ parameters to fit -} -> 
  DataSource {- ^ the model -} -> 
  IO (Map Core.Ident (Double, Double))
fitModelToData modelFormat fitData fitParams modelSource = 
  do  eqs <- 
        loadDiffEqsFrom
          modelFormat 
          Map.empty
          fitParams
          modelSource
      rawData <- 
        case fitData of
          Inline s -> pure s
          FromFile _ -> throwIO (NotImplementedError "reading fit data from file")
      dataSeries <- 
        case parseDataSeries (B.pack $ Text.unpack rawData) of
          Right d -> pure d
          Left err -> throwIO (MalformedDataSeries err)
      let (res, _) = ODE.fitModel eqs dataSeries Map.empty (Map.fromList (zip fitParams (repeat 0)))
      pure res

asSchematicGraph :: Core.Model -> Maybe Viz.Graph
asSchematicGraph g =  Viz.Graph <$> sequence effs
  where
    effs =
      [ mbEdge | evt <- Core.modelEvents g
               , (var, expr) <- Map.toList $ Core.eventEffect evt
               , let mbEdge = effectEdge evt var expr ]

    eventNode evt = Viz.Node (Core.eventName evt) Viz.Event
    stateNode name = Viz.Node name Viz.State
    effectEdge evt var e0 =
      case e0 of
        Core.Var v Core.:+: Core.NumLit n | n > 0, v == var ->
          Just (eventNode evt, stateNode var)
        Core.Var v Core.:-: Core.NumLit n | n > 0, v == var ->
          Just (stateNode var, eventNode evt)
        _ ->
          Nothing