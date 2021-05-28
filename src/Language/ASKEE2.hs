{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE2 where

import Control.Exception (throwIO)

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Text (Text)

import           Language.ASKEE.Check        ( checkModel )
import           Language.ASKEE.Convert      ( converter )
import qualified Language.ASKEE.Core         as Core
import           Language.ASKEE.Core.DiffEq  ( applyParams )
import qualified Language.ASKEE.DEQ.Syntax   as DEQ
import qualified Language.ASKEE.Latex.Syntax as Latex
import qualified Language.ASKEE.RNet.Syntax  as RNet
import           Language.ASKEE.SimulatorGen ( genModel )
import qualified Language.ASKEE.Syntax       as ESL
import           Language.ASKEE.Storage      ( loadModel )
import qualified Language.ASKEE.ModelStratify.Stratify     as Stratify
import           Language.ASKEE.Translate    ( ParseModel, parseModel )
import           Language.ASKEE.Types        ( Representation(..)
                                             , ModelType(..)
                                             , DataSource(..)
                                             , ParseError(..)
                                             , ValidationError(..)
                                             , ConversionError(..), StratificationInfo, StratificationType )
import Language.ASKEE.Core.ImportASKEE (modelAsCore)
import Data.Aeson (Value, decode, object, (.=))
import qualified Language.ASKEE.ModelStratify.GeoGraph as GG
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Language.ASKEE.Metadata as Meta
import Language.ASKEE.DataSeries ( DataSeries )
import qualified Language.ASKEE.Core.GSLODE as ODE
import qualified Language.ASKEE.Core.Visualization as Viz

parse :: ParseModel a => String -> String -> IO a
parse why modelString = 
  case parseModel modelString of
    Left err -> throwIO (ParseError $ why <> err)
    Right m -> pure m

foo :: String -> Either String a -> IO a
foo why modelE =
  case modelE of
    Left err -> throwIO (ParseError $ why <> " " <> err)
    Right m -> pure m 

-------------------------------------------------------------------------------
-- Loaders for "first class" models

loadESL :: DataSource -> IO ESL.Model
loadESL = loadESLFrom (ESL Concrete)
-- loadESL source = 
--   do  modelString <- loadModel (ESL Concrete) source
--       model <- parse "loadESL" modelString
--       case checkModel model of
--         Left err -> throwIO (ValidationError err)
--         Right m -> pure m

loadESLFrom :: ModelType -> DataSource -> IO ESL.Model
loadESLFrom format source =
  do  modelString <- loadModel format source
      let conv =
            case format of
              ESL Concrete -> $(converter (ESL Concrete) (ESL Abstract))
              RNET Concrete -> $(converter (RNET Concrete) (ESL Abstract))
      model <- foo "loadESLFrom" $ conv modelString
      case checkModel model of
        Left err -> throwIO (ValidationError err)
        Right m -> pure m

loadDiffEqs :: Map Text Double -> [Text] -> DataSource -> IO DEQ.DiffEqs
loadDiffEqs = loadDiffEqsFrom (DEQ Concrete)
-- loadDiffEqs overwrite params source = 
--   do  modelString <- loadModel (DEQ Concrete) source
--       eqns <- parse "loadDiffEqs" modelString
--       let eqns' = eqns { DEQ.deqParams = params }
--       let replaceParams = applyParams (Map.map Core.NumLit overwrite)
--       pure $ replaceParams eqns'

loadDiffEqsFrom :: 
  ModelType ->
  Map Text Double -> 
  [Text] -> 
  DataSource -> 
  IO DEQ.DiffEqs
loadDiffEqsFrom format overwrite params source = 
  do  modelString <- loadModel (DEQ Concrete) source
      let conv =
            case format of
              ESL Concrete   -> $(converter (ESL Concrete) (DEQ Abstract))
              DEQ Concrete   -> $(converter (DEQ Concrete) (DEQ Abstract))
              RNET Concrete  -> $(converter (RNET Concrete) (DEQ Abstract))
              LATEX Concrete -> $(converter (LATEX Concrete) (DEQ Abstract))
      eqns <- foo "loadDiffEqsFrom" $ conv modelString
      -- eqns <- parse "loadDiffEqs" modelString
      let eqns' = eqns { DEQ.deqParams = params }
      let replaceParams = applyParams (Map.map Core.NumLit overwrite)
      pure $ replaceParams eqns'

loadReactions :: DataSource -> IO RNet.ReactionNet
loadReactions source =
  do  modelString <- loadModel (RNET Concrete) source
      parse "loadReactions" modelString

loadLatex :: DataSource -> IO Latex.Latex
loadLatex source =
  do  modelString <- loadModel (LATEX Concrete) source
      parse "loadLatex" modelString

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

-------------------------------------------------------------------------------
-- Manipulators for models

renderCppModel :: Core.Model -> String
renderCppModel = show . genModel
  -- do  compiled <- loadCore' file
  --     pure $ show (genModel compiled)

-------------------------------------------------------------------------------

convertConcrete :: ModelType -> DataSource -> ModelType -> IO (Either String String)
convertConcrete inputType source outputType =
  do  model <- loadModel inputType source
      conv <- 
        case inputType of
          ESL Concrete ->
            case outputType of
              ESL Concrete    -> pure $(converter (ESL Concrete) (ESL Concrete))
              DEQ Concrete    -> pure $(converter (ESL Concrete) (DEQ Concrete))
              RNET Concrete   -> pure $(converter (ESL Concrete) (LATEX Concrete))
              LATEX Concrete  -> pure $(converter (ESL Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (ESL Concrete) (GROMET Concrete))
              TOPO Concrete   -> pure $(converter (ESL Concrete) (TOPO Concrete))
              _ -> die (show outputType)
          DEQ Concrete ->
            case outputType of
              -- ESL Concrete    -> pure $(converter (DEQ Concrete) (ESL Concrete))
              DEQ Concrete    -> pure $(converter (DEQ Concrete) (DEQ Concrete))
              -- RNET Concrete   -> pure $(converter (DEQ Concrete) (RNET Concrete))
              LATEX Concrete  -> pure $(converter (DEQ Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (DEQ Concrete) (GROMET Concrete))
              -- TOPO Concrete   -> pure $(converter (DEQ Concrete) (TOPO Concrete))
              _ -> die (show outputType)
          RNET Concrete ->
            case outputType of
              ESL Concrete    -> pure $(converter (RNET Concrete) (ESL Concrete))
              DEQ Concrete    -> pure $(converter (RNET Concrete) (DEQ Concrete))
              RNET Concrete   -> pure $(converter (RNET Concrete) (RNET Concrete))
              LATEX Concrete  -> pure $(converter (RNET Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (RNET Concrete) (GROMET Concrete))
              TOPO Concrete   -> pure $(converter (RNET Concrete) (TOPO Concrete))
              _ -> die (show outputType)
          LATEX Concrete ->
            case outputType of
              -- ESL Concrete    -> pure $(converter (LATEX Concrete) (ESL Concrete))
              DEQ Concrete    -> pure $(converter (LATEX Concrete) (DEQ Concrete))
              RNET Concrete   -> pure $(converter (LATEX Concrete) (LATEX Concrete))
              LATEX Concrete  -> pure $(converter (LATEX Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (LATEX Concrete) (GROMET Concrete))
              -- TOPO Concrete   -> pure $(converter (LATEX Concrete) (TOPO Concrete))
              _ -> die (show outputType)
          GROMET Concrete ->
            case outputType of
              -- ESL Concrete    -> pure $(converter (GROMET Concrete) (ESL Concrete))
              -- DEQ Concrete    -> pure $(converter (GROMET Concrete) (DEQ Concrete))
              -- RNET Concrete   -> pure $(converter (GROMET Concrete) (LATEX Concrete))
              -- LATEX Concrete  -> pure $(converter (GROMET Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (GROMET Concrete) (GROMET Concrete))
              -- TOPO Concrete   -> pure $(converter (GROMET Concrete) (TOPO Concrete))
              _ -> die (show outputType)
          TOPO Concrete ->
            case outputType of
              ESL Concrete    -> pure $(converter (TOPO Concrete) (ESL Concrete))
              DEQ Concrete    -> pure $(converter (TOPO Concrete) (DEQ Concrete))
              RNET Concrete   -> pure $(converter (TOPO Concrete) (LATEX Concrete))
              LATEX Concrete  -> pure $(converter (TOPO Concrete) (LATEX Concrete))
              -- GROMET Concrete -> pure $(converter (TOPO Concrete) (GROMET Concrete))
              TOPO Concrete   -> pure $(converter (TOPO Concrete) (TOPO Concrete))
              _ -> die (show outputType)
          _ -> die (show inputType)
      pure $ conv model
  where
    die s = throwIO (ConversionError $ "convertConcrete: "<>s)

stratifyModel ::
  DataSource ->
  String ->
  Maybe String ->
  StratificationType ->
  IO StratificationInfo
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