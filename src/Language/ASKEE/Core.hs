{-# Language PatternSynonyms, ApplicativeDo, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Core
  ( loadCore
  , loadCoreFrom
  , asSchematicGraph
  ) 
  where

import Data.Text(Text)

import Data.Map(Map)
import qualified Data.Map as Map

import Language.ASKEE.ModelType
import qualified Language.ASKEE.Core.Syntax as Core
import Language.ASKEE.Core.Visualization ( asSchematicGraph )
import qualified Language.ASKEE.ESL as ESL
import qualified Language.ASKEE.ESL.Syntax as ESL
import Language.ASKEE.ESL.Convert (modelAsCore)
import Language.ASKEE.Error (throwLeft, ASKEEError (ValidationError))
import Language.ASKEE.Storage ( DataSource )

loadCoreFrom' :: ModelType -> DataSource -> Map Text Double -> IO Core.Model
loadCoreFrom' format source parameters =
  do  model <- ESL.loadESLFrom format source
      let psExpr = Map.map Core.NumLit parameters'
          parameters' = parameters `Map.union` ESL.parameterMap model
      core <- throwLeft ValidationError (modelAsCore model)
      pure $ Core.applyParams psExpr core

loadCoreFrom :: ModelType -> DataSource -> IO Core.Model
loadCoreFrom format source = loadCoreFrom' format source Map.empty

loadCore :: DataSource -> IO Core.Model
loadCore = loadCoreFrom EaselType