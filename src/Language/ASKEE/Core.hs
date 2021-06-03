{-# Language PatternSynonyms, ApplicativeDo, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Core
  ( loadCore
  , loadCoreFrom
  , loadCoreFrom'
  , asSchematicGraph

  , Core.Model
  ) 
  where

import           Data.Map  ( Map )
import qualified Data.Map  as Map
import           Data.Text ( Text )

import qualified Language.ASKEE.Core.Syntax        as Core
import           Language.ASKEE.Core.Visualization ( asSchematicGraph )
import           Language.ASKEE.Error              ( throwLeft
                                                   , ASKEEError(ValidationError) )
import qualified Language.ASKEE.ESL                as ESL
import           Language.ASKEE.ESL.Convert        ( modelAsCore )
import qualified Language.ASKEE.ESL.Syntax         as ESL
import           Language.ASKEE.ModelType          ( ModelType(EaselType) )
import           Language.ASKEE.Storage            ( DataSource )

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