{-# Language PatternSynonyms, ApplicativeDo, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Core ( asSchematicGraph ) where

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

parameterize :: Map Text Double -> Core.Model -> Core.Model
parameterize parameters = Core.applyParams parameters'
  where
    parameters' = Map.map Core.NumLit parameters
  

      -- pure $ Core.applyParams psExpr core

-- loadCoreFrom :: ModelType -> DataSource -> IO Core.Model
-- loadCoreFrom format source = loadCoreFrom' 

