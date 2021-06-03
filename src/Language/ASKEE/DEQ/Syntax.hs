{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.ASKEE.DEQ.Syntax where

import           Data.Map  ( Map )
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           Data.Text (Text)

import           Language.ASKEE.Core.Syntax (TraverseExprs, traverseExprs, Ident, Expr(..))
import qualified Language.ASKEE.Core.Syntax as Core

-- | A system of differential equations.
data DiffEqs = DiffEqs
  { deqParams  :: [Ident]
  , deqInitial :: Map Ident Expr
  , deqRates   :: Map Ident Expr      -- ^ These are the diff. eqns.
  , deqLets    :: Map Ident Expr
  }
  deriving Show

instance TraverseExprs DiffEqs where
  traverseExprs f DiffEqs { .. } =
    do deqInitial <- traverse f deqInitial
       deqRates   <- traverse f deqRates
       deqLets     <- traverse f deqLets
       pure DiffEqs { .. }

overwriteParameters :: Map Text Double -> DiffEqs -> DiffEqs
overwriteParameters parameters = applyParams parameters'
  where
    parameters' = Map.map Core.NumLit parameters

applyParams :: Map Ident Expr -> DiffEqs -> DiffEqs
applyParams su = dropParams . Core.mapExprs (Core.substExpr su)
  where
  dropParams m = m { deqParams = [ x | x <- deqParams m
                                     , not (x `Set.member` pSet) ] }
  pSet = Map.keysSet su