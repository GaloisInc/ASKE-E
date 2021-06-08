{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.ASKEE.DEQ.Syntax where

import           Data.Map  ( Map )
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           Data.Text (Text)

import           Language.ASKEE.Core.Expr ( traverseExprs
                                          , mapExprs
                                          , substExpr
                                          , TraverseExprs
                                          , Ident
                                          , Expr(..))
import qualified Language.ASKEE.Core.Expr as CoreExpr

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

applyParams :: Map Text Double -> DiffEqs -> DiffEqs
applyParams parameters = applyParams' parameters'
  where
    parameters' = Map.map CoreExpr.NumLit parameters

-- XXX may want to check and/or panic if we try to overwrite something
-- that isn't a parameter
applyParams' :: Map Ident Expr -> DiffEqs -> DiffEqs
applyParams' su = dropParams . mapExprs (substExpr su)
  where
  dropParams m = m { deqParams = [ x | x <- deqParams m
                                     , not (x `Set.member` pSet) ] }
  pSet = Map.keysSet su

addParams :: [Ident] -> DiffEqs -> DiffEqs
addParams ps eqs = eqs { deqParams = ps }