{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.ASKEE.DEQ.Syntax where

import Data.Map ( Map )

import Language.ASKEE.Core.Syntax (TraverseExprs, traverseExprs, Ident, Expr(..))

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
