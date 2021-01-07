{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.ASKEE.DEQ.Syntax where

import           Data.Text  ( unpack )
import           Data.Map   ( Map )
import qualified Data.Map   as Map

import Language.ASKEE.Core (TraverseExprs, traverseExprs, Ident, Expr(..), ppExpr)

import Text.PrettyPrint ( Doc, hsep, text, vcat )

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

ppDiffEqs :: DiffEqs -> Doc
ppDiffEqs DiffEqs{..} = vcat [lets, initials, rates]
  where
    lets     = vcat $ map (binding "let") (Map.toList deqLets)
    initials = vcat $ map (binding "var") (Map.toList deqInitial)
    rates    = vcat $ map (binding "d/dt") (Map.toList deqRates)

    binding decl (i,e) = hsep [decl, text (unpack i), "=", ppExpr e]