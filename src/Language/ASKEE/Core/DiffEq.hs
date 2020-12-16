{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Convert a core model to a system of differentila equations
module Language.ASKEE.Core.DiffEq where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)
import Data.Text (unpack)

import Language.ASKEE.Core
import Language.ASKEE.Core.Simplify(simplifyExpr)

import Text.PrettyPrint as PP

-- | A sysmte of differential equations.
-- The `let` equations are already inlined
data DiffEqs = DiffEqs
  { deqParams  :: [Ident]
  , deqInitial :: Map Ident Expr
  , deqState   :: Map Ident Expr      -- ^ These are the diff. eqns.
  , deqLet     :: Map Ident Expr
  }
  deriving Show

asEquationSystem :: Model -> DiffEqs
asEquationSystem mdl =
  DiffEqs { deqParams  = modelParams mdl
          , deqInitial = modelInitState mdl
          , deqState   = Map.mapWithKey stateEq (modelInitState mdl)
          , deqLet     = modelLets mdl
          }
  where
  stateEq sv _ = simplifyExpr
               $ foldr (:+:) (NumLit 0)
               $ mapMaybe (eventTerm sv)
               $ modelEvents mdl


-- Eventually we may want to consider approaches to try to make guard
-- continues (e.g., sigmoid?)
eventTerm :: Ident -> Event -> Maybe Expr
eventTerm sv event =
  do stExp <- Map.lookup sv (eventEffect event)
     pure (If (eventWhen event)
              (eventRate event :*: (stExp :-: Var sv))
              (NumLit 0))


ppDiffEqs :: DiffEqs -> Doc
ppDiffEqs DiffEqs{..} = vcat [lets, initials, states]
  where
    initials = vcat $ map (binding "state") (Map.toList deqInitial)
    states   = vcat $ map (binding "rate") (Map.toList deqState)
    lets     = vcat $ map (binding "let") (Map.toList deqLet)

    binding decl (i,e) = hsep [decl, text (unpack i), "=", ppExpr e]