-- | Convert a core model to a system of differentila equations
module Language.ASKEE.Core.DiffEq where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)

import Language.ASKEE.Core
import Language.ASKEE.Core.Simplify(simplifyExpr)

-- | A sysmte of differential equations.
-- The `let` equations are already inlined
data DiffEqs = DiffEqs
  { deqInitial :: Map Ident Double
  , deqState   :: Map Ident Expr      -- ^ These are the diff. eqns.
  , deqLet     :: Map Ident Expr
  }

asEquationSystem :: Model -> DiffEqs
asEquationSystem mdl =
  DiffEqs { deqInitial = modelInitState mdl
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


