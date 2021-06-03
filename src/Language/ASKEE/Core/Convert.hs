module Language.ASKEE.Core.Convert where

import           Data.Map   ( Map )
import qualified Data.Map   as Map
import           Data.Maybe ( mapMaybe )
import qualified Data.Set   as Set

import Language.ASKEE.Core.Syntax
import Language.ASKEE.Core.Simplify ( simplifyExpr )
import Language.ASKEE.DEQ.Syntax    ( DiffEqs(..) )

asEquationSystem :: Model -> DiffEqs
asEquationSystem mdl =
  DiffEqs { deqParams  = modelParams mdl
          , deqInitial = modelInitState mdl
          , deqRates   = Map.mapWithKey stateEq (modelInitState mdl)
          , deqLets    = modelLets mdl
          }
  where
  stateEq sv _ = simplifyExpr
               $ foldr (:+:) (NumLit 0)
               $ mapMaybe (eventTerm sv)
               $ modelEvents mdl

applyParams :: Map Ident Expr -> DiffEqs -> DiffEqs
applyParams su = dropParams . mapExprs (substExpr su)
  where
  dropParams m = m { deqParams = [ x | x <- deqParams m
                                 , not (x `Set.member` pSet) ] }
  pSet = Map.keysSet su

-- Eventually we may want to consider approaches to try to make guard
-- continues (e.g., sigmoid?)
eventTerm :: Ident -> Event -> Maybe Expr
eventTerm sv event =
  do stExp <- Map.lookup sv (eventEffect event)
     pure (If (eventWhen event)
              (eventRate event :*: (stExp :-: Var sv))
              (NumLit 0))

