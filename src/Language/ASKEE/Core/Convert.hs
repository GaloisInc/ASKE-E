{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Language.ASKEE.Core.Convert where

import qualified Data.Map      as Map
import           Data.Maybe    ( mapMaybe )

import           Language.ASKEE.Core.Expr
import qualified Language.ASKEE.Core.Expr         as Core
import           Language.ASKEE.Core.Syntax
import           Language.ASKEE.DEQ.Syntax        ( DiffEqs(..) )

-------------------------------------------------------------------------------
-- DiffEqs

data ToDiffEqMethod = WithGuards | NoGuards

asDiffEqs :: ToDiffEqMethod -> Model -> DiffEqs
asDiffEqs how mdl =
  DiffEqs { deqParams  = modelParams mdl
          , deqInitial = modelInitState mdl
          , deqRates   = Map.mapWithKey stateEq (modelInitState mdl)
          , deqLets    = modelLets mdl
          }
  where
  stateEq sv _ = simplifyExpr
               $ foldr (:+:) (NumLit 0)
               $ mapMaybe (eventTerm how sv)
               $ modelEvents mdl

-- Eventually we may want to consider approaches to try to make guard
-- continues (e.g., sigmoid?)
eventTerm :: ToDiffEqMethod -> Core.Ident -> Event -> Maybe Expr
eventTerm how sv event =
  do stExp <- Map.lookup sv (eventEffect event)
     let e = eventRate event :*: (stExp :-: Var sv)
     pure case how of
            WithGuards -> If (eventWhen event) e (NumLit 0)
            NoGuards   -> e

