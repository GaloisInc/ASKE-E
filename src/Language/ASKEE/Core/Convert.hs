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

asDiffEqs :: Model -> DiffEqs
asDiffEqs mdl =
  DiffEqs { deqParams  = Map.keys (modelParams mdl)
          , deqInitial = modelInitState mdl
          , deqRates   = Map.mapWithKey stateEq (modelInitState mdl)
          , deqLets    = modelLets mdl
          }
  where
  stateEq sv _ = simplifyExpr
               $ foldr (:+:) (NumLit 0)
               $ mapMaybe (eventTerm sv)
               $ modelEvents mdl

-- Eventually we may want to consider approaches to try to make guard
-- continues (e.g., sigmoid?)
eventTerm :: Core.Ident -> Event -> Maybe Expr
eventTerm sv event =
  do stExp <- Map.lookup sv (eventEffect event)
     pure (If (eventWhen event)
              (eventRate event :*: (stExp :-: Var sv))
              (NumLit 0))

