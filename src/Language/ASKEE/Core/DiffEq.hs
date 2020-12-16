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
ppDiffEqs DiffEqs{..} = vcat [params, lets, initials, states, lets]
  where
    params = "Parameters:" PP.<+> vcat (punctuate "," (map (text . unpack) deqParams))
    initials = 
      "Initial conditions:" PP.<+> 
        vcat 
          (map (\(i, e) -> text (unpack i) PP.<> ": " PP.<> ppDiffEqExpr e) (Map.toList deqInitial))
    states = 
      "State equations:" PP.<+> 
        vcat 
          (map (\(i, e) -> text (unpack i) PP.<> " = " PP.<> ppDiffEqExpr e) (Map.toList deqState))
    lets = 
      "Let bindings:" PP.<+> 
        vcat 
          (map (\(i, e) -> text (unpack i) PP.<> " = " PP.<> ppDiffEqExpr e) (Map.toList deqLet))


ppDiffEqExpr :: Expr -> Doc
ppDiffEqExpr e =
  case simplifyExpr e of
    NumLit d -> double d
    Op1 Neg e' -> "-" PP.<> ppDiffEqExpr e'
    e1 :+: e2 -> hcat $ punctuate "+" [ppDiffEqExpr e1, ppDiffEqExpr e2]
    e1 :-: e2 -> hcat $ punctuate "-" [ppDiffEqExpr e1, ppDiffEqExpr e2]
    e1 :*: e2 -> hcat $ punctuate "*" [ppDiffEqExpr e1, ppDiffEqExpr e2]
    Op2 Div e1 e2 -> hcat $ punctuate "/" [ppDiffEqExpr e1, ppDiffEqExpr e2]
    Var i -> text $ unpack i
    _ -> error $ "printing not yet supported for "Prelude.<>show e


