-- TODO: support enabling predicates
{-# Language FlexibleInstances #-}
module Language.ASKEE.DiffEq where

import Data.Text(Text)
import Control.Monad.Fail(MonadFail)
import Data.List(partition)
import Data.Maybe(catMaybes)

import qualified Language.ASKEE.Syntax as Syntax

type EqGen a = Either String a
instance MonadFail (Either String) where
  fail = Left

data DiffEq =
    StateEq Text EqExp  -- dx/dt = exp
  | VarEq   Text EqExp  -- x = exp 
  deriving(Show, Eq, Ord)

data EqExp =
    Var Text          
  | Lit Double
  | Add EqExp EqExp
  | Sub EqExp EqExp
  | Mul EqExp EqExp
  | Div EqExp EqExp
  | Neg EqExp
  deriving(Show, Eq, Ord)

-- algebraic constructors -----------------------------------------------------

negExp :: EqExp -> EqExp
negExp (Neg e0) = e0
negExp e0 = Neg e0

addExp :: EqExp -> EqExp -> EqExp
addExp (Lit 0.0) e = e
addExp e (Lit 0.0) = e
addExp (Lit d1) (Lit d2) = Lit (d1 + d2) 
addExp e1 e2 = Add e1 e2

subExp :: EqExp -> EqExp -> EqExp
subExp e (Lit 0.0) = e
subExp (Lit 0.0) e = negExp e
subExp (Lit d1) (Lit d2) = Lit (d1 - d2)
subExp e1 e2 = Sub e1 e2

mulExp :: EqExp -> EqExp -> EqExp
mulExp (Lit 0.0) _ = Lit 0.0
mulExp _ (Lit 0.0) = Lit 0.0
mulExp (Lit 1.0) e = e
mulExp e (Lit 1.0) = e
mulExp (Lit d1) (Lit d2) = Lit (d1 * d2)
mulExp e1 e2 = Mul e1 e2

divExp :: EqExp -> EqExp -> EqExp
divExp (Lit d1) (Lit d2) = Lit (d1/d2)
divExp e (Lit 1.0) = e
divExp e1 e2 = Div e1 e2

termList :: EqExp -> [EqExp]
termList e =
  case e of
    Add e1 e2 -> termList e1 ++ termList e2
    Sub e1 e2 -> termList e1 ++ (negExp <$> termList e2)
    _ -> [e]

sumTerms :: [EqExp] -> EqExp
sumTerms = foldr addExp (Lit 0.0) 

-- 'interpreter' --------------------------------------------------------------

asEquationSystem :: Syntax.Model -> EqGen [DiffEq]
asEquationSystem mdl =
  do  declEqs <- declEq `traverse` (Syntax.modelDecls mdl)
      stateEqs <- stateEq `traverse` stateVars
      pure $ declEqs ++ stateEqs

  where
    asMbStateVar decl =
      case decl of
        Syntax.State n v -> Just (n, v)
        Syntax.Let _ _ -> Nothing

    declEq decl =
      do  declExp' <- asEqExp (Syntax.val decl)
          pure $ VarEq (Syntax.name decl) declExp' 

    stateVars = 
      catMaybes (asMbStateVar <$> Syntax.modelDecls mdl)

    stateEq (sv, _) = StateEq sv . sumTerms <$> eventTerms sv

    eventTerms sv =
      catMaybes <$> (eventTerm sv `traverse` (Syntax.modelEvents mdl))

    eventTerm sv event = 
      case sv `lookup` Syntax.eventEffect event of
        Nothing -> pure Nothing
        Just stExp -> 
          do  rate' <- asEqExp (Syntax.eventRate event)
              stExp' <- asEqExp stExp
              Just <$> stateEventTerm stExp' sv rate'

stateEventTerm :: EqExp -> Text -> EqExp -> EqGen EqExp
stateEventTerm e0 stateVar rateExp =
    case partition isStateVar (termList e0) of
      ([_], deltaTerms) -> pure (sumTerms deltaTerms)
      _ -> fail ("not sure how to express state term of exp" ++ show e0)
  where
    isStateVar e = Var stateVar == e 

asEqExp :: Syntax.Exp -> EqGen EqExp
asEqExp e =
  case e of
    Syntax.Add e1 e2 -> binop addExp e1 e2
    Syntax.Sub e1 e2 -> binop subExp e1 e2
    Syntax.Mul e1 e2 -> binop mulExp e1 e2
    Syntax.Div e1 e2 -> binop divExp e1 e2
    Syntax.Neg en -> negExp <$> asEqExp en
    Syntax.Var v -> pure $ Var v
    Syntax.Real r -> pure $ Lit r
    _ -> expNotImplemented e
  where
    binop op e1 e2 = op <$> asEqExp e1 <*> asEqExp e2
    expNotImplemented nexp = fail ("Expression form not implemented: " ++ show nexp)

