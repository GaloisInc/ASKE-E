-- TODO: support enabling predicates
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
module Language.ASKEE.DiffEq.DiffEq where

import Data.Text(Text)
import Control.Monad.Fail(MonadFail)
import Data.List(partition)
import Data.Maybe(catMaybes)
import qualified Text.PrettyPrint as Pretty
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Text as Text

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

-- PP -------------------------------------------------------------------------

ppDiffEq :: DiffEq -> Pretty.Doc
ppDiffEq deq = Pretty.hsep [ intro, Pretty.text "=", expr ]
  where
    expr = 
      ppEqExp $ case deq of
                  StateEq var e -> e
                  VarEq var e -> e
    intro = 
      case deq of
        StateEq var e -> Pretty.text ("d" <> Text.unpack var <> "/dt")
        VarEq var e   -> Pretty.text (Text.unpack var)

ppEqExp :: EqExp -> Pretty.Doc
ppEqExp e =
  case e of
    Var t -> Pretty.text (Text.unpack t)
    Lit d -> Pretty.double d
    Add p1 p2 -> binop "+" p1 p2 
    Sub p1 p2 -> binop "-" p1 p2
    Mul p1 p2 -> binop "*" p1 p2
    Div p1 p2 -> binop "/" p1 p2
    Neg p1 -> undefined
      
  where
    (litP, negP, mulP, addP) = (0,1,2,3)
    plvl pe =
     case pe of
       Var _ -> litP
       Lit _ -> litP
       Add _ _ -> addP
       Sub _ _ -> addP
       Mul _ _ -> mulP
       Div _ _ -> mulP
       Neg _ -> negP

    ppPrec p =
      if plvl p > plvl e
        then Pretty.parens (ppEqExp p)
        else ppEqExp p

    binop op p1 p2 = Pretty.hsep [ppPrec p1, Pretty.text "+", ppEqExp p2]

-- evaluator ------------------------------------------------------------------

eval :: Map Text Double -> EqExp -> Double
eval env e =
  case e of
    Var v ->
      case Map.lookup v env of
        Just a -> a
        Nothing -> error ("Unbound variable " ++ (show v))
    Lit l -> l
    Add e1 e2 -> eval env e1 + eval env e2
    Sub e1 e2 -> eval env e1 - eval env e2
    Mul e1 e2 -> eval env e1 * eval env e2
    Div e1 e2 -> eval env e1 / eval env e2
    Neg e1 -> negate $ eval env e1

