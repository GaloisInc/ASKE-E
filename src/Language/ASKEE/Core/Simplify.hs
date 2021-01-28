module Language.ASKEE.Core.Simplify where

import Language.ASKEE.Core

simplifyExpr :: Expr -> Expr
simplifyExpr expr =
  case mapExprs simplifyExpr expr of
    If (BoolLit b) e1 e2          -> if b then e1 else e2

    -- Numerics
    Op1 Neg (Op1 Neg e)           -> e
    Op1 Neg (NumLit x)            -> NumLit (negate x)

    Op2 Add (NumLit 0) e          -> e
    Op2 Add e (NumLit 0)          -> e
    Op2 Add (NumLit x) (NumLit y) -> NumLit (x + y)

    Op2 Sub (NumLit 0) e          -> simplifyExpr (Op1 Neg e)
    Op2 Sub e (NumLit 0)          -> e
    Op2 Sub (NumLit x) (NumLit y) -> NumLit (x - y)

    Op2 Mul (NumLit 0) _          -> NumLit 0
    Op2 Mul _ (NumLit 0)          -> NumLit 0
    Op2 Mul (NumLit 1) e          -> e
    Op2 Mul e (NumLit 1)          -> e
    Op2 Mul (NumLit x) (NumLit y) -> NumLit (x * y)

    Op2 Div e (NumLit 1)          -> e
    Op2 Div (NumLit x) (NumLit y) -> NumLit (x / y)


    -- Booleans
    Op1 Not (Op1 Not e)           -> e
    Op1 Not (BoolLit x)           -> BoolLit (not x)

    Op2 Lt  (NumLit x) (NumLit y) -> BoolLit (x < y)
    Op2 Leq (NumLit x) (NumLit y) -> BoolLit (x <= y)
    Op2 Eq  (NumLit x) (NumLit y) -> BoolLit (x == y)

    Op2 And (BoolLit False) _     -> BoolLit False
    Op2 And _ (BoolLit False)     -> BoolLit False
    Op2 And (BoolLit True) e      -> e
    Op2 And e (BoolLit True)      -> e

    Op2 Or (BoolLit False) e      -> e
    Op2 Or e (BoolLit False)      -> e
    Op2 Or (BoolLit True) _       -> BoolLit True
    Op2 Or _ (BoolLit True)       -> BoolLit True

    e                             -> e


-- | Return the list of terms of a linear expression, negating them if they are subtracted
exprToTermList :: Expr -> [Expr]
exprToTermList e =
  case e of
    Op2 Add e1 e2 -> exprToTermList e1 ++ exprToTermList e2
    Op2 Sub e1 e2 -> exprToTermList e1 ++ (simplifyExpr . Op1 Neg <$> exprToTermList e2)
    _ -> [e]

-- | turn a term list back into a linear expression
termListToExpr :: [Expr] -> Expr
termListToExpr = foldr mkTerm (NumLit 0.0)
  where
    mkTerm e1 (Op1 Neg e2) = Op2 Sub e1 e2
    mkTerm e1 e2 = Op2 Add e1 e2



