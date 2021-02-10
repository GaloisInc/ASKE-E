module Language.ASKEE.Core.Simplify where

import Data.List(foldl')
import Data.Map(Map)
import qualified Data.Map as Map

import Language.ASKEE.Core


simplifyExpr :: Expr -> Expr
simplifyExpr expr =
  case mapExprs (toFrom . simplifyExpr . toFrom) expr of
    If (BoolLit b) e1 e2          -> if b then e1 else e2

    -- Numerics
    Op1 Neg (Op1 Neg e)           -> e
    Op1 Neg (NumLit x)            -> NumLit (negate x)
    Op1 Neg (Op2 Div x y)         -> simplifyExpr (Op2 Div (Op1 Neg x) y)
    Op1 Neg (Op2 Mul x y)         -> simplifyExpr (Op2 Mul (Op1 Neg x) y)

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

    Op2 Mul e (NumLit (-1))       -> simplifyExpr (Op1 Neg e)
    Op2 Mul (NumLit (-1)) e       -> simplifyExpr (Op1 Neg e)
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


--------------------------------------------------------------------------------

toFrom :: Expr -> Expr
toFrom = fromSum . toSum

data Sum = Sum Double (Map Expr Double)   -- a * x + b * y + c

add :: Sum -> Sum -> Sum
add (Sum a xs) (Sum b ys) = Sum (a+b) (Map.unionWith (+) xs ys)

mulK :: Double -> Sum -> Sum
mulK x (Sum a xs) = Sum (x * a) ((* x) <$> xs)

atom :: Expr -> Sum
atom x = Sum 0 (Map.singleton x 1)

lit :: Double -> Sum
lit x = Sum x Map.empty

toSum :: Expr -> Sum
toSum expr =
  case expr of
    Literal l ->
      case l of
        Num x -> lit x
        _      -> atom expr

    Op1 op e ->
      case op of
        Neg -> mulK (-1) (toSum e)
        _   -> atom expr

    Op2 op e1 e2 ->
      case op of
        Add -> add (toSum e1) (toSum e2)
        Sub -> add (toSum e1) (mulK (-1) (toSum e2))
        Mul | NumLit x <- e1 -> mulK x (toSum e2)
            | NumLit x <- e2 -> mulK x (toSum e1)
        _   -> atom expr

    _ -> atom expr

fromSum :: Sum -> Expr
fromSum (Sum k0 xs) =
  case reverse (Map.toList (Map.filter (/= 0) xs)) of
    []       -> NumLit k0
    t0@(e,k) : rest ->
      case compare k0 0 of
        LT -> Op2 Sub (termFrom start rest) (NumLit (negate k0))
        EQ -> termFrom start rest
        GT | k < 0      -> termFrom (NumLit k0) (t0 : rest)
           | otherwise  -> Op2 Add (termFrom start rest) (NumLit k0)

        where
        termFrom = foldl' term

        start
          | k == 1      = e
          | k == (-1)   = Op1 Neg e
          | otherwise   = Op2 Mul (NumLit k) e

  where
  term t (e,k)
    | k == 1    = Op2 Add t e
    | k == (-1) = Op2 Add t (Op1 Neg e)
    | k < 0     = Op2 Sub t (Op2 Mul (NumLit (negate k)) e)
    | otherwise = Op2 Add t (Op2 Mul (NumLit k) e)




