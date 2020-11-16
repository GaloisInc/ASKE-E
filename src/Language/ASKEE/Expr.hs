{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.ASKEE.Expr where

import Data.Map  (Map, (!?))
import Data.Text (Text, unpack)

import Prelude hiding (LT, EQ, GT)

data Expr =
    Add  Expr Expr
  | Sub  Expr Expr
  | Mul  Expr Expr
  | Div  Expr Expr
  | Neg  Expr
  | And  Expr Expr
  | Or   Expr Expr
  | Not  Expr
  | LT   Expr Expr
  | LTE  Expr Expr
  | EQ   Expr Expr
  | GTE  Expr Expr
  | GT   Expr Expr
  | If   Expr Expr Expr
  | Cond [(Expr, Expr)] (Maybe Expr)
  | Var  Text
  | LitD Double
  | LitB Bool
  deriving (Eq, Show, Ord)


instance MonadFail (Either String) where
  fail = Left


eval :: Map Text Expr -> Expr -> Either String Double
eval vars e = ev e
  where
    ev :: Expr -> Either String Double
    ev (Add e1 e2) = binop (+) e1 e2
    ev (Sub e1 e2) = binop (-) e1 e2
    ev (Mul e1 e2) = binop (*) e1 e2
    ev (Div e1 e2) = binop (/) e1 e2
    ev (Neg e1) = negate <$> ev e1
    ev (And e1 e2) = logop (&&) e1 e2
    ev (Or e1 e2) = logop (||) e1 e2
    ev (Not e1) = double . not <$> evB e1
    ev (LT e1 e2) = cmpop (<) e1 e2
    ev (LTE e1 e2) = cmpop (<=) e1 e2
    ev (EQ e1 e2) = cmpop (==) e1 e2
    ev (GTE e1 e2) = cmpop (>=) e1 e2
    ev (GT e1 e2) = cmpop (>) e1 e2
    ev (If e1 e2 e3) = do
      e1' <- evB e1
      if e1' then ev e2 else ev e3
    ev (Cond es other) = undefined
    ev (Var t) =
      case vars !? t of
        Just e -> ev e
        Nothing -> fail $ "unbound variable "<>unpack t
    ev (LitD d) = pure d
    ev (LitB b) = pure $ double b

    evB :: Expr -> Either String Bool
    evB e = bool <$> ev e

    binop :: (Double -> Double -> Double) -> Expr -> Expr -> Either String Double
    binop op e1 e2 = op <$> ev e1 <*> ev e2

    logop :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Either String Double
    logop op e1 e2 = double <$> (op <$> evB e1 <*> evB e2)

    cmpop :: (Double -> Double -> Bool) -> Expr -> Expr -> Either String Double
    cmpop op e1 e2 = double <$> (op <$> ev e1 <*> ev e2)

    double True = 1.0
    double False = 0.0

    bool 0.0 = False
    bool _ = True
{-

data ArithExpr =
    Add ArithExpr ArithExpr
  | Sub ArithExpr ArithExpr
  | Mul ArithExpr ArithExpr
  | Div ArithExpr ArithExpr
  | Neg ArithExpr
  | Var Text
  | ALit Double
  deriving (Show, Eq, Ord)

data LogExpr =
    And LogExpr LogExpr
  | Or  LogExpr LogExpr
  | Not LogExpr
  | LT  ArithExpr ArithExpr
  | LTE ArithExpr ArithExpr
  | EQ  ArithExpr ArithExpr
  | GTE ArithExpr ArithExpr
  | GT  ArithExpr ArithExpr
  | LLit Bool
  deriving (Show, Eq, Ord)


instance MonadFail (Either String) where
  fail = Left


evalArith :: Map Text ArithExpr -> ArithExpr -> Either String Double
evalArith vars exp = ev exp
  where
    ev :: ArithExpr -> Either String Double
    ev (Add e1 e2) = binop (+) e1 e2
    ev (Sub e1 e2) = binop (-) e1 e2
    ev (Mul e1 e2) = binop (*) e1 e2
    ev (Div e1 e2) = binop (/) e1 e2
    ev (Neg e1)    = negate <$> ev e1
    ev (ALit d)     = pure d
    ev (Var t)     = 
      case vars !? t of
        Just e -> ev e
        Nothing -> fail $ "Expr: variable "<>unpack t<>" not found"

    binop :: (Double -> Double -> Double) -> ArithExpr -> ArithExpr -> Either String Double
    binop op e1 e2 = op <$> ev e1 <*> ev e2


evalLog :: Map Text ArithExpr -> LogExpr -> Either String Bool
evalLog vars exp = ev exp
  where
    ev :: LogExpr -> Either String Bool
    ev (And e1 e2) = binop ev (&&) e1 e2
    ev (Or  e1 e2) = binop ev (||) e1 e2
    ev (Not e1)    = not <$> ev e1
    ev (LT e1 e2)  = binop evA (<)  e1 e2 
    ev (LTE e1 e2) = binop evA (<=) e1 e2 
    ev (EQ e1 e2)  = binop evA (==) e1 e2 
    ev (GTE e1 e2) = binop evA (>=) e1 e2 
    ev (GT e1 e2)  = binop evA (>)  e1 e2
    ev (LLit b)    = pure b

    evA :: ArithExpr -> Either String Double
    evA = evalArith vars

    binop :: (e -> Either String r) -> (r -> r -> Bool) -> e -> e -> Either String Bool
    binop eval op e1 e2 = op <$> eval e1 <*> eval e2

-}