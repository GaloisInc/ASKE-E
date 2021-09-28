{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.ASKEE.Expr where

import Control.DeepSeq (NFData)

import Data.List (find)
import Data.Map  (Map, (!?))
import Data.Text (Text, unpack)

import GHC.Generics (Generic)

import Prelude hiding (LT, EQ, GT)

data Expr =
    Add  Expr Expr
  | Sub  Expr Expr
  | Mul  Expr Expr
  | Div  Expr Expr
  | Neg  Expr
  | Exp  Expr
  | Log  Expr
  | Pow  Expr Expr
  | Sin  Expr
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
  deriving (Eq, Generic, NFData, Ord, Show)


eval :: Map Text Expr -> Expr -> Either String Double
eval vars e = ev e
  where
    ev :: Expr -> Either String Double
    ev (Add e1 e2) = binop (+) e1 e2
    ev (Sub e1 e2) = binop (-) e1 e2
    ev (Mul e1 e2) = binop (*) e1 e2
    ev (Div e1 e2) = binop (/) e1 e2
    ev (Neg e1) = negate <$> ev e1
    ev (Exp e1) = exp <$> ev e1
    ev (Log e1) = log <$> ev e1
    ev (Pow e1 e2) = binop (**) e1 e2
    ev (Sin e1) = sin <$> ev e1
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
    ev (Cond branches other) = do
      -- each of `es` is a tuple (e1, e2) :: (Expr, Expr) corresponding to "e1 if e2"
      results <- mapM (evB . snd) branches
      let choices = zip (map fst branches) results
      choice <- case (find snd choices, other) of
                  (Just res, _) -> Right $ fst res
                  (Nothing, Just oth) -> Right oth
                  (Nothing, Nothing) -> Left "no branch of 'cond' evaluated to True"
      ev choice
    ev (Var t) =
      case vars !? t of
        Just e' -> ev e'
        Nothing -> Left $ "unbound variable "<>unpack t
    ev (LitD d) = pure d
    ev (LitB b) = pure $ double b

    evB :: Expr -> Either String Bool
    evB e' = bool <$> ev e'

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