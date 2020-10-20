module Language.ASKEE.DiffEq.Lexer where

import Data.Text ( Text )  

data Token = EOF
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Frac
  | OpenB
  | CloseB
  | OpenP
  | CloseP
  | Var Text
  | Lit Double
  deriving Show

data Located a =
  Located { locLine :: Int
          , locCol :: Int
          , locVal :: a
          }
  
instance Show a => Show (Located a) where
  show (Located _ _ x) = show x