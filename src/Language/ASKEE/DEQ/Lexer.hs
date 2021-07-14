module Language.ASKEE.DEQ.Lexer where

import Data.Text ( Text )

import Prelude hiding ( LT, EQ, GT )

data Token = EOF
  | Plus
  | Minus
  | Times
  | Divide
  | Assign
  | OpenP
  | CloseP
  | Let
  | Parameter
  | DDt
  | If
  | Then
  | Else
  | GT
  | GTE
  | EQ
  | LTE
  | LT
  | Not
  | And
  | Or
  | LitB Bool
  | LitD Double
  | Sym Text
  deriving Show
