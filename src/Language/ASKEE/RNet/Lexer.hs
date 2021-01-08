module Language.ASKEE.RNet.Lexer where

import Data.Text ( Text )

data Token = EOF
  | Plus
  | Minus
  | Times
  | Divide
  | Assign
  | OpenP
  | CloseP
  | Let
  | When
  | Nil
  | Comma
  | Arrow
  | Sym Text
  | Lit Double
  | Int Int
  deriving Show