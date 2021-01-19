module Language.ASKEE.Latex.Lexer where

import Data.Text ( Text )  

data Token = EOF
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Frac
  | OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | Sym Text
  | Lit Double
  deriving Show
