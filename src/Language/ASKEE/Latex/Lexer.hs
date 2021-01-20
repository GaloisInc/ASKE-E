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
  | Newline
  | Sym Text
  | SymTime Text
  | SymInit Text
  | Lit Double
  deriving (Show, Eq)
