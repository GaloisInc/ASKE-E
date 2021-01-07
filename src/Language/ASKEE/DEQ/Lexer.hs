module Language.ASKEE.DEQ.Lexer where

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
  | Var
  | DDt
  | Sym Text
  | Lit Double
  deriving Show