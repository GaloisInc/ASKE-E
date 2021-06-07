module Language.ASKEE.Jupyter.Lexer where

import Data.Text (Text)

data Token = EOF
  | LitD Double
  | LitS Text
  | Sym Text
  | Assign
  | Comma
  | OpenP
  | CloseP
  deriving Show
