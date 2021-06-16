module Language.ASKEE.Exposure.Lexer where

import Data.Text (Text)

data Token = EOF
  | LitD Double
  | LitS Text
  | Sym Text
  | Assign
  | Comma
  | OpenP
  | CloseP
  | LoadEasel
  deriving Show
