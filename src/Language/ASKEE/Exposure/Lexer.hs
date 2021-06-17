module Language.ASKEE.Exposure.Lexer where

import Data.Text (Text)

data Token = EOF
  | LitD Double
  | LitS Text
  | PrefixIdent Text
  | InfixIdent Text
  | Assign
  | Comma
  | OpenP
  | CloseP
  deriving Show
