module Language.ASKEE.Exposure.Lexer where

import Data.Text (Text)

data Token
  = EOF
  | LitD Double
  | LitS Text
  | Assign
  | Comma
  | OpenP
  | CloseP
  | Ident Text
  | InfixAdd
  | InfixSub
  | InfixMul
  | InfixDiv
  | InfixGT
  | InfixGTE
  | InfixLT
  | InfixLTE
  | InfixEQ
  | InfixNEQ
  | InfixAnd
  | InfixOr
  deriving Show
