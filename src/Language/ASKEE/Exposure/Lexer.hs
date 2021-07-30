module Language.ASKEE.Exposure.Lexer where

import Data.Text (Text)

data Token
  = EOF
  | LitD Double
  | LitS Text
  | Define
  | End
  | DefChar Text
  | Assign
  | Comma
  | OpenP
  | CloseP
  | OpenB
  | CloseB
  | OpenC
  | CloseC
  | LambdaArr
  | DotDot
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
  | InfixNot
  | BoolFalse
  | BoolTrue
  | Dot
  | At
  | By
  deriving Show
