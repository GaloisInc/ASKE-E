module Language.ASKEE.Exposure.Lexer where

import Data.Text (Text)

data Token
  = EOF
  | LitI Int
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
  | OpenCC
  | CloseCC
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
  | Peak
  | Over
  | By
  | AtSymbol
  deriving Show
