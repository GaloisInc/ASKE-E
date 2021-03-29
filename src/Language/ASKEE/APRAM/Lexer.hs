module Language.ASKEE.APRAM.Lexer where

import Prelude hiding ( GT, EQ, LT )

data Token = EOF
  | Sym String
  | LitD Double
  | LitB Bool 
  | LitS String
  | OpenP
  | CloseP
  | OpenB
  | CloseB
  | Comma
  | Colon
  | Lambda
  | Log
  | Exp
  | Dot
  | Size
  | MakeColumn
  | MakeParam
  | MakeCohort
  | MakeMod
  | CohortExpr String LogOp
  | ParamVal String
  | CohortSize String
  | ColumnAssign String
  | Plus
  | Minus
  | Times
  | Divide
  | Assign
  | If
  | Then
  | Else
  | GT
  | GTE
  | EQ
  | LTE
  | LT
  | And
  | Or
  | Not
  | Pop
  | Sim
  | For
  | In
  | Return
  | Star
  | StarStar
  | IGNORE
  deriving (Eq, Show)

data LogOp =
    Eq
  | Lt
  | Gt
  deriving (Eq, Show)