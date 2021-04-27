module Language.ASKEE.APRAM.Lexer where

import Data.Text ( Text )

import Prelude hiding ( GT, EQ, LT )

data Token = EOF
  | Sym Text
  | LitD Double
  | LitB Bool 
  | LitS Text
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
  | CohortExpr Text LogOp
  | ParamVal Text
  | CohortSize Text
  | ColumnAssign Text
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