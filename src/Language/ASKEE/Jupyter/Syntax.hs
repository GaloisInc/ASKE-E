module Language.ASKEE.Jupyter.Syntax where

import Data.Text (Text)

type Binder = Text
type Ident  = Text
type Label  = Int

data Stmt
  = StmtLet Binder Expr
  | StmtDisplay DisplayExpr
  deriving Show

-- TODO: Labels?
newtype DisplayExpr
  = DisplayScalar Expr
  deriving Show

data Literal
  = LitNum Double
  | LitString Text
  deriving Show

data Expr
  = EVar Text
  | ELit Literal
  | ECall FunctionName [Expr]
  | EMember Expr Label
  deriving Show

data FunctionName
  = FAdd
  | FSub
  | FMul
  | FDiv
  | FGT
  | FGTE
  | FLT
  | FLTE
  | FEQ
  | FNEQ
  | FNot
  | FAnd
  | FOr
  | FProb
  | FSample
  | FAt
  | FMkArray
  | FMkIntervals
  | FLoadEasel
  deriving Show
