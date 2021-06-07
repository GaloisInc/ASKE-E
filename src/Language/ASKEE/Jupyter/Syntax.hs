module Language.ASKEE.Jupyter.Syntax where

import Data.Text (Text)

data Stmt
  = StmtAssign Text Expr
  | StmtEval Expr
  deriving Show

data Expr
  = ExprCall Text [Expr]
  | ExprVar Text
  | ExprString Text
  | ExprNumber Double
  deriving Show
