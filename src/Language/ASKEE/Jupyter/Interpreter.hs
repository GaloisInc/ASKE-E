module Language.ASKEE.Jupyter.Interpreter where

import qualified Data.Aeson as JSON

import Language.ASKEE.Jupyter.Syntax

data JupyterInfo = JupyterInfo
  deriving Show

instance JSON.ToJSON JupyterInfo where
  toJSON JupyterInfo = JSON.object []

-- TODO: Finish implementing these
interpretStmt :: Stmt -> JupyterInfo
interpretStmt stmt =
  case stmt of
    StmtAssign var expr -> interpretExpr expr
    StmtEval expr       -> interpretExpr expr

interpretExpr :: Expr -> JupyterInfo
interpretExpr expr =
  case expr of
    ExprCall fun args -> JupyterInfo
    ExprVar var       -> JupyterInfo
    ExprNumber num    -> JupyterInfo
    ExprString str    -> JupyterInfo
