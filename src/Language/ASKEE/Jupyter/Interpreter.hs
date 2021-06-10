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
    StmtLet var expr     -> interpretExpr expr
    StmtDisplay dispExpr -> interpretDisplayExpr dispExpr

interpretExpr :: Expr -> JupyterInfo
interpretExpr expr =
  case expr of
    EVar var         -> JupyterInfo
    ELit lit         -> interpretLiteral lit
    ECall fun args   -> interpretCall fun args
    EMember expr lab -> interpretExpr expr

interpretDisplayExpr :: DisplayExpr -> JupyterInfo
interpretDisplayExpr (DisplayScalar scalar) = interpretExpr scalar

interpretLiteral :: Literal -> JupyterInfo
interpretLiteral lit =
  case lit of
    LitNum num    -> JupyterInfo
    LitString str -> JupyterInfo

interpretCall :: FunctionName -> [Expr] -> JupyterInfo
interpretCall fun args =
  case fun of
    FAdd         -> JupyterInfo
    FSub         -> JupyterInfo
    FMul         -> JupyterInfo
    FDiv         -> JupyterInfo
    FGT          -> JupyterInfo
    FGTE         -> JupyterInfo
    FLT          -> JupyterInfo
    FLTE         -> JupyterInfo
    FEQ          -> JupyterInfo
    FNEQ         -> JupyterInfo
    FNot         -> JupyterInfo
    FAnd         -> JupyterInfo
    FOr          -> JupyterInfo
    FProb        -> JupyterInfo
    FSample      -> JupyterInfo
    FAt          -> JupyterInfo
    FMkArray     -> JupyterInfo
    FMkIntervals -> JupyterInfo
    FLoadEasel   -> JupyterInfo
