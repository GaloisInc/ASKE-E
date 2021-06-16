module Language.ASKEE.Exposure.Interpreter where

import qualified Data.Aeson as JSON

import Language.ASKEE.Exposure.Syntax

data ExposureInfo = ExposureInfo
  deriving Show

instance JSON.ToJSON ExposureInfo where
  toJSON ExposureInfo = JSON.object []

-- TODO: Finish implementing these
interpretStmt :: Stmt -> ExposureInfo
interpretStmt stmt =
  case stmt of
    StmtLet var expr     -> interpretExpr expr
    StmtDisplay dispExpr -> interpretDisplayExpr dispExpr

interpretExpr :: Expr -> ExposureInfo
interpretExpr expr =
  case expr of
    EVar var         -> ExposureInfo
    ELit lit         -> interpretLiteral lit
    ECall fun args   -> interpretCall fun args
    EMember expr lab -> interpretExpr expr

interpretDisplayExpr :: DisplayExpr -> ExposureInfo
interpretDisplayExpr (DisplayScalar scalar) = interpretExpr scalar

interpretLiteral :: Literal -> ExposureInfo
interpretLiteral lit =
  case lit of
    LitNum num    -> ExposureInfo
    LitString str -> ExposureInfo

interpretCall :: FunctionName -> [Expr] -> ExposureInfo
interpretCall fun args =
  case fun of
    FAdd         -> ExposureInfo
    FSub         -> ExposureInfo
    FMul         -> ExposureInfo
    FDiv         -> ExposureInfo
    FGT          -> ExposureInfo
    FGTE         -> ExposureInfo
    FLT          -> ExposureInfo
    FLTE         -> ExposureInfo
    FEQ          -> ExposureInfo
    FNEQ         -> ExposureInfo
    FNot         -> ExposureInfo
    FAnd         -> ExposureInfo
    FOr          -> ExposureInfo
    FProb        -> ExposureInfo
    FSample      -> ExposureInfo
    FAt          -> ExposureInfo
    FMkArray     -> ExposureInfo
    FMkIntervals -> ExposureInfo
    FLoadEasel   -> ExposureInfo
