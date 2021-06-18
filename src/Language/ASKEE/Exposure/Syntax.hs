{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Exposure.Syntax where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.ASKEE.Core.Syntax as Core

type Ident  = Text
data Stmt
  = StmtLet Ident Expr
  | StmtDisplay DisplayExpr
  deriving Show

-- TODO: Labels?
newtype DisplayExpr
  = DisplayScalar Expr
  deriving Show

data Expr
  = EVar Text
  | EVal Value
  | ECall FunctionName [Expr]
  | EMember Expr Ident
  deriving Show

data Value
  = VDouble Double
  | VInt
  | VBool Bool
  | VString Text
  | VModel Core.Model
  | VModelExpr Expr
  | VDFold DynamicalFold Expr
  | VSFold SampleFold DynamicalFold Expr
  deriving Show

-- sir = loadESL("model.esl") -> VModelExpr (EVal (VModel ....) )
-- sir.I at 30.0


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
  | FLoadEasel
  deriving Show

-------------------------------------------------------------------------------
-- compilable expr

-- P(S.I + 10.0 > 30.0 at 10.0)
data DynamicalFold =
  DFAt Double
  deriving Show

data SampleFold =
    SFProbability
  | SFSample
  deriving Show

prefixFunctionName :: Ident -> Either String FunctionName
prefixFunctionName ident =
  case T.unpack ident of
    "loadEasel" -> Right FLoadEasel
    strIdent    -> Left $ "Unsupported prefix function name: " ++ strIdent

infixFunctionName :: Ident -> Either String FunctionName
infixFunctionName ident =
  case T.unpack ident of
    "+"      -> Right FAdd
    "-"      -> Right FSub
    "*"      -> Right FMul
    "/"      -> Right FDiv
    ">"      -> Right FGT
    ">="     -> Right FGTE
    "<"      -> Right FLT
    "<="     -> Right FLTE
    "=="     -> Right FEQ
    "!="     -> Right FNEQ
    "&&"     -> Right FAnd
    "||"     -> Right FOr
    strIdent -> Left $ "Unsupported infix function name: " ++ strIdent
