{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Exposure.Syntax where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.ASKEE.Core.Syntax as Core

type Ident  = Text
data Stmt
  = StmtLet Ident Expr
  | StmtDisplay DisplayExpr
  deriving (Eq, Show)

-- TODO: Labels?
newtype DisplayExpr
  = DisplayScalar Expr
  deriving (Eq, Show)

data Expr
  = EVar Text
  | EVal Value
  | ECall FunctionName [Expr]
  | EMember Expr Ident
  deriving (Eq, Show)

data Value
  = VDouble Double
  | VInt
  | VBool Bool
  | VString Text
  | VModel Core.Model
  | VModelExpr Expr
  | VDFold DynamicalFold Expr
  | VSFold SampleFold DynamicalFold Expr
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- compilable expr

-- P(S.I + 10.0 > 30.0 at 10.0)
data DynamicalFold =
  DFAt Double
  deriving (Eq, Show)

data SampleFold =
    SFProbability
  | SFSample
  deriving (Eq, Show)

prefixFunctionName :: Ident -> Either String FunctionName
prefixFunctionName ident =
  case T.unpack ident of
    "loadESL" -> Right FLoadEasel
    strIdent  -> Left $ "Unsupported prefix function name: " ++ strIdent
