{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Exposure.Syntax where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map(Map)

import Language.ASKEE.DataSeries (DataSeries)
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

newtype DisplayValue
  = DisplayValue { unDisplayValue :: Value }
  deriving (Eq, Show)

data Expr
  = EVar Text
  | EVal Value
  | ECall FunctionName [Expr]
  | ECallWithLambda FunctionWithLambdaName [Expr] Ident Expr
  | EMember Expr Ident
  | EList [Expr]
  | EListRange Expr Expr Expr
  deriving (Show, Eq, Ord)

data Value
  = VDouble Double
  | VInt Int
  | VBool Bool
  | VString Text
  | VDataSeries (DataSeries Double)
  | VModel Core.Model

  | VTimed Value Double
  | VPoint (Map Text Value)
  | VArray [Value]

  | VModelExpr Expr
  | VDFold DynamicalFold Expr
  | VSFold SampleFold DynamicalFold Expr
  | VSuspended

  | VHistogram Double Double Double !(Map Int Int) -- ^ min max size bins
  deriving (Show, Eq, Ord)

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
  | FMin
  | FMax
  | FAt
  | FLoadEasel
  | FLoadCSV
  | FMean
  | FInterpolate
  | FHistogram
  | FTimedTime
  | FTimedValue
  | FIn
  deriving (Show, Eq, Ord)

data FunctionWithLambdaName
  = FFilter
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- compilable expr

-- P(S.I + 10.0 > 30.0 at 10.0)
data DynamicalFold =
    DFAt Double
  | DFAtMany [Double]
  | DFIn Double Double
  deriving (Show, Eq, Ord)

data SampleFold =
    SFProbability Double
  | SFSample Double
  deriving (Show, Eq, Ord)

prefixFunctionName :: Ident -> Either String FunctionName
prefixFunctionName ident =
  case T.unpack ident of
    "loadESL"     -> Right FLoadEasel
    "loadCSV"     -> Right FLoadCSV
    "P"           -> Right FProb
    "mean"        -> Right FMean
    "interpolate" -> Right FInterpolate
    "sample"      -> Right FSample
    "min"         -> Right FMin
    "max"         -> Right FMax
    "histogram"   -> Right FHistogram
    "time"        -> Right FTimedTime
    "value"       -> Right FTimedValue
    "in"          -> Right FIn
    strIdent  -> Left $ "Unsupported prefix function name: " ++ strIdent

functionWithLambdaName :: Ident -> Either String FunctionWithLambdaName
functionWithLambdaName ident =
  case T.unpack ident of
    "filter" -> Right FFilter
    strIdent -> Left $ "Unsupported function-with-lambda name: " ++ strIdent
