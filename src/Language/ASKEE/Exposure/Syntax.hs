{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.ASKEE.Exposure.Syntax where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map(Map)
import Data.ByteString.Lazy(ByteString)

import Language.ASKEE.Exposure.Plot
import qualified Language.ASKEE.Core.Syntax as Core
import Language.ASKEE.Latex.Syntax (Latex)

type Ident  = Text
data Stmt
  = StmtLet Ident Expr
  | StmtDisplay DisplayExpr
  deriving (Eq, Show)

-- TODO: Labels?
newtype DisplayExpr
  = DisplayScalar Expr
  deriving (Eq, Show, Generic, NFData)

newtype DisplayValue
  = DisplayValue { unDisplayValue :: Value }
  deriving (Eq, Show, Generic, NFData)

data Expr
  = EVar Text
  | EVal Value
  | ECall FunctionName [Expr]
  | ECallWithLambda FunctionWithLambdaName [Expr] Ident Expr
  | EMember Expr Ident
  | EList [Expr]
  | EListRange Expr Expr Expr
  | EPoint [(Ident, Expr)]
  deriving (Show, Eq, Ord, Generic, NFData)

data Value
  = VDouble Double
  | VInt Int
  | VBool Bool
  | VString Text
  | VModel Core.Model
  | VLatex Latex
  | VSampledData [Value] -- ^ list of samples

  | VTimed Value Double
  | VPoint (Map Text Value)
  | VArray [Value]

  | VModelExpr Expr
  | VDFold DynamicalFold Expr
  | VSFold SampleFold DynamicalFold Expr
  | VSuspended

  | VHistogram Double Double Double !(Map Int Int) -- ^ min max size bins
  | VPlot (Plot [Double])
  | VSeries (PlotSeries [Double])
  | VScatter Text [Text] [Double] [[[Double]]]
    -- ^ last list is indexed by series, then time, then sample
  | VTable [Text]    -- List of header labels
           [[Value]] -- The contents of rows after the header.
                     -- The outer list should be equal in length to the list of
                     -- header labels. Each of the inner lists should be of
                     -- equal length to each other.
  | VSVG ByteString
  deriving (Show, Eq, Ord, Generic, NFData)

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
  | FSimulate
  | FFit
  | FMin
  | FMax
  | FAt
  | FLoadEasel
  | FLoadCSV
  | FJoin
  | FMean
  | FInterpolate
  | FHistogram
  | FTimedTime
  | FTimedValue
  | FIn
  | FPlot
  | FSeries
  | FScatter
  | FAsEqnArray
  | FMSE -- Mean Squared Error
  | FMAE -- Mean Absolute Error
  | FSkillRank
  | FTable
  | FSimplify
  | FWithParams
  | FLoadPNC
  | FDescribeModel
  | FModelGraph
  deriving (Show, Eq, Ord, Generic, NFData)

data FunctionWithLambdaName
  = FFilter
  deriving (Show, Eq, Ord, Generic, NFData)

-------------------------------------------------------------------------------
-- compilable expr

-- P(S.I + 10.0 > 30.0 at 10.0)
data DynamicalFold =
    DFAt Double
  | DFAtMany [Double]
  | DFIn Double Double
  deriving (Show, Eq, Ord, Generic, NFData)

data SampleFold =
    SFProbability Double
  | SFSample Double
  deriving (Show, Eq, Ord, Generic, NFData)

prefixFunctionName :: Ident -> Either String FunctionName
prefixFunctionName ident =
  case T.unpack ident of
    "loadESL"     -> Right FLoadEasel
    "loadCSV"     -> Right FLoadCSV
    "loadPNC"     -> Right FLoadPNC
    "describeModel" -> Right FDescribeModel
    "modelGraph"  -> Right FModelGraph
    "join"        -> Right FJoin
    "P"           -> Right FProb
    "mean"        -> Right FMean
    "interpolate" -> Right FInterpolate
    "sample"      -> Right FSample
    "simulate"    -> Right FSimulate
    "fit"         -> Right FFit
    "min"         -> Right FMin
    "max"         -> Right FMax
    "histogram"   -> Right FHistogram
    "time"        -> Right FTimedTime
    "value"       -> Right FTimedValue
    "in"          -> Right FIn
    "plot"        -> Right FPlot
    "scatter"     -> Right FScatter
    "asEqnArray"  -> Right FAsEqnArray
    "mse"         -> Right FMSE
    "mae"         -> Right FMAE
    "table"       -> Right FTable
    "simplify"    -> Right FSimplify
    "withParams"  -> Right FWithParams
    "modelSkillRank" -> Right FSkillRank
    "series"      -> Right FSeries
    strIdent  -> Left $ "Unsupported prefix function name: " ++ strIdent

functionWithLambdaName :: Ident -> Either String FunctionWithLambdaName
functionWithLambdaName ident =
  case T.unpack ident of
    "filter" -> Right FFilter
    strIdent -> Left $ "Unsupported function-with-lambda name: " ++ strIdent
