{-# Language OverloadedStrings #-}
module Language.ASKEE.Experiment.Syntax where

import Data.Text(Text)
import Data.Map(Map)

-------------------------------------------------------------------------------
-- Syntax

data TypedName =
  TypedName { tnName :: Text
            , tnType :: Maybe Type
            }
  deriving Show

untypedName :: Text -> TypedName
untypedName x = TypedName { tnName = x, tnType = Nothing }

setType :: TypedName -> Type -> TypedName
setType x t = x { tnType = Just t }

getType :: TypedName -> Type
getType x = case tnType x of
              Just t  -> t
              Nothing -> error "[BUG] Missing type on name"

type Binder = TypedName
type Ident = TypedName
type Label = Text

data Decl =
    DMeasure MeasureDecl
  | DExperiment ExperimentDecl
  deriving Show

data MeasureExpr =
  MeasureExpr { meMeasureName :: Ident
              , meDataset     :: Ident
              , meArgs        :: [Expr]
              , meTypeArgs    :: [Type]
              }
  deriving Show

data SampleExpr =
  SampleExpr { seName :: Ident
             , seArgs :: [Expr]
             , seRange :: Expr
             }
  deriving Show


data ExperimentStmt =
    ESLet Binder Expr
  | ESSample Binder SampleExpr
  | ESMeasure Binder MeasureExpr
  deriving Show

data ExperimentDecl =
  ExperimentDecl { experimentName :: Ident
                 , experimentArgs :: [Binder]
                 , experimentStmts :: [ExperimentStmt]
                 , experimentReturn :: Expr
                 }
  deriving Show

-- XXX: add measure finalizer/initializers?
data MeasureDecl =
  MeasureDecl { measureName :: Ident
              , measureTArgs :: [Int]
              , measureConstraints :: [TypeConstraint]
              , measureArgs :: [Binder]
              , measureVars :: [(Binder, Expr)]
              , measureDataBinder :: Binder
              , measureImpl :: [Stmt]
              }
  deriving Show

data Qualified t = Forall [Int] [TypeConstraint] t

data MeasureType = MeasureType
  { mtArgs   :: [Type]
  , mtData   :: Type
  , mtResult :: Type
  }

--XXX: this is really a function/callable type
data ExperimentType = ExperimentType
  { etArgs   :: [Type]
  , etResult :: Type
  }

measureType :: MeasureDecl -> Qualified MeasureType
measureType m = Forall (measureTArgs m) (measureConstraints m)
                MeasureType
                  { mtArgs   = getType <$> measureArgs m
                  , mtData   = getType (measureDataBinder m)
                  , mtResult = getType (measureName m)
                  }

experimentType :: ExperimentDecl -> ExperimentType
experimentType e =
  ExperimentType { etArgs = getType <$> experimentArgs e
                 , etResult = getType (experimentName e)
                 }


data Stmt =
    Set Ident Expr
  | Let Binder Expr
  | If [(Expr, [Stmt])] [Stmt]
  deriving Show

data TypeVar =
    TVBound Int
  | TVFree  Int
  deriving (Show, Eq, Ord)

data Type =
    TypeNumber
  | TypeBool
  | TypeStream Type -- stream/dataset
  | TypePoint (Map Text Type)
  -- | TypeCallable [Type] Type
  | TypeVar TypeVar
  deriving (Show, Eq, Ord)

data TypeConstraint =
    HasField Type Label Type
  deriving (Show, Eq, Ord)

data Literal =
    LitBool Bool
  | LitNum Double
  deriving Show

data Expr =
    Lit Literal
  | Var Ident
  | Call FunctionName [Expr]
  | Dot Expr Label
  | Point [(Text, Expr)]
  deriving Show

data FunctionName =
    Add
  | Multiply
  | Divide
  | Subtract
  | Negate
  | Not
  | LessThan
  | GreaterThan
  | LessThanEqual
  | GreaterThanEqual
  | Equal
  | NotEqual
  | Or
  | And
  deriving Show
