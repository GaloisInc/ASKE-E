{-# Language OverloadedStrings #-}
module Language.ASKEE.Experiment.Syntax where

import Data.Text(Text)

-------------------------------------------------------------------------------
-- Syntax

data TypedName =
  TypedName { tnName :: Text
            , tnType :: Maybe Type
            }
  deriving Show

untypedName :: Text -> TypedName
untypedName x = TypedName { tnName = x, tnType = Nothing }

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
              }
  deriving Show

data SampleExpr =
  SampleExpr { seName :: Ident
             , seArgs :: [Expr]
             , seRange :: Expr
             }
  deriving Show

data ExperimentDecl =
  ExperimentDecl { experimentName :: Ident
                 , experimentArgs :: [Binder]
                 , experimentInputs :: [(Binder, SampleExpr)]
                 , experimentMeasures :: [(Binder, MeasureExpr)]
                 , experimentLets :: [(Binder, Expr)]
                 , experimentYield :: [(Label, Expr)]
                 }
  deriving Show

data MeasureDecl =
  MeasureDecl { measureName :: Ident
              , measureArgs :: [Binder]
              , measureVars :: [(Binder, Expr)]
              , measureImpl :: (Binder, [Stmt])
              }
  deriving Show

data Stmt =
    Set Ident Expr
  | Let Binder Expr
  | If [(Expr, [Stmt])] [Stmt]
  deriving Show

data Type =
    TypeNumber
  | TypeBool
  | TypeSequence Type
  -- | TypePoint (Map Text Type)
  -- | TypeCallable [Type] Type
  | TypeVar Int
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
