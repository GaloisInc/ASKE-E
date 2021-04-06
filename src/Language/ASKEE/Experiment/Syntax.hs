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
    Set Text Expr
  | Let Text Expr
  | If [(Expr, [Stmt])] [Stmt]
  deriving Show

data Type =
    TypeNumber
  | TypeBool
  | TypeSequence Type
  | TypePoint [(Text, Type)]
  | TypeModel Text
  deriving Show

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