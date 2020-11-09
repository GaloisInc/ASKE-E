module Language.ASKEE.Syntax where

import Data.Text (Text)

import Prelude hiding (LT, EQ, GT)

import qualified Language.ASKEE.Expr as Expr

data Model = Model { modelName :: Text
                   , modelDecls :: [Decl]
                   , modelEvents :: [Event]
                   }
  deriving (Show, Eq)

data Decl = Let   Text ModelExpr
          | State Text ModelExpr
          | Assert Expr.LogExpr
  deriving (Show, Eq)   

data Event = Event { eventName     :: Text
                   , eventWhen     :: Maybe (Expr.LogExpr)
                   , eventRate     :: ModelExpr
                   , eventEffect   :: [Statement]
                   , eventMetadata :: Maybe Text
                   }
  deriving (Show, Eq)

type Statement = (Text, Expr.ArithExpr)

data ModelExpr = 
    ArithExpr Expr.ArithExpr
  | IfExpr    Expr.LogExpr ModelExpr ModelExpr
  | CondExpr  Condition
  deriving (Show, Eq)

data Condition = Condition { condChoices   :: [(Expr.ArithExpr, Expr.LogExpr)]
                           , condOtherwise :: Maybe (Expr.ArithExpr)
                           }
  deriving (Show, Eq)

-------------------------------------------------------------------------------

-- utility functions

stateDecls :: [Decl] -> [(Text, ModelExpr)]
stateDecls ds = ds >>= sd
  where
    sd (State n v) = [(n,v)]
    sd _ = [] 

letDecls :: [Decl] -> [(Text, ModelExpr)]
letDecls ds = ds >>= ld 
  where
    ld (Let n v) = [(n, v)]
    ld _ = []

varDecls :: [Decl] -> [(Text, ModelExpr)]
varDecls ds = ds >>= vd
  where
    vd (Let n v) = [(n, v)]
    vd (State n v) = [(n, v)]
    vd _ = []
    
    
