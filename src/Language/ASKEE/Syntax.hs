module Language.ASKEE.Syntax where

import Data.Text (Text)

import Prelude hiding (LT, EQ, GT)

import qualified Language.ASKEE.Expr as Expr

data Model = Model { modelName :: Text
                   , modelDecls :: [Decl]
                   , modelEvents :: [Event]
                   }
  deriving (Show, Eq)

data Decl = Let   { name :: Text
                  , val  :: ModelExpr
                  }
          | State { name :: Text
                  , val  :: ModelExpr
                  }
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
