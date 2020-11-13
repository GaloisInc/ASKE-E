{-# Language FlexibleContexts #-}

module Language.ASKEE.Core where

import Data.Map
import Data.Text(Text)

type Ident = Text

data Expr =
    Literal Literal
  | Op1 Op1 Expr
  | Op2 Op2 Expr Expr
  | Var Ident
  | If Expr Expr Expr
  | Fail String
    deriving (Show,Eq)

data Op1 = Not | Neg
  deriving (Show,Eq)

data Op2 = Add | Mul | Sub | Div | Lt | Leq | Eq | And | Or
  deriving (Show,Eq)

data Literal =
    Num Double
  | Bool Bool
    deriving (Show,Eq)

data Event =
  Event { eventName   :: Ident
        , eventRate   :: Expr
        , eventWhen   :: Expr
        , eventEffect :: Map Ident Expr
        }
  deriving (Show, Eq)

data Model =
  Model { modelName :: Text
        , modelInitState :: Map Ident Double
        , modelEvents    :: [Event]
        }
  deriving (Show, Eq)



