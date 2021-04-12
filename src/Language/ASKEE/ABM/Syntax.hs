{-# LANGUAGE EmptyDataDeriving #-}
module Language.ASKEE.ABM.Syntax where

import Data.Text ( Text )

import Language.ASKEE.Expr

data Model = Model
  { modelName   :: Text
  , modelAgent  :: [AgentAttribute]
  , modelLets   :: [(Text, Expr)]
  , modelInit   :: [(Text, Expr)]
  , modelEvents :: [Event]
  }
  deriving Show

data AgentAttribute = AgentAttribute 
  { attributeName     :: Text
  , attributeStatuses :: [Text]
  }
  deriving Show

data Event = Event
  { eventName   :: Text
  , eventAgents :: [Text]
  , eventWhen   :: AgentExpr
  , eventRate   :: Expr
  , eventEffect :: [AgentAssign]
  }
  deriving Show

data AgentAssign = AgentAssign AttributeRef AttributeRef
  deriving Show

data AttributeRef =
    Status Text
  | Attribute Text Text
  deriving Show

data AgentExpr = 
    Eq AttributeRef AttributeRef
  | And AgentExpr AgentExpr
  | Or AgentExpr AgentExpr
  deriving Show

data RateExpr =
    Plain Expr
  | Size Expr
  deriving Show