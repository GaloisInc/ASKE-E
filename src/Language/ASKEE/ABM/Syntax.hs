{-# LANGUAGE EmptyDataDeriving #-}
module Language.ASKEE.ABM.Syntax where

import Language.ASKEE.Expr

data Model = Model
  { modelName   :: String
  , modelAgent  :: Agent
  , modelLets   :: [(String, Expr)]
  , modelInit   :: [(String, Expr)]
  , modelEvents :: [Event]
  }
  deriving Show

newtype Agent = Agent [AgentAttribute]
  deriving Show

data AgentAttribute = AgentAttribute 
  { attributeName     :: String
  , attributeStatuses :: [String]
  }
  deriving Show

data Event = Event
  { eventName   :: String
  , eventAgents :: [String]
  , eventWhen   :: AgentExpr
  , eventRate   :: Expr
  , eventEffect :: [AgentAssign]
  }
  deriving Show

data AgentAssign = AgentAssign AgentExpr AgentExpr
  -- LHS must be `Attribute`, RHS must be `Status`
  deriving Show

data AgentExpr =
    Status String             -- susceptible
  | Attribute String String   -- x.city
  | Eq AgentExpr AgentExpr
  | And AgentExpr AgentExpr
  | Or AgentExpr AgentExpr
  | Not AgentExpr
  deriving Show