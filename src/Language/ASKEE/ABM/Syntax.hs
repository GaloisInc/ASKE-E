{-# LANGUAGE EmptyDataDeriving #-}
module Language.ASKEE.ABM.Syntax where

import Data.Text ( Text )

import Language.ASKEE.Expr

data Model = Model
  { modelName   :: Text
  , modelAgent  :: Agent
  , modelLets   :: [(Text, Expr)]
  , modelInit   :: [(Text, Expr)]
  , modelEvents :: [Event]
  }
  deriving Show

newtype Agent = Agent [AgentAttribute]
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

data AgentAssign = AgentAssign AgentExpr AgentExpr
  -- LHS must be `Attribute`, RHS must be `Status`
  deriving Show

data AgentExpr =
    Status Text             -- susceptible
  | Attribute Text Text   -- x.city
  | Eq AgentExpr AgentExpr
  | And AgentExpr AgentExpr
  | Or AgentExpr AgentExpr
  --  | Not AgentExpr
  deriving Show