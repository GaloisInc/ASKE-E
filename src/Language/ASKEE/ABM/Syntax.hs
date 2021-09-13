{-# LANGUAGE EmptyDataDeriving #-}
module Language.ASKEE.ABM.Syntax where

import Data.Text ( Text )
import Data.Map ( Map )

import Language.ASKEE.Expr

type Agent = Map Text AgentAttribute  -- ^ Names of attributes mapped to attributes themselves

data Model = Model
  { modelName   :: Text
  , modelAgent  :: Agent
  , modelLets   :: Map Text Expr
  , modelInit   :: Map Text Expr -- ^ Initial conditions of the model
  , modelEvents :: [Event]
  }
  deriving Show

data Mingling = Mingling | Nonmingling
  deriving (Eq, Show)

data AgentAttribute = AgentAttribute 
  { attributeName     :: Text -- ^ The name/type of an attribute
  , attributeMingling :: Mingling -- ^ Whether or not agents with different statuses
                                  -- within this attribute might encounter one another
  , attributeStatuses :: [Text] -- ^ The statuses associated with this attribute
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
    Eq AttributeRef AttributeRef -- ^ `x.city == y.city` or `x.health == S`, e.g.
  | And AgentExpr AgentExpr
  | Or AgentExpr AgentExpr
  deriving Show
