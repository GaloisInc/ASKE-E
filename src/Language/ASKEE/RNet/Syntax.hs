module Language.ASKEE.RNet.Syntax where

import Data.Text ( Text )
import Data.Map ( Map )

import Language.ASKEE.Expr ( Expr(..) )

{-
Concrete syntax:

unconditional:
  Expr ',' ReactionExp '-->' ReactionExp

guarded:
  Expr 'when' Expr ',' ReactionExp '-->' ReactionExp 

-}

-- for a surface syntax, we may also want to support the "multiple reactions in one line" from [1]


data ReactionNet = ReactionNet
  { bindings :: Map Text Expr
  , reactions :: [Reaction]
  }
  deriving Show

data Reaction =
  Reaction { reactionRate       :: Expr
           , reactionType       :: ReactionRate
           , reactionEnabled    :: Maybe Expr
           , reactionSubstrates :: ReactionExp
           , reactionProducts   :: ReactionExp 
           } 
  deriving Show

data ReactionExp = Nil | ReactionTerms [ReactionTerm]
  deriving Show

data ReactionTerm = ReactionTerm Int Text
  deriving Show

data ReactionRate = MassAction | FixedRate
  deriving Show