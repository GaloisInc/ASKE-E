module Language.ASKEE.Syntax where

import Data.Text (Text)

import Prelude hiding (LT, EQ, GT)

import Language.ASKEE.Expr

data Model = Model { modelName :: Text
                   , modelDecls :: [Decl]
                   , modelEvents :: [Event]
                   }
  deriving (Show, Eq)

data Decl = Let   Text Expr
          | State Text Expr
          | Assert Expr
  deriving (Show, Eq)   

data Event = Event { eventName     :: Text
                   , eventWhen     :: Maybe Expr
                   , eventRate     :: Expr
                   , eventEffect   :: [Statement]
                   , eventMetadata :: Maybe Text
                   }
  deriving (Show, Eq)

type Statement = (Text, Expr)

-------------------------------------------------------------------------------

-- utility functions

stateDecls :: [Decl] -> [(Text, Expr)]
stateDecls ds = ds >>= sd
  where
    sd (State n v) = [(n,v)]
    sd _ = [] 

letDecls :: [Decl] -> [(Text, Expr)]
letDecls ds = ds >>= ld 
  where
    ld (Let n v) = [(n, v)]
    ld _ = []

varDecls :: [Decl] -> [(Text, Expr)]
varDecls ds = ds >>= vd
  where
    vd (Let n v) = [(n, v)]
    vd (State n v) = [(n, v)]
    vd _ = []
    
    
