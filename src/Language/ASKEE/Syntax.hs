module Language.ASKEE.Syntax where

import Data.Text (Text)

import Prelude hiding (LT, EQ, GT)
import qualified Data.Map as Map

import Language.ASKEE.Expr

data Model = Model { modelName :: Text
                   , modelDecls :: [Decl]
                   , modelEvents :: [Event]
                   }
  deriving (Show, Eq)

data Decl = Let   Text Expr
          | State Text Expr
          | Parameter Text (Maybe Double)
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
stateDecls ds = [(n,v) | State n v <- ds ]

letDecls :: [Decl] -> [(Text, Expr)]
letDecls ds = [(n,v) | Let n v <- ds ]

varDecls :: [Decl] -> [(Text, Expr)]
varDecls ds = ds >>= vd
  where
    vd (Let n v) = [(n, v)]
    vd (State n v) = [(n, v)]
    vd _ = []

parameterDecls :: [Decl] -> [(Text, Maybe Double)]
parameterDecls ds = [(n,v) | Parameter n v <- ds ]

parameterMap :: Model -> Map.Map Text Double
parameterMap mdl = Map.fromList [(n, v) | (n, Just v) <- parameterDecls (modelDecls mdl)]

    
    
