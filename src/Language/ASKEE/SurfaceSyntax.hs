module Language.ASKEE.SurfaceSyntax where

import Data.Text ( Text )

import Prelude hiding (LT, EQ, GT)

data Model = Model { modelName :: Text
                   , modelDecls :: [Decl]
                   , modelEvents :: [Event]
                   }
  deriving (Show, Eq)

data Decl = Let { name :: Text
                , val  :: Exp
                }
          | State { name :: Text
                  , val  :: Exp 
                  }
  deriving (Show, Eq)

data Event = Event { eventName     :: Text
                   , eventWhen     :: Maybe Exp
                   , eventRate     :: Exp
                   , eventEffect   :: [Statement]
                   , eventMetadata :: Maybe Text
                   }
  deriving (Show, Eq)

type Statement = (Text, Exp)

data Exp = Add  Exp Exp
         | Sub  Exp Exp
         | Mul  Exp Exp
         | Div  Exp Exp
         | Neg  Exp Exp
         | GT   Exp Exp
         | GTE  Exp Exp
         | EQ   Exp Exp
         | LTE  Exp Exp
         | LT   Exp Exp
         | And  Exp Exp
         | Or   Exp Exp
         | Not  Exp Exp
         | If   Exp Exp Exp
         | Cond Condition
         | Real Double
         | Var  Text
  deriving (Show, Eq)

data Condition = Condition { condIf        :: Exp
                           , condChoices   :: [(Exp, Exp)]
                           , condOtherwise :: Maybe (Exp, Exp)
                           }
  deriving (Show, Eq)