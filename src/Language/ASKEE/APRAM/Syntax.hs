module Language.ASKEE.APRAM.Syntax where

import Data.Map ( Map )
import Data.Text ( Text, unpack )

import Language.ASKEE.Expr ( Expr )
  
data APRAM = APRAM
  { apramAgents   :: Int
  , apramParams   :: Map Text Expr
  , apramStatuses :: Map Column [Status]
  , apramCohorts  :: [Cohort]
  , apramMods     :: [Mod]
  }
  deriving Show

data Mod = Mod
  { modName    :: Text 
  , modCohort  :: Cohort
  , modActions :: [(ActionSequence, ProbSpec)]
  , modPhase   :: Text
  }
  deriving Show

data Cohort = Cohort
  { cohortName :: Text 
  , cohortExpr :: CohortExpr
  }
  deriving (Eq, Show)

data CohortExpr =
    And CohortExpr CohortExpr
  | Or  CohortExpr CohortExpr
  | Is  Column Status
  | Not Column Status
  | All
  deriving (Eq, Show)

data ActionSequence =
    Actions [Action]
  | Pass
  deriving Show

data Action =
    Assign Column Status
  deriving Eq

data ProbSpec =
    Probability Expr
  | Rate Expr
  | Unknown
  deriving (Show, Eq)

type Column = Text
type Status = Text

instance Show Action where
  show (Assign _ status) = "Assign_status_"<>unpack status<>"_to"