module Language.ASKEE.APRAM.Syntax where

import Data.Map ( Map )

import Language.ASKEE.Expr ( Expr )
  
data APRAM = APRAM
  { apramAgents   :: Int
  , apramParams   :: Map String Expr
  , apramStatuses :: Map Column [Status]
  , apramCohorts  :: [Cohort]
  , apramMods     :: [Mod]
  }
  deriving Show

data Mod = Mod
  { modName    :: String 
  , modCohort  :: Cohort
  , modActions :: [(ActionSequence, ProbSpec)]
  , modPhase   :: String
  }
  deriving Show

data Cohort = Cohort
  { cohortName :: String 
  , cohortExpr :: CohortExpr
  }
  deriving Show

data CohortExpr =
    And CohortExpr CohortExpr
  | Or  CohortExpr CohortExpr
  | Is  Column Status
  | Not Column Status
  | All
  deriving Show

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
  deriving Show

type Column = String
type Status = String

instance Show Action where
  show (Assign _ status) = "Assign_status_"<>status<>"_to"