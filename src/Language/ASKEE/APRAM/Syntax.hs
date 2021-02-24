module Language.ASKEE.APRAM.Syntax where

import Data.Map ( Map )

import Language.ASKEE.Expr ( Expr )
  
data APRAM = APRAM
  { apramAgents   :: Int
  , apramStatuses :: Map Column [Status]
  , apramCohorts  :: [Cohort]
  , apramMods     :: [Mod]
  }
  deriving Show

data Mod = Mod
  { modName    :: String 
  , modCohort  :: Cohort
  , modActions :: [(ActionSequence, Expr)]
  , modPhase   :: String
  }
  deriving Show

data Cohort = 
    And Cohort Cohort
  | Or  Cohort Cohort
  | Is  Column Status
  | Not Column Status
  deriving Show

data ActionSequence =
    Actions [Action]
  | Pass
  deriving Show

data Action =
    Assign Column Status
  deriving Eq

type Column = String
type Status = String

instance Show Action where
  show (Assign _ status) = "Assign_status_"<>status<>"_to"