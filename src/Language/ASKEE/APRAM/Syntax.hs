module Language.ASKEE.APRAM.Syntax where

import Language.ASKEE.Expr ( Expr )
  
data APRAM = APRAM
  { apramAgents  :: Int
  , apramColumns :: [Column]
  , apramCohorts :: [Cohort]
  , apramMods    :: [Mod]
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
  | Is  Status
  | Not Status
  deriving Show

data ActionSequence =
    Actions [Action]
  | Pass
  deriving Show

data Action =
    Assign Column Status
  deriving Eq

type Column = String

data Status = Status Column String
  deriving (Eq, Ord)


instance Show Status where
  show (Status _ tag) = tag

instance Show Action where
  show (Assign _ status) = "Assign_status_"<>show status<>"_to"


-- data APRAM = APRAM
--   { columns :: [Column]
--   , cohorts :: [Cohort]
--   , mods :: [Mod]
--   }
--   deriving Show

-- type Status = Int

-- data Column = Column
--   { columnName :: Text
--   , columnStatus :: [Status]
--   }
--   deriving Show

-- data Cohort = Cohort
--   { cohortName :: Text
--   , cohortCondition :: CohortExpr
--   }
--   deriving Show

-- data CohortExpr =
--     Eq Status
--   | Ne Status
--   deriving Show

-- type CohortID = Text

-- data Mod = Mod
--   { modName :: Text
--   , modCohort :: CohortID
--   , modActions :: [Action]
--   , modProbabilities :: [Expr]
--   , modPhase :: ModPhase
--   }
--   deriving Show

-- data ModPhase = Setup | Loop 
--   deriving Show

-- data Action =
--     Assign CohortID Status -- is this a cohort or a column?
--   | Pass
--   deriving Show

-- data Expr =
--     Add Expr Expr
--   | Sub Expr Expr
--   | Mul Expr Expr
--   | Div Expr Expr
--   | Pow Expr Expr
--   | Lit Double
--   deriving Show