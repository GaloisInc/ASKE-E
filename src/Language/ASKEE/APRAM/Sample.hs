{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.APRAM.Sample where

import qualified Data.Map as Map
import           Data.Map ( Map )

import Language.ASKEE.APRAM.Syntax as APRAM
import Language.ASKEE.Expr

statuses :: Map Column [Status]
statuses = Map.fromList 
  [ (healthColumn,      [ susceptibleStatus
                        , exposedStatus
                        , infectedStatus
                        , recoveredStatus
                        , deadStatus
                        ])
  , (quarantineColumn,  [ notQuarantinedStatus
                        , quarantinedStatus
                        ])
  ]

healthColumn :: Column
healthColumn = "health"

susceptibleStatus, exposedStatus, infectedStatus, recoveredStatus, deadStatus :: Status
susceptibleStatus = "susceptible"
exposedStatus     = "exposed"
infectedStatus    = "infected"
recoveredStatus   = "recovered"
deadStatus        = "dead"

quarantineColumn :: Column
quarantineColumn = "quarantine"

notQuarantinedStatus, quarantinedStatus :: Status
notQuarantinedStatus = "not_quarantined"
quarantinedStatus    = "quarantined"

susCohort, expCohort, infCohort, recCohort, deadCohort, infectiousCohort, quarantinedCohort, allCohort :: Cohort
susCohort         = Cohort "S" (Is healthColumn susceptibleStatus)
expCohort         = Cohort "E" (Is healthColumn exposedStatus)
infCohort         = Cohort "I" (Is healthColumn infectedStatus)
recCohort         = Cohort "R" (Is healthColumn recoveredStatus)
deadCohort        = Cohort "D" (Is healthColumn deadStatus)
infectiousCohort  = Cohort "F" (APRAM.Not quarantineColumn quarantinedStatus `APRAM.And` (Is healthColumn exposedStatus `APRAM.Or` Is healthColumn infectedStatus))
quarantinedCohort = Cohort "Q" (Is quarantineColumn quarantinedStatus)
allCohort         = Cohort "P" All -- XXX unused

quarantineMod :: Mod
quarantineMod = Mod{..}
  where
    modName = "quarantine"
    modCohort = infectiousCohort
    modActions =
      [ (Actions [Assign quarantineColumn quarantinedStatus], Probability $ LitD 0.1)
      , (Pass,                                                Probability $ LitD 0.9)
      ]
    modPhase = "loop"

leaveQuarantineMod :: Mod 
leaveQuarantineMod = Mod{..}
  where
    modName = "leave_quarantine"
    modCohort = quarantinedCohort
    modActions =
      [ (Actions [Assign quarantineColumn notQuarantinedStatus], Probability $ LitD 0.5)
      , (Pass,                                                   Probability $ LitD 0.5)
      ]
    modPhase = "loop"

seedInfectionsMod :: Mod
seedInfectionsMod = Mod{..}
  where
    modName = "seed_infections"
    modCohort = susCohort
    modActions =
      [ (Actions [Assign healthColumn exposedStatus], Probability $ LitD 0.0001)
      , (Pass,                                        Probability $ LitD 0.9999)
      ]
    modPhase= "setup"

exposeMod :: Mod
exposeMod = Mod{..}
  where
    modName = "expose"
    modCohort = susCohort
    modActions =
      [ (Actions [Assign healthColumn exposedStatus], Probability $ Var "alpha")
      , (Pass,                                        Probability $ LitD 1 `Sub` Var "alpha")
      ]
    modPhase= "loop"

takeIllMod :: Mod
takeIllMod = Mod{..}
  where
    modName = "take_ill"
    modCohort = expCohort
    modActions =
      [ (Actions [Assign healthColumn infectedStatus], Probability $ Var "beta")
      , (Pass,                                         Probability $ LitD 1 `Sub` Var "beta")
      ]
    modPhase= "loop"

remainBlissfullyUnawareMod :: Mod
remainBlissfullyUnawareMod = Mod{..}
  where
    modName = "remain_blissfully_unaware"
    modCohort = expCohort
    modActions =
      [ (Actions [Assign healthColumn recoveredStatus], Probability $ Var "gamma")
      , (Pass,                                          Probability $ LitD 1 `Sub` Var "gamma")
      ]
    modPhase= "loop"

recoverMod :: Mod
recoverMod = Mod{..}
  where
    modName = "recover"
    modCohort = infCohort
    modActions =
      [ (Actions [Assign healthColumn recoveredStatus], Probability $ Var "delta")
      , (Pass,                                          Probability $ LitD 1 `Sub` Var "delta")
      ]
    modPhase= "loop"

dieMod :: Mod
dieMod = Mod{..}
  where
    modName = "die"
    modCohort = infCohort
    modActions =
      [ (Actions [Assign healthColumn deadStatus], Probability $ Var "epsilon")
      , (Pass,                                     Probability $ LitD 1 `Sub` Var "epsilon")
      ]
    modPhase= "loop"

sampleAPRAM :: APRAM
sampleAPRAM = APRAM{..}
  where
    apramAgents = 100000
    apramParams = Map.fromList 
      [ ("alpha", LitD 0.1)
      , ("beta", LitD 0.4)
      , ("gamma", LitD 0.04)
      , ("delta", LitD 0.3)
      , ("epsilon", LitD 0.2)
      ]
    apramStatuses = statuses
    apramCohorts = 
      [ susCohort
      , expCohort
      , infCohort
      , recCohort
      , deadCohort
      , infectiousCohort
      , quarantinedCohort
      ]
    apramMods = 
      [ exposeMod
      , takeIllMod
      , remainBlissfullyUnawareMod
      , recoverMod
      , dieMod
      , quarantineMod
      , leaveQuarantineMod
      -- , seedInfectionsMod
      ]

