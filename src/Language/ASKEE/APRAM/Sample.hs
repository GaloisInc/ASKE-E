{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.APRAM.Sample where

import Language.ASKEE.APRAM.Syntax as APRAM
import Language.ASKEE.Expr

healthColumn :: Column
healthColumn = "health"

susceptibleStatus, exposedStatus, infectedStatus, recoveredStatus, deadStatus :: Status
susceptibleStatus = Status healthColumn "sus"
exposedStatus     = Status healthColumn "exp"
infectedStatus    = Status healthColumn "inf"
recoveredStatus   = Status healthColumn "rec"
deadStatus        = Status healthColumn "dead"

healthStatuses :: [Status]
healthStatuses = [susceptibleStatus, exposedStatus, infectedStatus, recoveredStatus, deadStatus]


quarantineColumn :: Column
quarantineColumn = "quarantine"

notQuarantinedStatus, quarantinedStatus :: Status
notQuarantinedStatus = Status quarantineColumn "not_quarantined"
quarantinedStatus    = Status quarantineColumn "quarantined"

quarantineStatuses :: [Status]
quarantineStatuses = [quarantinedStatus, notQuarantinedStatus]


-- everExposedColumn :: Column 
-- everExposedColumn = "ever_exposed"

-- neverExposedStatus, everExposedStatus :: Status
-- neverExposedStatus = Status everExposedColumn "never_exposed"
-- everExposedStatus = Status everExposedColumn "ever_exposed"

-- everExposedStatuses :: [Status]
-- everExposedStatuses = [neverExposedStatus, everExposedStatus]


susCohort, expCohort, infCohort, recCohort, deadCohort, infectiousCohort, quarantinedCohort :: Cohort
susCohort         = Is susceptibleStatus
expCohort         = Is exposedStatus
infCohort         = Is infectedStatus
recCohort         = Is recoveredStatus
deadCohort        = Is deadStatus
infectiousCohort  = APRAM.Not quarantinedStatus `APRAM.And` (Is exposedStatus `APRAM.Or` Is infectedStatus)
quarantinedCohort = Is quarantinedStatus
-- everExposedCohort = Is everExposedStatus

quarantineMod :: Mod
quarantineMod = Mod{..}
  where
    modName = "quarantine"
    modCohort = infectiousCohort
    modActions =
      [ (Actions [Assign quarantineColumn quarantinedStatus], LitD 0.1)
      , (Pass,                                                LitD 0.9)
      ]
    modPhase = "loop"

leaveQuarantineMod :: Mod 
leaveQuarantineMod = Mod{..}
  where
    modName = "leave_quarantine"
    modCohort = quarantinedCohort
    modActions =
      [ (Actions [Assign quarantineColumn notQuarantinedStatus], LitD 0.5)
      , (Pass,                                                   LitD 0.5)
      ]
    modPhase = "loop"

seedInfectionsMod :: Mod
seedInfectionsMod = Mod{..}
  where
    modName = "seed_infections"
    modCohort = susCohort
    modActions =
      [ (Actions [Assign healthColumn exposedStatus], LitD 0.0001)
      , (Pass,                                        LitD 0.9999)
      ]
    modPhase= "setup"

exposeMod :: Mod
exposeMod = Mod{..}
  where
    modName = "expose"
    modCohort = susCohort
    modActions =
      [ (Actions [Assign healthColumn exposedStatus], Var "alpha")
      , (Pass,                                        LitD 1 `Sub` Var "alpha")
      ]
    modPhase= "loop"

takeIllMod :: Mod
takeIllMod = Mod{..}
  where
    modName = "take_ill"
    modCohort = expCohort
    modActions =
      [ (Actions [Assign healthColumn infectedStatus], Var "beta")
      , (Pass,                                         LitD 1 `Sub` Var "beta")
      ]
    modPhase= "loop"

remainBlissfullyUnawareMod :: Mod
remainBlissfullyUnawareMod = Mod{..}
  where
    modName = "remain_blissfully_unaware"
    modCohort = expCohort
    modActions =
      [ (Actions [Assign healthColumn recoveredStatus], Var "gamma")
      , (Pass,                                          LitD 1 `Sub` Var "gamma")
      ]
    modPhase= "loop"

recoverMod :: Mod
recoverMod = Mod{..}
  where
    modName = "recover"
    modCohort = infCohort
    modActions =
      [ (Actions [Assign healthColumn recoveredStatus], Var "delta")
      , (Pass,                                          LitD 1 `Sub` Var "delta")
      ]
    modPhase= "loop"

dieMod :: Mod
dieMod = Mod{..}
  where
    modName = "die"
    modCohort = infCohort
    modActions =
      [ (Actions [Assign healthColumn deadStatus], Var "epsilon")
      , (Pass,                                     LitD 1 `Sub` Var "epsilon")
      ]
    modPhase= "loop"

sampleAPRAM :: APRAM
sampleAPRAM = APRAM{..}
  where
    apramAgents = 100000
    apramColumns = [healthColumn, quarantineColumn]
    apramCohorts = [susCohort, expCohort, infCohort, recCohort, deadCohort, infectiousCohort, quarantinedCohort]
    apramMods = [ exposeMod
                , takeIllMod
                , remainBlissfullyUnawareMod
                , recoverMod
                , dieMod
                , quarantineMod
                , leaveQuarantineMod
                -- , seedInfectionsMod
                ]

