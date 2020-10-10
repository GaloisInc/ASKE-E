module Main where

import Language.ASKEE.Core

testModel :: Model
testModel =
  Model { modelInitState = 
          [ (susceptibleName, 990.0)
          , (infectedName, 10.0)
          , (recoveredName, 0.0)
          ]

        , modelEvents =
          [
            -- Infect
            Event { eventRate = beta `ExprMul` (lit 1.0 `ExprSub` contact_rate) `ExprMul` susceptible `ExprMul` infected `ExprDiv` total_pop
                  , eventWhen = (susceptible `ExprGT` lit 0.0) `ExprAnd` (infected `ExprGT` lit 0.0)
                  , eventEffect =
                    [ (susceptibleName, susceptible `ExprSub` lit 1.0 )
                    , (infectedName, infected `ExprAdd` lit 1.0)
                    ]
                  }

          , Event { eventRate = gamma `ExprMul` infected
                  , eventWhen = infected `ExprGT` lit 0.0
                  , eventEffect =
                    [ (infectedName, infected `ExprSub` lit 1.0)
                    , (recoveredName, recovered `ExprAdd` lit 1.0)
                    ]
                  }
          ]
        }
  where
    lit = ExprNumLit
    susceptibleName = 1
    infectedName = 2
    recoveredName = 3
    susceptible = ExprVar susceptibleName
    infected = ExprVar infectedName
    recovered = ExprVar recoveredName
    beta = ExprNumLit 0.0
    contact_rate = ExprNumLit 0.05
    total_pop =
      susceptible `ExprAdd` infected `ExprAdd` recovered
    gamma = ExprNumLit (1.0 / 14.0)

main :: IO ()
main = 
  do  _ <- runModel 1000000 testModel
      pure ()