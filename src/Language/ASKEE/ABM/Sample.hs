{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.ABM.Sample where

import Data.Text ( Text )
import Language.ASKEE.ABM.Syntax
import Language.ASKEE.Expr ( Expr(LitD, Var, Sub, Div) )

sampleModel :: Model
sampleModel = Model{..}
  where
    modelName = "SIR_Seattle_Portland"
    modelAgent = sampleAgent
    modelLets = [("pop_size", LitD 1e5)]
    modelInit = sampleInits
    modelEvents = sampleEvents

sampleAgent :: [AgentAttribute] 
sampleAgent =
  [ AgentAttribute "health" ["S", "E", "I", "R"]
  , AgentAttribute "city" ["sea", "pdx"]
  -- , AgentAttribute "age" ["0-30", "31-60", "61-90"]
  -- , AgentAttribute "quarantine" ["quarantined", "not_quarantined"]
  ]

sampleInits :: [(Text, Expr)]
sampleInits =
  [ ("S", Div (Sub (Var "pop_size") (LitD 3)) (Var "pop_size"))
  , ("E",     Div                   (LitD 3)  (Var "pop_size"))
  , ("I", LitD 0)
  , ("R", LitD 0)
  , ("sea", Div (LitD 2) (LitD 3))
  , ("pdx", Div (LitD 1) (LitD 3))
  ]

sampleEvents :: [Event]
sampleEvents = [expose_e, expose_i, infect, remit, recover]
  where
    expose_e :: Event
    expose_e = Event
      "Expose_e" 
      ["x", "y"] 
      (foldr1 And
        [ Eq (Attribute "x" "city") (Attribute "y" "city")
        , Eq (Attribute "x" "health") (Status "S")
        , Eq (Attribute "y" "health") (Status "E")
        ])
      (LitD 0.4)
      [AgentAssign (Attribute "x" "health") (Status "E")]

    expose_i :: Event
    expose_i = Event
      "Expose_i"
      ["x", "y"] 
      (foldr1 And
        [ Eq (Attribute "x" "city") (Attribute "y" "city")
        , Eq (Attribute "x" "health") (Status "S")
        , Eq (Attribute "y" "health") (Status "I")
        ])
      (LitD 0.4)
      [AgentAssign (Attribute "x" "health") (Status "E")]

    infect :: Event
    infect = Event
      "Infect"
      ["x"] 
      (Eq (Attribute "x" "health") (Status "E"))
      (LitD 0.5) 
      [AgentAssign (Attribute "x" "health") (Status "I")]

    remit :: Event
    remit = Event
      "Remit"
      ["x"] 
      (Eq (Attribute "x" "health") (Status "E"))
      (LitD 0.5) 
      [AgentAssign (Attribute "x" "health") (Status "R")]

    recover :: Event
    recover = Event
      "Recover"
      ["x"] 
      (Eq (Attribute "x" "health") (Status "I"))
      (LitD 0.8) 
      [AgentAssign (Attribute "x" "health") (Status "R")]