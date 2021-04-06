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
    modelAgent = agent
    modelLets = [("pop_size", LitD 1e5)]
    modelInit = inits
    modelEvents = events

agent :: Agent 
agent = Agent attrs
  where
    attrs =
      [ AgentAttribute "health" ["S", "I", "R"]
      , AgentAttribute "city" ["sea", "pdx"]
      -- , AgentAttribute "age" ["0-30", "31-60", "61-90"]
      -- , AgentAttribute "quarantine" ["quarantined", "not_quarantined"]
      ]

inits :: [(Text, Expr)]
inits =
  [ ("susceptible", Div (Sub (Var "pop_size") (LitD 3)) (Var "pop_size"))
  , ("exposed",     Div                       (LitD 3)  (Var "pop_size"))
  , ("infected", LitD 0)
  , ("recovered", LitD 0)
  , ("sea", Div (LitD 2) (LitD 3))
  , ("pdx", Div (LitD 1) (LitD 3))
  ]

events :: [Event]
events = [expose_e, expose_i, infect, remit, recover]
  where
    expose_e :: Event
    expose_e = Event
      "Expose_e" 
      ["x", "y"] 
      (foldr1 And
        [ Eq (Attribute "x" "city") (Attribute "y" "city")
        , Eq (Attribute "x" "city") (Status "susceptible")
        , Eq (Attribute "y" "city") (Status "exposed")
        ])
      (LitD 0.4)
      [AgentAssign (Attribute "x" "health") (Status "exposed")]
    
    expose_i :: Event
    expose_i = Event
      "Expose_i"
      ["x", "y"] 
      (foldr1 And
        [ Eq (Attribute "x" "city") (Attribute "y" "city")
        , Eq (Attribute "x" "city") (Status "susceptible")
        , Eq (Attribute "y" "city") (Status "infected")
        ])
      (LitD 0.4)
      [AgentAssign (Attribute "x" "health") (Status "exposed")]

    infect :: Event
    infect = Event
      "Infect"
      ["x"] 
      (Eq (Attribute "x" "health") (Status "exposed"))
      (LitD 0.5) 
      [AgentAssign (Attribute "x" "health") (Status "infected")]

    remit :: Event
    remit = Event
      "Remit"
      ["x"] 
      (Eq (Attribute "x" "health") (Status "exposed"))
      (LitD 0.5) 
      [AgentAssign (Attribute "x" "health") (Status "recovered")]

    recover :: Event
    recover = Event
      "Recover"
      ["x"] 
      (Eq (Attribute "x" "health") (Status "infected"))
      (LitD 0.8) 
      [AgentAssign (Attribute "x" "health") (Status "recovered")]