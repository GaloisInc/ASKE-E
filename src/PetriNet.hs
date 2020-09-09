-- | A continuous Petri Net as described in
-- http://eprints.gla.ac.uk/3476/1/gilbert.3476.pdf
{-# Language RecordWildCards #-}
module PetriNet where

import Data.Map(Map)
import qualified Data.Map as Map


type StateId       = Int
type R             = Double

-- | A description of a Petri Net
data PetriNet = PetriNet
  { netIntialState :: Map StateId R
    -- ^ Defines the states of the network, as well as theiry initial values.

  , netTransitions  :: [Transition]
  }

data Transition = Transition
  { transitionRate    :: Map StateId R -> R
    -- ^ The rate at which this transition fire.
    -- This is a function of the values of the inputs states.
    -- This function should return 0, if any of its inputs is 0,
    -- as this would mean that the transition is not enabled.

  , transitionInputs  :: Map StateId R
    -- ^ The key is the input state, and the value is the weight of this edge.

  , transitionOutputs :: Map StateId R
    -- ^ The key is the input state, and the value is the weight of this edge.
  }

--------------------------------------------------------------------------------
-- Simulation

-- | Current state of the system.
type State       = Map StateId R
type StateChange = Map StateId R

-- | Compute the change in a state based on a transition
transitionChange :: Transition -> State -> StateChange
transitionChange Transition { .. } s = Map.unionWith (+) inChange outChange
  where
  inS       = Map.restrictKeys s (Map.keysSet transitionInputs)
  rate      = transitionRate inS
  inChange  = (negate rate *) <$> transitionInputs
  outChange = (rate *)        <$> transitionOutputs

netStep :: PetriNet -> State -> State
netStep PetriNet { .. } s =
  Map.unionsWith (+) (s : [ transitionChange t s | t <- netTransitions ])

netTrace :: PetriNet -> [State]
netTrace net = iterate (netStep net) (netIntialState net)

