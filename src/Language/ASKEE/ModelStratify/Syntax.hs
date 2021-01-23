{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.ModelStratify.Syntax where

import Data.Aeson

import           Data.Map    ( Map )
import qualified Data.Map    as Map
import           Data.Text   ( Text )
import qualified Data.Vector as Vector

type StateID = Int
type TransitionID = Int

data Net =
  Net { netStates :: Map StateID Text
      , netTransitions :: Map TransitionID Text
      , netStateOutputs :: [Flow]
      , netTransitionOutputs :: [Flow]
      }
  deriving Show

data Flow =
    StateToTransition { flowState :: StateID
                      , flowTransition :: TransitionID
                      }
  | TransitionToState { flowState :: StateID
                      , flowTransition :: TransitionID
                      }
  deriving Show

data FlowType = I | O

instance FromJSON Net where
  parseJSON = withObject "Net" $ \v -> 
    do  s <- v .: "S"
        t <- v .: "T"
        i <- v .: "I"
        o <- v .: "O"
        Net <$> asStateMap s 
            <*> asTransMap t
            <*> asSToTFlows i 
            <*> asTToSFlows o

    where
      asTToSFlows = withArray "<state-to-transition flows>" arrayToTTSFlow
      arrayToTTSFlow arr = Vector.toList <$> Vector.mapM asTTSFlow arr
      asTTSFlow = withObject "<state-to-transition flow>" $ \v ->
        do  s <- v .: "os"
            t <- v .: "ot"
            pure $ TransitionToState s t

      asSToTFlows = withArray "<transition-to-state flows>" arrayToSTTFlow
      arrayToSTTFlow arr = Vector.toList <$> Vector.mapM asSTTFlow arr
      asSTTFlow = withObject "<transition-to-state flow>" $ \v ->
        do  s <- v .: "is"
            t <- v .: "it"
            pure $ StateToTransition s t

      asTransMap = withArray "<transitions>" arrayToTransMap
      arrayToTransMap arr = Map.fromList . Vector.toList <$> Vector.imapM asTransTuple arr
      asTransTuple i = withObject "<transition>" $ \s ->
        do  nm <- s .: "tname"
            pure (i + 1, nm)  

      asStateMap = withArray "<states>" arrayToStateMap
      arrayToStateMap arr = Map.fromList . Vector.toList <$> Vector.imapM asStateTuple arr
      asStateTuple i = withObject "<state>" $ \s ->
        do  nm <- s .: "sname"
            pure (i + 1, nm)  
        

instance ToJSON Net where
  toJSON Net{..} =
    object [ "S" .= map stateAsObj (Map.toAscList netStates) 
           , "T" .= map transitionAsObj (Map.toAscList netTransitions)
           , "I" .= map (flowAsObj I) netStateOutputs
           , "O" .= map (flowAsObj O) netTransitionOutputs
           ]

    where
      stateAsObj :: (StateID, Text) -> Value
      stateAsObj (_, nm) = object [ "sname" .= nm ]

      transitionAsObj :: (TransitionID, Text) -> Value
      transitionAsObj (_, nm) = object [ "tname" .= nm ]

      flowAsObj :: FlowType -> Flow -> Value
      flowAsObj I StateToTransition{..} = object  [ "is" .= flowState 
                                                  , "it" .= flowTransition
                                                  ]
      flowAsObj O TransitionToState{..} = object  [ "os" .= flowState 
                                                  , "ot" .= flowTransition
                                                  ]
      flowAsObj I TransitionToState{} = error "tried to serialize a transitionToState with the wrong tag (I)"
      flowAsObj O StateToTransition{} = error "tried to serialize a stateToTransition with the wrong tag (O)"


