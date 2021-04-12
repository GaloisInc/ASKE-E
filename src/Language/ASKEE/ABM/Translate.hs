{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Language.ASKEE.ABM.Translate where

import           Data.List ( nub )
import qualified Data.Map  as Map
import           Data.Map  ( Map )
import qualified Data.Text as Text
import           Data.Text ( Text )

import qualified Language.ASKEE.ABM.Syntax as ABM
import           Language.ASKEE.Expr
import qualified Language.ASKEE.Syntax as ESL
import Language.ASKEE.ABM.Sample
import Debug.Trace
import Prelude hiding ( succ, fail )

-- Intermediate representation of a particular combination of ABM statuses
-- (values) across the space of attributes (keys)
newtype ESLState = ESLState 
  { statuses :: Map Text Text 
  }
  deriving (Eq, Ord, Show)

abmToModel :: ABM.Model -> ESL.Model
abmToModel ABM.Model{..} = ESL.Model name (lets++states) events
  where
    name = modelName
    lets = 
      [ ESL.Let v e
      | (v, e) <- modelLets
      ]
    states = map stateDecl (allStates modelAgent)
    events = concatMap (translateEvent modelAgent) modelEvents


translateEvent :: [ABM.AgentAttribute] -> ABM.Event -> [ESL.Event]
translateEvent agentAttrs ABM.Event{..} = concatMap template relevantStates
  where
    template :: Map Text [ESLState] -> [ESL.Event]
    template agents = 
      nub $ 
        map (instantiateEvent . Map.fromList) (taggedCombos $ Map.toList agents)

    instantiateEvent :: Map Text ESLState -> ESL.Event
    instantiateEvent agentMapping = 
      ESL.Event eventName Nothing eventRate (concatMap asStatement eventEffect) Nothing
      where
        asStatement :: ABM.AgentAssign -> [ESL.Statement]
        asStatement (ABM.AgentAssign (ABM.Attribute agentName agentAttr) (ABM.Status agentStatus)) =
          let ESLState decAgentStatuses = agentMapping Map.! agentName
              incAgentStatuses = Map.insert agentAttr agentStatus decAgentStatuses
              decAgentName = stateName decAgentStatuses
              incAgentName = stateName incAgentStatuses
          in  [ (decAgentName, Var decAgentName `Sub` LitD 1)
              , (incAgentName, Var incAgentName `Add` LitD 1)
              ]
        asStatement _ = undefined


    relevantStates = agentSets agent eventWhen

    relevantStates = agentSets agentAttrs eventWhen


allStates :: [ABM.AgentAttribute] -> [ESLState]
allStates agentAttrs = 
  [ ESLState (Map.fromList combos)
  | combos <- taggedCombos (map flatten agentAttrs) 
  ]
  where
    flatten :: ABM.AgentAttribute -> (Text, [Text])
    flatten (ABM.AgentAttribute a ss) = (a,ss)


-- Generate every possible combination, Cartesian-product style, of `b`s, 
-- propagating their `a` tags
-- e.g. [(1, [2,3]), (2, [4])] -> [[(1,2),(2,4)],[(1,3),(2,4)]]
-- Adapted from https://mail.haskell.org/pipermail/haskell-cafe/2012-October/104127.html
taggedCombos :: [(a, [b])] -> [[(a, b)]]
taggedCombos xs = filter ((== length xs) . length) (go xs)
  where
    go [] = [[]]
    go ((_, []):ls) = go ls
    go ((tag, h:t):ls) = map ((tag, h):) (go ls) ++ go ((tag, t):ls)


stateDecl :: ESLState -> ESL.Decl
stateDecl (ESLState ss) = ESL.State (stateName ss) (Var "???")


stateName :: Map Text Text -> Text
stateName = Text.intercalate "_" . Map.elems


agentSets :: [ABM.AgentAttribute] -> ABM.AgentExpr -> [Map Text [ESLState]]
agentSets agentAttrs expr = 
  explode (search expr Map.empty [] (:) agentAttrs) agentAttrs


-- | Expand the provided agent -> status
explode :: [Map Text ESLState] -> [ABM.AgentAttribute] -> [Map Text [ESLState]]
explode restrictionSets agent = map (Map.map go) restrictionSets
  where
    go :: ESLState -> [ESLState]
    go restrictionSet = filter (fits restrictionSet) states

    states = allStates agent

    fits :: ESLState -> ESLState -> Bool
    fits (ESLState requiredStatuses) (ESLState actualStatuses) = 
      Map.intersection actualStatuses requiredStatuses == requiredStatuses


search :: ABM.AgentExpr 
       -> Map Text ESLState
       -> [Map Text ESLState]
       -> (Map Text ESLState -> [Map Text ESLState] -> [Map Text ESLState]) 
       -> [ABM.AgentAttribute]
       -> [Map Text ESLState]
search expr curr fail succ allAttrs =
  case expr of
    ABM.Eq (ABM.Attribute agentName agentAttr) (ABM.Status agentStatus) -> 
      doSimpleEq agentName agentAttr agentStatus
    ABM.Eq (ABM.Status agentStatus) (ABM.Attribute agentName agentAttr) ->
      doSimpleEq agentName agentAttr agentStatus
    ABM.Eq (ABM.Attribute n1 a1) (ABM.Attribute n2 a2) | a1 == a2 ->
      doComplexEq n1 n2 a1
    ABM.And e1 e2 -> 
      search e1 curr fail (\cur res -> search e2 cur res succ allAttrs) allAttrs
    ABM.Or e1 e2 -> 
      search e1 curr (search e2 curr fail succ allAttrs) succ allAttrs
    _ -> undefined 
  where
    doComplexEq :: Text -> Text -> Text -> [Map Text ESLState]
    doComplexEq agentName1 agentName2 agentAttr =
      case (curr Map.!? agentName1, curr Map.!? agentName2) of
        (Nothing, Nothing) -> 
          -- These agents don't exist in the mapping
          randomStatuses potentialStatuses $
            Map.fromList 
              [ (agentName1, Map.empty)
              , (agentName2, Map.empty)
              ]
        (Just (ESLState currentAttrs1), Nothing) ->
          -- agentName1 exists in the mapping - find a matching 
          -- status for agentName2
          makeStatusForAgent currentAttrs1 agentName2
        (Nothing, Just (ESLState currentAttrs2)) ->
          -- agentName2 exists in the mapping - find a matching 
          -- status for agentName1
          makeStatusForAgent currentAttrs2 agentName1
        (Just (ESLState currentAttrs1), Just (ESLState currentAttrs2)) -> 
          -- Both agents exist in the mapping...
          case (currentAttrs1 Map.!? agentAttr, currentAttrs2 Map.!? agentAttr) of
            (Nothing, Nothing) -> 
              -- ...and neither of them define the attribute in question
              -- randomStatus agentAttr potentialStatuses currentAttrs1 currentAttrs2
              randomStatuses potentialStatuses $
                Map.fromList 
                  [ (agentName1, currentAttrs1)
                  , (agentName2, currentAttrs2)
                  ]
            (Just s, Nothing) ->
              -- ...and agentName1 defines the attribute in question
              let newAttrs = Map.singleton agentAttr s
                  newState = ESLState newAttrs
                  newAgentMap = Map.insert agentName2 newState curr
              in succ newAgentMap fail
            (Nothing, Just s) ->
              -- ...and agentName2 defines the attribute in question
              let newAttrs = Map.singleton agentAttr s
                  newState = ESLState newAttrs
                  newAgentMap = Map.insert agentName1 newState curr
              in succ newAgentMap fail
            (Just s1, Just s2) | s1 == s2 -> 
              -- ...and both define the attribute in question
              succ curr fail
            _ -> fail
      where
        -- | From a list of potential statuses, pick the first and assign it
        -- to the attribute in question for agents' attributes in the 
        -- `agents` map
        randomStatuses :: [Text] -> Map Text (Map Text Text) -> [Map Text ESLState]
        randomStatuses candidateStatuses agents =
          case candidateStatuses of
            [] -> 
              -- No candidate statuses left to assign
              fail
            (s:ss) -> 
              -- Some candidate statuses left to assign
              let newAgentMap = Map.foldrWithKey (fill s) curr agents
              in  succ newAgentMap (randomStatuses ss agents)
          where
            fill :: Text -> Text -> Map Text Text -> Map Text ESLState -> Map Text ESLState
            fill status agentName agentAttrs cur =
              let newAttrs = Map.insert agentAttr status agentAttrs
                  newState = ESLState newAttrs
                  newAgentMap = Map.insert agentName newState cur
              in  newAgentMap

        -- | Given a set of attribute-status bindings for one agent, either
        -- find the correct attribute-status binding for the other agent or,
        -- if the attribute doesn't exist in the bindings, pick a new one
        makeStatusForAgent :: Map Text Text -> Text -> [Map Text ESLState]
        makeStatusForAgent boundAgentAttrs unboundAgentName =
          case boundAgentAttrs Map.!? agentAttr of
            Just s ->
              -- The already-bound agent has a status `s` associated with
              -- this attribute
              let newAttrs = Map.singleton agentAttr s
                  newState = ESLState newAttrs
                  newAgentMap = Map.insert unboundAgentName newState curr
              in succ newAgentMap fail
            Nothing -> 
              -- The already-bound agent has no status associated with
              -- this attribute
              randomStatuses potentialStatuses $
                Map.fromList 
                  [ (unboundAgentName, Map.empty)
                  , (otherAgent unboundAgentName, boundAgentAttrs)]

        -- | All potential statuses for the attribute name provided 
        -- to `doComplexEq`
        potentialStatuses :: [Text]
        potentialStatuses = 
          concatMap ABM.attributeStatuses $
            filter (\(ABM.AgentAttribute attr _) -> attr == agentAttr) allAttrs

        otherAgent :: Text -> Text
        otherAgent agentName
          | agentName == agentName1 = agentName2
          | agentName == agentName2 = agentName1
          | otherwise = error "attempted to find other agent for nonexistent agent"

    doSimpleEq :: Text -> Text -> Text -> [Map Text ESLState]
    doSimpleEq agentName agentAttr agentStatus =
      case curr Map.!? agentName of
        -- The agent doesn't exist in the mapping
        Nothing -> 
          let newAttrs = Map.singleton agentAttr agentStatus
              newState = ESLState newAttrs
              newAgentMap = Map.insert agentName newState curr
          in  succ newAgentMap fail
        Just (ESLState currentAttrs) ->
          -- The agent does exist in the mapping...
          case currentAttrs Map.!? agentAttr of
            Just s | s == agentStatus -> 
              -- ...and defines the attribute correctly
              succ curr fail
            Nothing ->
              -- ...and doesn't define the attribute of interest
              let newAttrs = Map.insert agentAttr agentStatus currentAttrs
                  newState = ESLState newAttrs
                  newAgentMap = Map.insert agentName newState curr
              in  succ newAgentMap fail
            _ -> 
              -- ...and defines the attribute incorrectly
              fail