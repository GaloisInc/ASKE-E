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
import           Language.ASKEE.ExprTransform
import qualified Language.ASKEE.Syntax as ESL
import Prelude hiding ( succ, fail )
import Control.Monad.Identity ( Identity(runIdentity) )

-- An attribute is something like "city" or "health"
-- A status is something like "Portland" or "infected"

-- Intermediate representation of a particular combination of ABM statuses
-- (values) across the space of attributes (keys)
newtype ESLStateVar = ESLStateVar 
  { statuses :: Map Text Text 
  }
  deriving (Eq, Ord, Show)

abmToModel :: ABM.Model -> ESL.Model
abmToModel ABM.Model{..} = ESL.Model name (lets++states) events
  where
    name = modelName
    lets = 
      [ ESL.Let v e
      | (v, e) <- Map.toList modelLets
      ]
    states = map stateDecl (allStates agentAttrs)
    events = concatMap (translateEvent agentAttrs) modelEvents
    agentAttrs = 
      [ (attr, statuses)
      | (attr, ABM.AgentAttribute _ statuses) <- Map.toList modelAgent
      ]


translateEvent :: [(Text, [Text])] -> ABM.Event -> [ESL.Event]
translateEvent agentAttrs ABM.Event{..} = concatMap template relevantStates
  where
    -- `nub` because events might only touch a portion of those agents in
    -- a mapping, and we blindly explode into events for every single possible
    -- mapping of agent to state variables. This leads to duplicates that
    -- (at this point) we can't see until now.
    template :: Map Text [ESLStateVar] -> [ESL.Event]
    template agents = 
      nub $ 
        map (instantiateEvent . Map.fromList) (taggedCombos $ Map.toList agents)

    -- Instantiate an event at a particular mapping of agents to ESL state
    -- variables.
    instantiateEvent :: Map Text ESLStateVar -> ESL.Event
    instantiateEvent agentMapping = 
      ESL.Event eventName Nothing eventRate' (concatMap asStatement eventEffect) Nothing
      where
        asStatement :: ABM.AgentAssign -> [ESL.Statement]
        asStatement (ABM.AgentAssign (ABM.Attribute agentName agentAttr) (ABM.Status agentStatus)) =
          let ESLStateVar decAgentStatuses = agentMapping Map.! agentName
              incAgentStatuses = Map.insert agentAttr agentStatus decAgentStatuses
              decAgentName = stateName decAgentStatuses
              incAgentName = stateName incAgentStatuses
          in  [ (decAgentName, Var decAgentName `Sub` LitD 1)
              , (incAgentName, Var incAgentName `Add` LitD 1)
              ]
        asStatement _ = undefined

        eventRate' = runIdentity $ transformExpr spliceSizes eventRate

        -- Convert expressions like size(x) and size(x.city) into `Expr`s
        -- that ESL can recognize - namely, (sums of) state variables.
        spliceSizes :: Expr -> Identity Expr
        spliceSizes e =
          case e of
            Fn "size" [Var v] ->
              case Text.split (== '.') v of
                [agentName] -> 
                  let ESLStateVar thisAgentStatuses = agentMapping Map.! agentName
                  in  (pure . Var . stateName) thisAgentStatuses
                [agentName, agentAttr] ->
                  let ESLStateVar thisAgentStatuses = agentMapping Map.! agentName
                      thisStatus = thisAgentStatuses Map.! agentAttr
                      dummyAgent = Map.singleton "_" (ESLStateVar $ Map.singleton agentAttr thisStatus)
                      [foo] = explode [dummyAgent] agentAttrs
                  in  pure $ foldr1 Add (map (Var . stateName . statuses) (foo Map.! "_"))
                _ -> undefined
            _ -> pure e

    -- We'll map over these state variable mappings to instantiate
    -- our events
    relevantStates = agentSets agentAttrs eventWhen


allStates :: [(Text, [Text])] -> [ESLStateVar]
allStates agentAttrs = 
  [ ESLStateVar (Map.fromList combos)
  | combos <- taggedCombos agentAttrs
  ]


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


stateDecl :: ESLStateVar -> ESL.Decl
stateDecl (ESLStateVar ss) = ESL.State (stateName ss) (Var "???")


stateName :: Map Text Text -> Text
stateName = Text.intercalate "_" . Map.elems


-- | Provided a mapping of attribute names to their statuses, and an agent
-- expression, synthesize a list of every possible mapping of agent name
-- to ESL state variable that satisfies the agent expression
agentSets :: [(Text, [Text])] -> ABM.AgentExpr -> [Map Text [ESLStateVar]]
agentSets agentAttrs expr = 
  explode (search expr Map.empty [] (:) agentAttrs) agentAttrs


-- | Expand the provided "ground truth" agent -> state variable mappings
-- to include all possible combinations of attributes not mentioned in
-- agents' state variables. For example:
-- restrictionSets = 
--   [ Map.singleton "X" (ESLStateVar (Map.singleton "city" "PDX")) ]
-- agentAttrs = 
--   [ AgentAttribute "city" ["PDX", "SEA"]
--   , AgentAttribute "health" ["S", "I", "R"]
--   ]
-- --> 
-- [ Map.singleton "X" [ ESLStateVar (Map.fromList [("city","PDX"),("health","S")])
--                     , ESLStateVar (Map.fromList [("city","PDX"),("health","I")])
--                     , ESLStateVar (Map.fromList [("city","PDX"),("health","R")])
--                     ]]
explode :: [Map Text ESLStateVar] -> [(Text, [Text])] -> [Map Text [ESLStateVar]]
explode restrictionSets agentAttrs = map (Map.map go) restrictionSets
  where
    go :: ESLStateVar -> [ESLStateVar]
    go restrictionSet = filter (fits restrictionSet) allStateVariables

    allStateVariables = allStates agentAttrs

    fits :: ESLStateVar -> ESLStateVar -> Bool
    fits (ESLStateVar requiredStatuses) (ESLStateVar actualStatuses) = 
      Map.intersection actualStatuses requiredStatuses == requiredStatuses


search :: ABM.AgentExpr 
       -> Map Text ESLStateVar
       -> [Map Text ESLStateVar]
       -> (Map Text ESLStateVar -> [Map Text ESLStateVar] -> [Map Text ESLStateVar]) 
       -> [(Text, [Text])]
       -> [Map Text ESLStateVar]
search expr curr fail succ allAttrs =
  case expr of
    -- x.city == pdx
    ABM.Eq (ABM.Attribute agentName agentAttr) (ABM.Status agentStatus) -> 
      doSimpleEq agentName agentAttr agentStatus
    -- pdx == x.city
    ABM.Eq (ABM.Status agentStatus) (ABM.Attribute agentName agentAttr) ->
      doSimpleEq agentName agentAttr agentStatus
    -- x.city == y.city
    ABM.Eq (ABM.Attribute n1 a1) (ABM.Attribute n2 a2) | a1 == a2 ->
      doComplexEq n1 n2 a1
    ABM.And e1 e2 -> 
      search e1 curr fail (\cur res -> search e2 cur res succ allAttrs) allAttrs
    ABM.Or e1 e2 -> 
      search e1 curr (search e2 curr fail succ allAttrs) succ allAttrs
    _ -> undefined 
  where
    doComplexEq :: Text -> Text -> Text -> [Map Text ESLStateVar]
    doComplexEq agentName1 agentName2 agentAttr =
      case (curr Map.!? agentName1, curr Map.!? agentName2) of
        (Nothing, Nothing) -> 
          -- These agents don't exist in the mapping
          randomStatuses potentialStatuses $
            Map.fromList 
              [ (agentName1, Map.empty)
              , (agentName2, Map.empty)
              ]
        (Just (ESLStateVar currentAttrs1), Nothing) ->
          -- agentName1 exists in the mapping - find a matching 
          -- status for agentName2
          makeStatusForAgent currentAttrs1 agentName2
        (Nothing, Just (ESLStateVar currentAttrs2)) ->
          -- agentName2 exists in the mapping - find a matching 
          -- status for agentName1
          makeStatusForAgent currentAttrs2 agentName1
        (Just (ESLStateVar currentAttrs1), Just (ESLStateVar currentAttrs2)) -> 
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
                  newState = ESLStateVar newAttrs
                  newAgentMap = Map.insert agentName2 newState curr
              in succ newAgentMap fail
            (Nothing, Just s) ->
              -- ...and agentName2 defines the attribute in question
              let newAttrs = Map.singleton agentAttr s
                  newState = ESLStateVar newAttrs
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
        randomStatuses :: [Text] -> Map Text (Map Text Text) -> [Map Text ESLStateVar]
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
            fill :: Text -> Text -> Map Text Text -> Map Text ESLStateVar -> Map Text ESLStateVar
            fill status agentName agentAttrs cur =
              let newAttrs = Map.insert agentAttr status agentAttrs
                  newState = ESLStateVar newAttrs
                  newAgentMap = Map.insert agentName newState cur
              in  newAgentMap

        -- | Given a set of attribute-status bindings for one agent, either
        -- find the correct attribute-status binding for the other agent or,
        -- if the attribute doesn't exist in the bindings, pick a new one
        makeStatusForAgent :: Map Text Text -> Text -> [Map Text ESLStateVar]
        makeStatusForAgent boundAgentAttrs unboundAgentName =
          case boundAgentAttrs Map.!? agentAttr of
            Just s ->
              -- The already-bound agent has a status `s` associated with
              -- this attribute
              let newAttrs = Map.singleton agentAttr s
                  newState = ESLStateVar newAttrs
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
          concatMap (\(_, statuses) -> statuses) $
            filter (\(attr, _) -> attr == agentAttr) allAttrs

        otherAgent :: Text -> Text
        otherAgent agentName
          | agentName == agentName1 = agentName2
          | agentName == agentName2 = agentName1
          | otherwise = error "attempted to find other agent for nonexistent agent"

    doSimpleEq :: Text -> Text -> Text -> [Map Text ESLStateVar]
    doSimpleEq agentName agentAttr agentStatus =
      case curr Map.!? agentName of
        -- The agent doesn't exist in the mapping
        Nothing -> 
          let newAttrs = Map.singleton agentAttr agentStatus
              newState = ESLStateVar newAttrs
              newAgentMap = Map.insert agentName newState curr
          in  succ newAgentMap fail
        Just (ESLStateVar currentAttrs) ->
          -- The agent does exist in the mapping...
          case currentAttrs Map.!? agentAttr of
            Just s | s == agentStatus -> 
              -- ...and defines the attribute correctly
              succ curr fail
            Nothing ->
              -- ...and doesn't define the attribute of interest
              let newAttrs = Map.insert agentAttr agentStatus currentAttrs
                  newState = ESLStateVar newAttrs
                  newAgentMap = Map.insert agentName newState curr
              in  succ newAgentMap fail
            _ -> 
              -- ...and defines the attribute incorrectly
              fail