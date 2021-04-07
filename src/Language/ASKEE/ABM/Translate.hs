{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Language.ASKEE.ABM.Translate where

import           Data.List ( intercalate )
import qualified Data.Map  as Map
import           Data.Map  ( Map )
import qualified Data.MultiSet as MSet
import           Data.MultiSet ( MultiSet )
import qualified Data.Set  as Set
import           Data.Set  ( Set )
import qualified Data.Text as Text
import           Data.Text ( Text, pack )

import qualified Language.ASKEE.ABM.Syntax as ABM
import           Language.ASKEE.Expr
import qualified Language.ASKEE.Syntax as ESL
import Language.ASKEE.ABM.Sample

-- Intermediate representation of a particular combination of ABM statuses
-- (values) across the space of attributes (keys)
newtype ESLState = ESLState 
  { statuses :: Map Text Text 
  }
  deriving (Eq, Ord, Show)

abmToModel :: ABM.Model -> ESL.Model
abmToModel ABM.Model{..} = ESL.Model name (lets++states) []
  where
    name = modelName
    lets = 
      [ ESL.Let v e
      | (v, e) <- modelLets
      ]
    states = map stateDecl (allStates modelAgent)

translateEvent :: ABM.Event -> [ESL.Event]
translateEvent = undefined 

-- unpackESLState :: ESLState -> [String]
-- unpackESLState (ESLState statuses) = Map.elems statuses

generateMultiSets :: [ESLState] -> Int -> Set (MultiSet ESLState)
generateMultiSets options 1 = Set.fromList $ map MSet.singleton options
generateMultiSets options n = Set.unions $ map go options
  where
    go :: ESLState -> Set (MultiSet ESLState)
    go s = Set.map (MSet.insert s) (generateMultiSets options (n - 1))

allStates :: ABM.Agent -> [ESLState]
allStates (ABM.Agent agentAttrs) = 
  [ ESLState (Map.fromList combos)
  | combos <- taggedCombos (map flatten agentAttrs) ]

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
  -- where
stateName :: Map Text Text -> Text
stateName = Text.intercalate "_" . Map.elems


agentSets :: ABM.Agent -> ABM.AgentExpr -> [Map Text [ESLState]]
agentSets agent expr = explode (search expr Map.empty [] (:) agent) agent

explode :: [Map Text ESLState] -> ABM.Agent -> [Map Text [ESLState]]
explode restrictionSets agent = map (Map.map go) restrictionSets
  where
    go :: ESLState -> [ESLState]
    go restrictionSet = filter (fits restrictionSet) states

    states = allStates agent

    fits :: ESLState -> ESLState -> Bool
    fits (ESLState requiredStatuses) (ESLState actualStatuses) = 
      Map.intersection actualStatuses requiredStatuses == requiredStatuses

-- foo expr = explode (search expr Map.empty [] (:) agent) agent

simpleEq1 :: ABM.AgentExpr
simpleEq2 :: ABM.AgentExpr
simpleEq3 :: ABM.AgentExpr
simpleEq :: ABM.AgentExpr
simpleEq1 = ABM.Eq (ABM.Attribute "x" "health") (ABM.Status "S")
simpleEq2 = ABM.Eq (ABM.Attribute "y" "health") (ABM.Status "I")
simpleEq3 = ABM.Eq (ABM.Attribute "y" "health") (ABM.Status "R")
simpleEq = foldr1 ABM.Or [simpleEq1, simpleEq2, simpleEq3]
-- x.city == y.city && ((x.health == S && y.health == I) || (x.health == S && y.health == R))
complexEq1 :: ABM.AgentExpr
complexEq2 :: ABM.AgentExpr
complexEq3 :: ABM.AgentExpr
complexEq4 :: ABM.AgentExpr
complexEq5 :: ABM.AgentExpr
complexEq6 :: ABM.AgentExpr
complexEq :: ABM.AgentExpr
complexEq1 = ABM.Eq (ABM.Attribute "x" "city") (ABM.Attribute "y" "city")
complexEq2 = ABM.Eq (ABM.Attribute "x" "city") (ABM.Status "sea")
complexEq3 = ABM.Eq (ABM.Attribute "y" "health") (ABM.Status "S")
complexEq4 = ABM.Eq (ABM.Attribute "x" "health") (ABM.Attribute "y" "health")
complexEq5 = ABM.Eq (ABM.Attribute "x" "city") (ABM.Attribute "y" "city")
complexEq6 = ABM.And complexEq4 complexEq5
complexEq = foldr1 ABM.And [complexEq1, complexEq2, complexEq3]

e1 = ABM.And complexEq1 simpleEq1
e2 = ABM.And simpleEq1 complexEq1


search :: ABM.AgentExpr 
       -> Map Text ESLState
       -> [Map Text ESLState]
       -> (Map Text ESLState -> [Map Text ESLState] -> [Map Text ESLState]) 
       -> ABM.Agent
       -> [Map Text ESLState]
search expr curr fail succ a@(ABM.Agent allAttrs) =
  case expr of
    ABM.Eq (ABM.Attribute agentName agentAttr) (ABM.Status agentStatus) -> 
      doSimpleEq agentName agentAttr agentStatus
    ABM.Eq (ABM.Status agentStatus) (ABM.Attribute agentName agentAttr) ->
      doSimpleEq agentName agentAttr agentStatus
    ABM.Eq (ABM.Attribute n1 a1) (ABM.Attribute n2 a2) | a1 == a2 ->
      doComplexEq n1 n2 a1
    ABM.And e1 e2 -> 
      search e1 curr fail (\cur res -> search e2 cur res succ a) a
    ABM.Or e1 e2 -> 
      search e1 curr (search e2 curr fail succ a) succ a
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
        (Just (ESLState currentAttrs), Nothing) ->
          -- agentName1 exists in the mapping - find a matching 
          -- status for agentName2
          makeStatusForAgent currentAttrs agentName2
        (Nothing, Just (ESLState currentAttrs)) ->
          -- agentName2 exists in the mapping - find a matching 
          -- status for agentName1
          makeStatusForAgent currentAttrs agentName1
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
              -- The already-bound agent has a status associated with
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


factorial :: Int -> Int
factorial x = product [1..x]

choose :: Int -> Int -> Int
choose n k = factorial n `div` (factorial k * factorial (n - k))

mchoose :: Int -> Int -> Int
mchoose n k = choose (n + k - 1) k