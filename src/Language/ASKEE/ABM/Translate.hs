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
stateDecl (ESLState ss) = ESL.State (mkName ss) (Var "???")
  where
    mkName :: Map Text Text -> Text
    mkName = Text.intercalate "_" . Map.elems


foo expr = go expr Map.empty (error "foo") (\cur res -> cur) agent

simpleEq1 = ABM.Eq (ABM.Attribute "x" "health") (ABM.Status "sus")
simpleEq2 = ABM.Eq (ABM.Attribute "y" "health") (ABM.Status "exp")
simpleEq3 = ABM.Eq (ABM.Attribute "y" "health") (ABM.Status "pdx")
simpleEq = foldr1 ABM.Or [simpleEq1, simpleEq2, simpleEq3]

complexEq1 = ABM.Eq (ABM.Attribute "x" "city") (ABM.Attribute "y" "city")
complexEq2 = ABM.Eq (ABM.Attribute "x" "city") (ABM.Status "sea")
complexEq3 = ABM.Eq (ABM.Attribute "y" "health") (ABM.Status "sea")
complexEq = foldr1 ABM.And [complexEq1, complexEq2, complexEq3]

go :: ABM.AgentExpr 
   -> Map Text ESLState
   -> Map Text ESLState
   -> (Map Text ESLState -> Map Text ESLState -> Map Text ESLState) 
   -> ABM.Agent
   -> Map Text ESLState
go expr curr fail succ a@(ABM.Agent allAttrs) =
  case expr of
    ABM.Eq (ABM.Attribute agentName agentAttr) (ABM.Status agentStatus) -> 
      doSimpleEq agentName agentAttr agentStatus
    ABM.Eq (ABM.Status agentStatus) (ABM.Attribute agentName agentAttr) ->
      doSimpleEq agentName agentAttr agentStatus
    ABM.Eq (ABM.Attribute n1 a1) (ABM.Attribute n2 a2) | a1 == a2 ->
      doComplexEq n1 n2 a1
    ABM.And e1 e2 -> go e1 curr fail (\cur res -> go e2 cur res succ a) a
    ABM.Or e1 e2 -> go e1 curr (go e2 curr fail succ a) succ a
    _ -> undefined 

  where
    doComplexEq :: Text -> Text -> Text -> Map Text ESLState
    doComplexEq agentName1 agentName2 agentAttr =
      case (curr Map.!? agentName1, curr Map.!? agentName2) of
        (Nothing, Nothing) -> 
          -- These agents don't exist in the mapping
          pickStatusForAgents potentialStatuses
        (Just (ESLState currentAttrs), Nothing) ->
          -- agentName1 exists in the mapping - find a matching 
          -- status for agentName1
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
              pickStatusForAgents potentialStatuses
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
        -- to the attribute in question for both agents
        pickStatusForAgents :: [Text] -> Map Text ESLState
        pickStatusForAgents _potentialStatuses =
          case _potentialStatuses of
            [] -> fail
            (s:ss) -> 
              let newAttrs = Map.singleton agentAttr s
                  newState = ESLState newAttrs
                  newAgentMap = Map.insert agentName1 newState (Map.insert agentName2 newState curr)
              in  succ newAgentMap (pickStatusForAgents ss)

        -- | Given a set of attribute-status bindings for one agent, either
        -- find the correct attribute-status binding for the other agent or,
        -- if the attribute doesn't exist in the bindings, pick a new one
        makeStatusForAgent :: Map Text Text -> Text -> Map Text ESLState
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
              pickStatusForAgents potentialStatuses

        -- | All potential statuses for the attribute name provided 
        -- to `doComplexEq`
        potentialStatuses :: [Text]
        potentialStatuses = 
          concatMap ABM.attributeStatuses $
            filter (\(ABM.AgentAttribute attr ss) -> attr == agentAttr) allAttrs

    doSimpleEq :: Text -> Text -> Text -> Map Text ESLState
    doSimpleEq agentName agentAttr agentStatus =
      case curr Map.!? agentName of
        Nothing -> 
          let newAttrs = Map.singleton agentAttr agentStatus
              newState = ESLState newAttrs
              newAgentMap = Map.insert agentName newState curr
          in  succ newAgentMap fail
        Just (ESLState currentAttrs) ->
          case currentAttrs Map.!? agentAttr of
            Just s | s == agentStatus -> succ curr fail
            Nothing ->
              let newAttrs = Map.insert agentAttr agentStatus currentAttrs
                  newState = ESLState newAttrs
                  newAgentMap = Map.insert agentName newState curr
              in  succ newAgentMap fail
            _ -> fail


factorial :: Int -> Int
factorial x = product [1..x]

choose :: Int -> Int -> Int
choose n k = factorial n `div` (factorial k * factorial (n - k))

mchoose :: Int -> Int -> Int
mchoose n k = choose (n + k - 1) k