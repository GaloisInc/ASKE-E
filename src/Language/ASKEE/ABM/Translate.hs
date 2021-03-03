{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

-- Intermediate representation of a particular combination of ABM statuses
-- (values) across the space of attributes (keys)
newtype ESLState = ESLState 
  { statuses :: Map String String 
  }
  deriving (Eq, Ord, Show)

abmToModel :: ABM.Model -> ESL.Model
abmToModel ABM.Model{..} = ESL.Model name (lets++states) []
  where
    name = pack modelName
    lets = 
      [ ESL.Let (pack v) e
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
    flatten :: ABM.AgentAttribute -> (String, [String])
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
    mkName :: Map String String -> Text
    mkName = pack . intercalate "_" . Map.elems




factorial :: Int -> Int
factorial x = product [1..x]

choose :: Int -> Int -> Int
choose n k = factorial n `div` (factorial k * factorial (n - k))

mchoose :: Int -> Int -> Int
mchoose n k = choose (n + k - 1) k