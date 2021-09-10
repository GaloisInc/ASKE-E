{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ABM.Manipulate where

import           Data.List  ( nub )
import qualified Data.Map   as Map
import qualified Data.Text  as Text

import Language.ASKEE.ABM.Syntax

combine :: Model -> Model -> Model
combine m1 m2 = Model name agent lets initial (modelEvents m1 <> modelEvents m2)
  where
    name = modelName m1 <> "_" <> modelName m2
    agent
      | compatibleAgents (modelAgent m1) (modelAgent m2) =
        Map.union (modelAgent m1) (modelAgent m2)
      | otherwise = undefined

    lets
      | null (Map.intersection (modelLets m1) (modelLets m2)) =
        Map.union (modelLets m1) (modelLets m2)
      | otherwise = undefined

    initial
      | null (Map.intersection (modelInit m1) (modelInit m2)) =
        Map.union (modelInit m1) (modelInit m2)
      | otherwise = undefined

-- | Two agents are compatible if they don't share names of attributes and if
-- those attributes' statuses don't have name clashes
compatibleAgents :: Agent -> Agent -> Bool
compatibleAgents a1 a2 = noSharedAttributeNames && noSharedStatuses
  where
    noSharedAttributeNames = null (Map.intersection a1 a2)
    noSharedStatuses = length (nub allStatuses) == length allStatuses
    allStatuses = concat [ ss | AgentAttribute _ ss <- Map.elems a1 <> Map.elems a2 ]

synthAgent :: Model -> Agent
synthAgent Model{..} = 
  Map.fromList 
    [ (attribute, AgentAttribute attrName stats)
    | (attrName, (attribute, stats)) <- zip attrNames (Map.toList statuses)
    ]
  where
    statuses = 
      Map.map nub $
        Map.fromListWith (++) $
          concat
            [ statusesFromAgentExpr eventWhen <> 
              concatMap (\(AgentAssign r1 r2) -> statusesFromAttrRefs r1 r2) eventEffect
            | Event{..} <- modelEvents
            ]

    statusesFromAgentExpr e =
      case e of
        And e1 e2 -> statusesFromAgentExpr e1 <> statusesFromAgentExpr e2
        Or e1 e2 -> statusesFromAgentExpr e1 <> statusesFromAgentExpr e2
        Eq r1 r2 -> statusesFromAttrRefs r1 r2
    
    statusesFromAttrRefs r1 r2 =
      case (r1, r2) of
        (Status _, Status _) -> undefined
        (Status stat, Attribute _ attr) -> [(attr, [stat])]
        (Attribute _ attr, Status stat) -> [(attr, [stat])]
        (Attribute _ a1, Attribute _ a2) 
          | a1 == a2 -> [(a1, [])]
          | otherwise -> undefined

    attrNames = [ Text.pack ("S"<>show i) | i <- [1::Int ..] ]