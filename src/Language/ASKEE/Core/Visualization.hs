{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Core.Visualization where

import qualified Language.ASKEE.Core as Core
import Data.Text(Text)
import Data.List(nub)
import qualified Data.Map as Map
import qualified Data.Aeson as JSON
import Data.Aeson((.=))

data NodeType = Event | State
  deriving(Show, Eq, Ord)
data Node =
  Node  { nodeName :: Text
        , nodeType :: NodeType
        }
  deriving(Show, Eq, Ord)

newtype Graph =
  Graph { edges :: [(Node, Node)] }
  deriving(Show, Eq, Ord)

nodes :: Graph -> [Node]
nodes g = nub ((fst <$> edges g) ++ (snd <$> edges g))

asSchematicGraph :: Core.Model -> Maybe Graph
asSchematicGraph g =  Graph <$> sequence effs
  where
    effs =
      [ mbEdge | evt <- Core.modelEvents g
               , (var, expr) <- Map.toList $ Core.eventEffect evt
               , let mbEdge = effectEdge evt var expr ]

    eventNode evt = Node (Core.eventName evt) Event
    stateNode name = Node name State
    effectEdge evt var e0 =
      case e0 of
        Core.Var v Core.:+: Core.NumLit n | n > 0, v == var ->
          Just (eventNode evt, stateNode var)
        Core.Var v Core.:-: Core.NumLit n | n > 0, v == var ->
          Just (stateNode var, eventNode evt)
        _ ->
          Nothing

-------------------------------------------------------------------------------

instance JSON.ToJSON Graph where
  toJSON g =
    JSON.object [ "nodes" .= nodes g
                , "edges" .= edges g
                ]

instance JSON.ToJSON Node where
  toJSON node =
    JSON.object [ "name" .= nodeName node
                , "type" .= nt
                ]
    where
      nt =
        case nodeType node of
          State -> JSON.String "state"
          Event -> JSON.String "event"



