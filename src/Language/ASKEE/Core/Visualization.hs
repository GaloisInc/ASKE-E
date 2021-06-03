{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Core.Visualization where

-- import qualified Language.ASKEE.Core as Core
import           Data.Aeson( (.=) )
import qualified Data.Aeson as JSON
import           Data.List  ( nub )
import qualified Data.Map   as Map
import           Data.Text  ( Text )

import Language.ASKEE.Core.Syntax hiding ( Event )

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

asSchematicGraph :: Model -> Maybe Graph
asSchematicGraph g =  Graph . nub <$> sequence effs
  where
    effs =
      [ mbEdge | evt <- modelEvents g
               , (var, expr) <- Map.toList $ eventEffect evt
               , let mbEdge = effectEdge evt var expr ]

    eventNode evt = Node (eventName evt) Event
    stateNode name = Node name State
    effectEdge evt var e0 =
      case e0 of
        Var v :+: NumLit n | n > 0, v == var ->
          Just (eventNode evt, stateNode var)
        Var v :-: NumLit n | n > 0, v == var ->
          Just (stateNode var, eventNode evt)
        _ ->
          Nothing