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



