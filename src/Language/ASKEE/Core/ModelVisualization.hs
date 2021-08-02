{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Core.ModelVisualization where

import           Control.Monad.IO.Class

import           Data.Text  ( Text )
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe ( fromJust )

import           Data.Aeson( (.=) )
import qualified Data.Aeson as JSON

import           Language.ASKEE.Core.Expr
import           Language.ASKEE.Core.Syntax hiding ( Event )

import           Debug.Trace (trace, traceIO)

-------------------------------------------------------------------------

data NodeType = Event | State
  deriving(Show, Eq, Ord)

data Node =
  Node  { nodeName :: Text
        , nodeType :: NodeType
        }
  deriving(Show, Eq, Ord)

data Edge = 
  Edge { edgeSource :: Int
       , edgeTarget :: Int
       }
  deriving(Show, Eq, Ord)

data Graph = 
  Graph { nodes :: [Node]
        , edges :: [Edge]
        }
  deriving(Show, Eq, Ord)
  

stateNode :: Ident -> Node
stateNode t = Node t State  

eventNode :: Ident -> Node
eventNode t = Node t Event
  
-- addNode :: Node -> Graph -> Graph
-- addNode n g = g { nodes = nodes g ++ n }

-- addEdge :: Node -> Node -> Graph -> Graph
-- addEdge n1 n2 g = 
--   let i1 = fromJust $ List.elemIndex n1 (nodes g)
--       i2 = fromJust $ List.elemIndex n2 (nodes g)
--   in g { edges = edges g ++ [(i1, i2)] }

  
---------------------------------------------------------------------------------  

toNetworkGraph :: Model -> Graph
toNetworkGraph model =
  let allNodes = Map.elems stateNodesMap ++ Map.elems eventNodesMap
      allNodesWithIndices = Map.fromList $ zip allNodes [0..]
      edge (n1, n2) = Edge (allNodesWithIndices Map.! n1) (allNodesWithIndices Map.! n2)
      allEdgesRaw = Set.unions $ map eventEdges (modelEvents model)
      allEdges = Set.toList $ Set.map edge allEdgesRaw
  in Graph allNodes allEdges
  where
    stateNameSet = Set.unions $ map (Map.keysSet . eventEffect) (modelEvents model)
    stateNodesMap = Map.fromSet stateNode stateNameSet
    eventNodesMap = Map.fromList $ map (\ev -> (ev, eventNode $ eventName ev)) (modelEvents model)
    stateDependencies e = Set.intersection stateNameSet (collectVars e)
    eventEdges ev =
      let evNode = eventNodesMap Map.! ev
          edgeToState st = (evNode, stateNodesMap Map.! st)
          edgeToEvent st = (stateNodesMap Map.! st, evNode)
          outGoingEdges = Set.fromList $ map edgeToState $ Map.keys (eventEffect ev)
          allstateDependencies = Set.unions $ map stateDependencies $ Map.elems (eventEffect ev)
          incomingEdges = Set.map edgeToEvent allstateDependencies
      in Set.union incomingEdges outGoingEdges
      
      
-------------------------------------------------------------------------------------

convertToVegaData :: Graph -> JSON.Value
convertToVegaData g = JSON.toJSON [nodeValues, edgeValues]
    where
      nodeValues =
        JSON.object [ "name"   .= JSON.String "node-data"
                    , "values" .= map convertNode (nodes g )
                    ]
      edgeValues =
        JSON.object [ "name"   .= JSON.String "link-data"
                    , "values" .= map convertEdge (edges g)
                    ]
      convertNode node =
        JSON.object [ "name" .= nodeName node
                    , "type" .= nt
                    ]
        where
          nt = case nodeType node of
                 State -> JSON.String "state"
                 Event -> JSON.String "event"
      convertEdge edge =  
        JSON.object [ "source" .= edgeSource edge
                    , "target" .= edgeTarget edge
                    ]    
                    
convertToVega :: Graph -> JSON.Value 
convertToVega g =
  -- Load the template, convert to JSON and add our data to it  !
  let template = JSON.decodeStrict (encodeUtf8 vegaNetworkGraphTemplate) :: Maybe JSON.Value 
      JSON.Object obj = fromJust template
      graphValue = HashMap.insert "data" (convertToVegaData g) obj
  in JSON.Object graphValue
  
  
---------------------------------------------------------------------

renderModelAsNetwork :: Model -> JSON.Value 
renderModelAsNetwork model = convertToVega (toNetworkGraph model)
  
-- Quick helper to test things out
renderModelAsNetworkIO :: MonadIO m => Model -> FilePath -> m ()
renderModelAsNetworkIO model f = liftIO $ JSON.encodeFile f (renderModelAsNetwork model)
      

-- This really should go in to some config file somewhere!
-- Quoting like this is remarkably ugly ..
vegaNetworkGraphTemplate :: Text
vegaNetworkGraphTemplate = " \
 \ { \
 \   \"$schema\": \"https://vega.github.io/schema/vega/v5.json\", \
 \   \"description\": \"A network representation of a model\", \
 \   \"width\": 700, \
 \   \"height\": 500, \
 \   \"padding\": 0, \
 \   \"autosize\": \"none\", \
 \  \
 \   \"signals\": [ \
 \     { \"name\": \"cx\", \"update\": \"width / 2\" }, \
 \     { \"name\": \"cy\", \"update\": \"height / 2\" }, \
 \     { \"name\": \"nodeRadius\", \"value\": 24 }, \
 \     { \"name\": \"nodeCharge\", \"value\": -60 }, \
 \     { \"name\": \"linkDistance\", \"value\": 120 }, \
 \     { \"name\": \"static\", \"value\": true } \
 \   ], \
 \   \ 
 \   \"scales\": [ \
 \     { \
 \       \"name\": \"color\", \
 \       \"type\": \"ordinal\", \
 \       \"domain\": {\"data\": \"node-data\", \"field\": \"type\"}, \
 \       \"range\": {\"scheme\": [\"blue\", \"orange\"]} \
 \     } \
 \   ], \
 \  \
 \   \"marks\": [ \
 \     { \
 \       \"name\": \"nodes\", \
 \       \"type\": \"symbol\", \
 \       \"zindex\": 1, \
 \  \
 \       \"from\": {\"data\": \"node-data\"}, \
 \  \
 \       \"encode\": { \
 \         \"enter\": { \
 \           \"fill\": {\"scale\": \"color\", \"field\": \"type\"}, \
 \           \"stroke\": {\"value\": \"black\"}, \
 \           \"shape\": {\"signal\": \"datum.group === 1 ? 'square' : 'circle'\"}, \
 \           \"size\": {\"signal\": \"datum.type === 'event' ? nodeRadius * nodeRadius / 2 : 2 * nodeRadius * nodeRadius\"} \
 \         } \
 \       }, \
 \  \
 \       \"transform\": [ \
 \         { \
 \           \"type\": \"force\", \
 \           \"iterations\": 300, \
 \           \"static\": true, \
 \           \"signal\": \"force\", \
 \           \"forces\": [ \
 \             {\"force\": \"center\", \"x\": {\"signal\": \"cx\"}, \"y\": {\"signal\": \"cy\"}}, \
 \             {\"force\": \"collide\", \"radius\": {\"signal\": \"nodeRadius\"}}, \
 \             {\"force\": \"nbody\", \"strength\": {\"signal\": \"nodeCharge\"}}, \
 \             {\"force\": \"link\", \"links\": \"link-data\", \"distance\": {\"signal\": \"linkDistance\"}} \
 \           ] \
 \         } \
 \       ] \
 \     }, \
 \     { \
 \       \"type\": \"path\", \
 \       \"from\": {\"data\": \"link-data\"}, \
 \       \"interactive\": false, \
 \       \"encode\": { \
 \         \"enter\": { \
 \           \"stroke\": {\"value\": \"#ccc\"}, \
 \           \"strokeWidth\": {\"value\": 2.5} \
 \         } \
 \       }, \
 \       \"transform\": [ \
 \         { \
 \           \"type\": \"linkpath\", \
 \           \"require\": {\"signal\": \"force\"}, \
 \           \"shape\": \"curve\", \
 \           \"sourceX\": \"datum.source.x\", \"sourceY\": \"datum.source.y\", \
 \           \"targetX\": \"datum.target.x\", \"targetY\": \"datum.target.y\" \
 \         } \
 \       ] \
 \     }, \
 \     { \
 \       \"type\": \"text\", \
 \       \"from\": {\"data\": \"nodes\"}, \
 \       \"zindex\": 2, \
 \       \"encode\": { \
 \         \"enter\": { \
 \           \"text\": {\"field\": \"datum.name\"}, \
 \           \"x\": {\"field\": \"x\"}, \
 \           \"y\": {\"field\": \"y\"}, \
 \           \"align\": {\"value\": \"center\"}, \
 \           \"baseline\": {\"value\": \"middle\"}, \
 \           \"fontSize\": { \"value\": 20} , \
 \           \"fontWeight\": { \"value\": \"bold\"}, \
 \           \"fill\": {\"value\": \"white\"} \
 \         } \
 \       } \
 \     }  \
 \   ] \
 \ } \
 \ "