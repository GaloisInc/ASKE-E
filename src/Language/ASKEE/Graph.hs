module Language.ASKEE.Graph where

import Control.Monad.State

-- import           Data.Array ( Array )
import qualified Data.Array as Array
import           Data.Graph (graphFromEdges,  vertices, Edge, Graph, Vertex )
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map

data Distance = N Int | Infinity
  deriving (Show, Eq)

instance Ord Distance where
  compare (N i)    (N j)    = compare i j
  compare (N _)    Infinity = LT
  compare Infinity (N _)    = GT
  compare Infinity Infinity = EQ

instance Num Distance where
  (N i) + (N j) = N (i + j)
  (N _) + Infinity = Infinity
  Infinity + (N _) = Infinity
  Infinity + Infinity = Infinity

  fromInteger = N . fromIntegral

  _ * _ = undefined
  abs _ = undefined
  signum _ = undefined
  negate _ = undefined

data Marked a = Marked
  { distance :: Distance
    -- ^ from the start node, dijkstra-style
  , value    :: a
  }
  deriving (Show)

instance Eq a => Eq (Marked a) where
  Marked _ val == Marked _ val' = val == val'

instance (Eq a, Ord a) => Ord (Marked a) where
  Marked dist val `compare` Marked dist' val'
    | val == val' = EQ
    | dist == dist' = val `compare` val'
    | otherwise = dist `compare` dist'


data Info = Info
  { unvisited :: Set (Marked Vertex)
  , graph     :: Graph
  , prevs     :: Map (Marked Vertex) (Marked Vertex)
  , markings  :: Map Vertex (Marked Vertex)
  , next      :: Marked Vertex
  }
  deriving Show

type M = State Info

shortestPath :: Graph -> Vertex -> Vertex -> Maybe [Edge]
shortestPath g start end = deepRev <$> buildPath (stripMarkings paths) (value start') (value end')
  where
    paths = evalState (driver start' end') info
    info = Info uv g Map.empty ms undefined
    uv = Set.fromList (Map.elems ms)
    ms = Map.fromSet mark unmarked
    unmarked = Set.fromList (vertices g)
    start' = mark start
    end' = mark end
    stripMarkings = Map.mapKeys value . Map.map value

    mark :: Vertex -> Marked Vertex
    mark v
      | v == start = Marked (N 0) v
      | otherwise = Marked Infinity v
    
    deepRev :: [(a, b)] -> [(b, a)]
    deepRev = map (\(x, y) -> (y, x)) . reverse

driver :: Marked Vertex -> Marked Vertex -> M (Map (Marked Vertex) (Marked Vertex))
driver current goal =
  do  uv <- gets unvisited
      if not (Set.member goal uv)
        then gets prevs
        else do visitNeighbors current
                nxt <- gets next
                driver nxt goal

visitNeighbors :: Marked Vertex -> M ()
visitNeighbors current =
  do  neighbors <- markedNeighbors current
      unvisitedNeighbors <- gets (Set.intersection (Set.fromList neighbors) . unvisited)
      mapM_ visit unvisitedNeighbors
      modify (\i -> i { unvisited = Set.delete current (unvisited i)})
      modify (\i -> i { next = Set.findMin (unvisited i) })

  where
    visit :: Marked Vertex -> M ()
    visit neighbor
      | distance current + 1 < distance neighbor =
        do  let neighbor' = neighbor { distance = distance current + 1 }
            modify (\i -> i { markings = Map.insert (value neighbor) neighbor' (markings i) })
            modify (\i -> i { prevs = Map.insert neighbor' current (prevs i) })
            modify (\i -> i { unvisited = Set.map ((markings i Map.!) . value) (unvisited i)})
      | otherwise = pure ()

markedNeighbors :: Marked Vertex -> M [Marked Vertex]
markedNeighbors v = 
  do  g <- gets graph
      ms <- gets markings
      pure $ map (ms Map.!) (neighbors g (value v))

  where
    neighbors :: Graph -> Vertex -> [Vertex]
    neighbors = (Array.!)

buildPath :: Map Vertex Vertex -> Vertex -> Vertex -> Maybe [Edge]
buildPath ps start end 
  | start == end = pure []
  | otherwise = 
    do  predecessor <- ps Map.!? end
        (fmap . (:)) (end, predecessor) (buildPath ps start predecessor)

testGraph :: Graph
nodeFromVertex :: Vertex -> (ModelType, ModelType, [ModelType])
vertexFromKey :: ModelType -> Maybe Vertex
(testGraph, nodeFromVertex, vertexFromKey) = graphFromEdges nodes 
  where
    nodes = [ mkNode  ESL_C   [ESL_A]
            , mkNode  ESL_A   [DEQ_A, ESL_C, TOPO_A]
            , mkNode  DEQ_C   [DEQ_A]
            , mkNode  DEQ_A   [DEQ_C, LATEX_C]
            , mkNode  LATEX_C [DEQ_A]
            , mkNode  RNET_C  [RNET_A]
            , mkNode  RNET_A  []
            , mkNode  TOPO_C  [TOPO_A]
            , mkNode  TOPO_A  [TOPO_C]
            ]
    mkNode x ys = (x,x,ys)

data ModelType =
    ESL_C
  | ESL_A
  | DEQ_C
  | DEQ_A
  | RNET_C
  | RNET_A
  | TOPO_C
  | TOPO_A
  | LATEX_C
  deriving (Enum, Eq, Ord)