--
-- visual summary graph for uncharted
--

{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.VisualSummaryGraph where

import qualified Data.Aeson as Aeson
import           Data.Aeson((.=))
import qualified Data.Map as Map
import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS

import qualified Language.ASKEE.DiffEq.DiffEq as DiffEq
import qualified Language.ASKEE.Expr as Expr
import qualified Language.ASKEE.AMIDOLIR as AMI

type Identifier = String

data NodeType = NTVariable | NTConstant
  deriving(Show, Eq, Ord)

-- TODO: add fields
data SummaryMetadata = SummaryMetadata
  deriving(Show, Eq, Ord)

-- TODO: add fields
data NodeMetadata = NodeMetadata
  deriving(Show, Eq, Ord)

data SummaryGraph =
  SummaryGraph { summaryGraphMetadata :: SummaryMetadata
               , summaryGraphNodes    :: [Node]
               , summaryGraphEdges    :: [Edge]
               , summaryGraphGroups   :: [Group]
               }
  deriving(Show, Eq, Ord)

data Node =
  Node { nodeId :: Identifier
       , nodeType :: NodeType
       , nodeConcept :: String
       , nodeLabel :: String
       , nodeMetadata :: NodeMetadata
       }
  deriving(Show, Eq, Ord)

data Edge =
  Edge { edgeId :: Identifier
       , edgeSource :: Identifier
       , edgeTarget :: Identifier
       }
  deriving(Show, Eq, Ord)

data Group =
  Group { groupId :: Identifier
        , groupMembers :: [Identifier]
        }
  deriving(Show, Eq, Ord)

instance Aeson.ToJSON SummaryMetadata where
  toJSON meta = Aeson.object [ ]

instance Aeson.ToJSON NodeMetadata where
  toJSON meta = Aeson.object [ ]

instance Aeson.ToJSON SummaryGraph where
  toJSON graph =
    Aeson.object [ "metadata" .= summaryGraphMetadata graph
                 , "nodes" .= summaryGraphNodes graph
                 , "edges" .= summaryGraphEdges graph
                 , "groups" .= summaryGraphGroups graph
                 ]

instance Aeson.ToJSON Node where
  toJSON node =
    Aeson.object [ "id" .= nodeId node
                 , "concept" .= nodeConcept node
                 , "label" .= nodeLabel node
                 , "type" .=
                      case nodeType node of
                        NTVariable -> "variable" :: String
                        NTConstant -> "constant"
                 , "metadata" .= nodeMetadata node
                 ]

instance Aeson.ToJSON Edge where
  toJSON edge =
    Aeson.object [ "id" .= edgeId edge
                 , "source" .= edgeSource edge
                 , "target" .= edgeTarget edge
                 ]

instance Aeson.ToJSON Group where
  toJSON group = 
    Aeson.object [ "id" .= groupId group
                 , "members" .= groupMembers group
                 ]


emptyGraph :: SummaryGraph
emptyGraph = SummaryGraph SummaryMetadata [] [] []




nodeLabelMap :: SummaryGraph -> Map.Map Identifier Node
nodeLabelMap graph =  
  Map.fromList $ (\n -> (nodeLabel n, n)) <$> summaryGraphNodes graph

addGroup :: String -> [String] -> SummaryGraph -> SummaryGraph
addGroup groupName nodeNames graph =
    graph { summaryGraphGroups = newGroup:summaryGraphGroups graph}
  where
    getNode name = (nodeLabelMap graph) Map.! name
    newGroup =
      Group { groupId = groupName
            , groupMembers = nodeId . getNode <$> nodeNames
            }

-- TODO: we can derive the var-ness from the edges
--       vars are things with self-edges or that depend on
--       things with self-edges
fromEdges :: [String] -> [(String, String)] -> SummaryGraph
fromEdges vars edges = go edges Map.empty 1 emptyGraph
  where
    varSet = Set.fromList vars
    getNode name nameMap =  
      case Map.lookup name  nameMap of
        Just ident -> (ident, nameMap, []) 
        Nothing ->
          let ident = show $ 1 + Map.size nameMap
              node = 
                Node { nodeId = ident
                     , nodeLabel = name
                     , nodeConcept = name
                     , nodeType = 
                        if   Set.member name varSet 
                        then NTVariable 
                        else NTConstant
                     , nodeMetadata = NodeMetadata
                     }

          in  (ident, Map.insert name ident nameMap, [node])
  
    go :: [(String, String)] -> Map.Map String String -> Int -> SummaryGraph -> SummaryGraph
    go es nameMap edgeNum graph =
      case es of
        [] -> graph
        (nd1, nd2):rs ->
          let (srcId, nameMap', added1) = getNode nd1 nameMap
              (tgtId, nameMap'', added2) = getNode nd2 nameMap'
              nodes' = (summaryGraphNodes graph) ++ added1 ++ added2
              edge = 
                Edge { edgeId = show edgeNum
                     , edgeSource = srcId
                     , edgeTarget = tgtId  
                     }
              edges' =  (summaryGraphEdges graph) ++ [edge]
              graph' = graph { summaryGraphNodes = nodes'
                             , summaryGraphEdges = edges'
                             }
              edgeNum' = edgeNum + 1

          in go rs nameMap'' edgeNum' graph'


withGroups :: SummaryGraph -> [(String, [String])] -> SummaryGraph
withGroups graph groups = 
  foldl (\sg (g, elts) -> addGroup g elts sg) graph groups

-- everything below this is deletable - mostly added for testing 

edgesForArithExp :: Text -> Expr.Expr -> [(Identifier, Identifier)]
edgesForArithExp tgt eqn =
  case eqn of
    Expr.Var name -> [(Text.unpack name, Text.unpack tgt)]
    Expr.LitD _ -> []
    Expr.Add e1 e2 -> bin e1 e2
    Expr.Sub e1 e2 -> bin e1 e2
    Expr.Mul e1 e2 -> bin e1 e2
    Expr.Div e1 e2 -> bin e1 e2
    Expr.Neg e -> edgesForArithExp tgt e
  where
    bin e1 e2 = edgesForArithExp tgt e1 ++ edgesForArithExp tgt e2

edgesForDiffEq :: DiffEq.DiffEq -> [(Identifier, Identifier)]
edgesForDiffEq eq =
  case eq of
    DiffEq.StateEq n eqn -> edgesForArithExp n eqn
    DiffEq.VarEq n eqn -> edgesForArithExp n eqn

irExpAsDiffEq :: AMI.ModelExp -> Expr.Expr
irExpAsDiffEq e = 
  case e of
    AMI.Add e1 e2 -> bin e1 e2 Expr.Add
    AMI.Mul e1 e2 -> bin e1 e2 Expr.Mul
    AMI.Sub e1 e2 -> bin e1 e2 Expr.Sub
    AMI.Div e1 e2 -> bin e1 e2 Expr.Div
    AMI.Neg e -> Expr.Neg $ irExpAsDiffEq e
    AMI.LitNum n -> Expr.LitD n
    AMI.Var t -> Expr.Var t
  where
    bin e1 e2 c = irExpAsDiffEq e1 `c` irExpAsDiffEq e2


writeToFile :: FilePath -> SummaryGraph -> IO ()
writeToFile fp g = BS.writeFile fp (Aeson.encode g)

cov1_beijing :: SummaryGraph
cov1_beijing = fromEdges vars edges
  where
    vars = ["Rp", "Ip", "R", "E", "S", "I"]
    edges = eqnSystem >>= mkEdges
    mkEdges (vc, eq) =
      case AMI.tryParseExp eq of
        Just peq -> edgesForDiffEq (vc $ irExpAsDiffEq peq)
        Nothing -> error "noooo"

    eqnSystem =
      [ (DiffEq.StateEq "Rp", "sigma * Ip")
      , (DiffEq.StateEq "Ip", "rp * (S * Ip) / N - sigma * Ip")
      , (DiffEq.StateEq "R", "alpha * I")
      , (DiffEq.StateEq "E", "r * (S * I) / N - beta * E")
      , (DiffEq.StateEq "S", "-r * (S * I) / N - rp * (S * Ip) / N")
      , (DiffEq.StateEq "I", "beta * E - alpha * I")
      ]

    

sir :: SummaryGraph
sir = base `withGroups` groups
  where
    base = 
      fromEdges [s, i, r]
          [ (n, s)
          , (n, i)
          , (s, s)
          , (s, i)
          , (beta, s)
          , (beta, i)
          , (i, i)
          , (i, r)
          , (gamma, i)
          , (gamma, r)
          ]

    groups =
      [ ("Population", [s, r])
      , ("Patient" , [i])
      ]

    s = "Susceptible"
    i = "Infected"
    r = "Recovered"
    beta = "beta"
    gamma = "gamma"
    n = "Total population"

-- QS: why does I have an edge to S
chime :: SummaryGraph
chime = (fromEdges vars edges) `withGroups` groups
  where
    groups =
      [("Population", [s, r])
      ,("Recovered" , [r])
      ]
    vars = [s, i, r, beta, growthRate, doublingTime]
    edges =
      [(s, i), (s,s)
      ,(i, i), (i,r), (i,s)
      ,(beta, s), (beta, i)
      ,(gamma, beta), (gamma, i), (gamma, r)
      ,(contactRate, beta)
      ,(growthRate, growthRate), (growthRate, beta)
      ,(doublingTime, doublingTime), (doublingTime, growthRate)
      ]

    base =
      fromEdges vars 

    s = "Susceptible"
    i = "Infected"
    r = "Recovered"
    beta = "beta"
    gamma = "gamma"
    growthRate = "Growth Rate"
    doublingTime = "Doubling Time"
    contactRate = "Contact Rate"






-- -- QS: n goes to Ip but not I, 
-- --     I goes to E but I_p doesn't
-- --     no exposure for disease B?
-- sierp :: SummaryGraph
-- sierp = (fromEdges vars edges) `withGroups` groups
--   where
--     vars = [ _S, _I, _R_p, _I_p, _R, _E]
--     edges = 
--       [ (n, _S), (n, _E), (n, _I_p), (n, _E) 

--       , (_I, _S), (_I, _E), (_I, _I), (_I, _R)
--       , (_S, _S), (_S, _I_p), (_S, _E)
--       , (r, _S), (r, _E)
--       , (_E, _E), (_E, _I)

--       , (_I_p, _S), (_I_p, _R_p), (_I_p, _R_p)
--       , (r_p, _S), (r_p, _E)
      
--       , (alpha, _R), (alpha, _I)
--       , (beta, _E), (beta, _I)
--       , (sigma, _I_p), (sigma, _R_p)
--       ]

--     eqns =
--       [(_S, )]

--     _S = "Susceptible"
--     _E = "Exposed to A"
--     _I = "Infected with A"
--     _R = "Protected from A"
--     _R_p = "Protected from B"
--     _I_p = "Infected with B"
--     beta = "beta"
--     alpha = "alpha"
--     sigma = "sigma"
--     n = "Total Population"
--     r = "Rate of Spread of A"
--     r_p = "Rate of Spread of B"


