{-# Language OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.ASKEE.ModelStratify.GeoGraph where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.List(mapAccumL)
import qualified Data.Aeson as JS
import Data.Aeson((.=))
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Data.Map (Map)


-- | Parse a graph, quick and dirty, we can improve on this later
parseConnGraph :: String -> Either String [(String,String,Double)]
parseConnGraph = mapM parseEdge . zip [ 1 .. ] . lines
  where
  parseEdge (n,ln) =
    let opts =
          do (a,ws) <- lex ln
             ("->",xs) <- lex ws
             (b,ys) <- lex xs
             (",",zs) <- lex ys
             (c,_) <- reads zs
             pure (mapMaybe clean a,mapMaybe clean b,c)
    in case opts of
         [] -> Left ("Invalid edge on line: " ++ show (n::Int))
         x : _ -> pure x
         
  clean :: Char -> Maybe Char
  clean c
    | isSpace c = Just '_'
    | c == '"' = Nothing
    | otherwise = Just c


-- | Returns: (number of nodes, edges, mapping from node to name)
intGraph :: [(String,String,Double)] -> (Int, [(Int,Int,Double)], Int -> String)
intGraph txtEdges = ( Map.size txtToNode
                    , edges
                    , \x -> Map.findWithDefault "?" x nodeToTxt
                    )
  where
  (finS,edges) = mapAccumL edge start txtEdges
  txtToNode    = snd finS
  nodeToTxt    = Map.fromList [ (n,txt) | (txt,n) <- Map.toList (snd finS) ]

  start = (1, Map.empty)

  resolveNode s@(nextNode,nodes) n =
    case Map.lookup n nodes of
      Just a  -> (s, a)
      Nothing ->
        let s1 = (nextNode + 1, Map.insert n nextNode nodes)
        in (s1,nextNode)

  edge s (from,to,rate) =
    let (s1,x) = resolveNode s from
        (s2,y) = resolveNode s1 to
    in (s2, (x,y,rate))


gtriJSON :: Int -> [(Int,Int)] -> JS.Value
gtriJSON n es =
  JS.object
    [ "V" .= replicate n (JS.object [])
    , "E" .= [ edge e | e <- es ]
    ]
  where
  edge (a,b) = JS.object [ "src" .= a, "tgt" .= b ]

data ConnGraph = 
  ConnGraph
    { connNodes :: [ConnNode]
    , connEdges :: [ConnEdge]
    }

data ConnNode = ConnNode

data ConnEdge =
  ConnEdge
    { edgeSource :: ConnNodeRef
    , edgeTarget :: ConnNodeRef
    , edgeRate :: Double
    }

type ConnNodeRef = Int

instance JS.ToJSON ConnGraph where
  toJSON ConnGraph{..} = 
    JS.object
      [ "V" .= connNodes
      , "E" .= connEdges ]

instance JS.ToJSON ConnNode where
  toJSON _ = JS.object []

instance JS.ToJSON ConnEdge where
  toJSON ConnEdge{..} =
    JS.object
      [ "src" .= edgeSource
      , "tgt" .= edgeTarget ]
  

asConnGraph :: String -> Either String (ConnGraph, Int -> String)
asConnGraph graphString =
  do  graph <- parseConnGraph graphString
      let (howManyNodes, edges, nodeName) = intGraph graph
          connNodes = replicate howManyNodes ConnNode
          connEdges = map (uncurry3 ConnEdge) edges
      pure (ConnGraph{..}, nodeName)
  where
    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f = \(x, y, z) -> f x y z

asMap :: ConnGraph -> (Int -> String) -> Map Int String
asMap ConnGraph{..} namer =
  Map.fromList [(i, namer i) | i <- [1..length connNodes]]


example :: String
example = unlines
  [ "City1 -> City2, 1.0"
  , "City2 -> City3, 2.0"
  , "City3 -> City1, 0.6"
  , "City1 -> \"Long Island\", 1e-4"
  ]

-- _test :: IO ()
-- _test =
--   case parseConnGraph example of
--     Left err -> putStrLn err
--     Right g  ->
--       do let (n,es,toTxt) = intGraph g
--          print es
--         --  putStrLn (BS8.unpack (JS.encode (gtriJSON n es)))
--         --  mapM_ (putStrLn . toTxt) [ 1 .. n ]

test :: IO ()
test =
  case asConnGraph example of
    Left err -> putStrLn err
    Right (g@ConnGraph{..}, nodeName) ->
      do  putStrLn (BS8.unpack $ JS.encode g)
          mapM_ (\i -> putStrLn (show i<>": "<>nodeName i))  [1 .. (length connNodes)]