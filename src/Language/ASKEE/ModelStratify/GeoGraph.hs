{-# Language OverloadedStrings #-}
module Language.ASKEE.ModelStratify.GeoGraph
  ( parseGeoGraph
  , intGraph
  , gtriJSON
  , _test
  ) where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.List(mapAccumL)
import qualified Data.Aeson as JS
import Data.Aeson((.=))
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)


-- | Parse a graph, quick and diryt, we can improve on this later
parseGeoGraph :: String -> Either String [(String,String)]
parseGeoGraph = mapM parseEdge . zip [ 1 .. ] . lines
  where
  parseEdge (n,ln) =
    let opts =
          do (a,xs) <- lex ln
             ("->",ys) <- lex xs
             (b,_) <- lex ys
             pure (mapMaybe clean a,mapMaybe clean b)
    in case opts of
         [] -> Left ("Invalid edge on line: " ++ show (n::Int))
         x : _ -> pure x
         
  clean :: Char -> Maybe Char
  clean c
    | isSpace c = Just '_'
    | c == '"' = Nothing
    | otherwise = Just c


-- | Returns: (number of nodes, edges, mapping from node to name)
intGraph :: [(String,String)] -> (Int, [(Int,Int)], Int -> String)
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

  edge s (from,to) =
    let (s1,x) = resolveNode s from
        (s2,y) = resolveNode s1 to
    in (s2, (x,y))


gtriJSON :: Int -> [(Int,Int)] -> JS.Value
gtriJSON n es =
  JS.object
    [ "V" .= replicate n (JS.object [])
    , "E" .= [ edge e | e <- es ]
    ]
  where
  edge (a,b) = JS.object [ "src" .= a, "tgt" .= b ]


example :: String
example = unlines
  [ "City1 -> City2"
  , "City2 -> City3"
  , "City3 -> City1"
  , "City1 -> \"Long Island\""
  ]

_test :: IO ()
_test =
  case parseGeoGraph example of
    Left err -> putStrLn err
    Right g  ->
      do let (n,es,toTxt) = intGraph g
         print es
         putStrLn (BS8.unpack (JS.encode (gtriJSON n es)))
         mapM_ (putStrLn . toTxt) [ 1 .. n ]

