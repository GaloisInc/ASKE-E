{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Language.ASKEE.Convert where

import Control.Monad ( (>=>) )

import           Data.Map   ( Map )
import qualified Data.Map   as Map
import           Data.Graph ( graphFromEdges, Edge, Graph, Vertex )

import           Language.ASKEE.Core.DiffEq      ( asEquationSystem )
import           Language.ASKEE.Core.ImportASKEE ( modelAsCore )
import           Language.ASKEE.Graph            ( shortestPath )
import qualified Language.ASKEE.DEQ.GenLexer     as DL
import qualified Language.ASKEE.DEQ.GenParser    as DP
import qualified Language.ASKEE.DEQ.Print        as DPr
import qualified Language.ASKEE.DEQ.Syntax       as DEQ
import qualified Language.ASKEE.GenLexer         as AL
import qualified Language.ASKEE.GenParser        as AP
import qualified Language.ASKEE.Latex.GenLexer   as LL
import qualified Language.ASKEE.Latex.GenParser  as LP
import           Language.ASKEE.Panic            ( panic )
import qualified Language.ASKEE.Print            as APr
import qualified Language.ASKEE.RNet.GenLexer    as RL 
import qualified Language.ASKEE.RNet.GenParser   as RP
import qualified Language.ASKEE.RNet.Syntax      as RNet
import qualified Language.ASKEE.Syntax           as ESL

import Language.Haskell.TH
import Data.Maybe ( mapMaybe )

data ModelType =
    ESL_C
  | ESL_A
  | DEQ_C
  | DEQ_A
  | RNET_C
  | RNET_A
  | LATEX_C
  deriving (Enum, Eq, Ord)

instance Show ModelType where
  show ESL_C = "ESL concrete syntax"
  show ESL_A = "ESL abstract syntax"
  show DEQ_C = "differential equation concrete syntax"
  show DEQ_A = "differential equation abstract syntax"
  show RNET_C = "reaction network concrete syntax"
  show RNET_A = "reaction network abstract syntax"
  show LATEX_C = "latex concrete syntax"

data Repr = Abstract | Concrete
  deriving (Eq, Show)

class Tagged m where
  tagOf :: Repr -> ModelType

instance Tagged ESL.Model where
  tagOf Abstract = ESL_A
  tagOf Concrete = ESL_C

instance Tagged DEQ.DiffEqs where
  tagOf Abstract = DEQ_A
  tagOf Concrete = DEQ_C

instance Tagged RNet.ReactionNet where
  tagOf Abstract = RNET_A
  tagOf Concrete = RNET_C

data Latex

instance Tagged Latex where
  tagOf Concrete = LATEX_C
  tagOf Abstract = error "no latex abstract syntax exists"

converter :: ModelType -> ModelType -> Q Exp 
converter from to = 
  case pairToVertices (from, to) >>= uncurry sp of
    Just p -> functionFromPath $ deepRev p
    Nothing -> fail $ "No path from "++show from++" to "++show to

  where
    functionFromPath :: [Edge] -> Q Exp
    functionFromPath es = 
      foldr (\(f, t) nxt -> [e| $(translator f t) >=> $nxt |]) [e| Right |] es

    translator :: Vertex -> Vertex -> Q Exp
    translator f t = 
      case mapFromVertex f Map.!? modelTypeFromVertex t of
        Just tr -> tr
        Nothing -> panic "Couldn't find translation function" ["from "++show (modelTypeFromVertex f)++" to "++show (modelTypeFromVertex t)]


    modelTypeFromVertex :: Vertex -> ModelType
    modelTypeFromVertex = snd3 . nodeFromVertex

    mapFromVertex :: Vertex -> Map ModelType (Q Exp)
    mapFromVertex = fst3 . nodeFromVertex

    fst3 (x,_,_) = x
    snd3 (_,y,_) = y

    sp :: Vertex -> Vertex -> Maybe [Edge]
    sp = shortestPath graph

    pairToVertices :: (ModelType, ModelType) -> Maybe (Vertex, Vertex)
    pairToVertices (f, t) =
      case (vertexFromKey f, vertexFromKey t) of
        (Nothing, Nothing) -> Nothing
        (Just _, Nothing) -> Nothing
        (Nothing, Just _) -> Nothing
        (Just fromV, Just toV) -> Just (fromV, toV)

    deepRev :: [(a, b)] -> [(b, a)]
    deepRev = map (\(x, y) -> (y, x)) . reverse


graph :: Graph
nodeFromVertex :: Vertex -> (Map ModelType (Q Exp), ModelType, [ModelType])
vertexFromKey :: ModelType -> Maybe Vertex
(graph, nodeFromVertex, vertexFromKey) = graphFromEdges nodes
  where
    nodes = [ mkNode eslCTranslators   ESL_C   [ESL_A]
            , mkNode eslATranslators   ESL_A   [DEQ_A, ESL_C]
            , mkNode deqCTranslators   DEQ_C   [DEQ_A]
            , mkNode deqATranslators   DEQ_A   [DEQ_C, LATEX_C]
            , mkNode latexCTranslators LATEX_C [DEQ_A]
            , mkNode rnetCTranslators  RNET_C  [RNET_A]
            , mkNode rnetATranslators  RNET_A  []
            ]

    eslCTranslators =   Map.fromList [ (ESL_A,  [e| AL.lexModel >=> AP.parseModel |]) ]
    eslATranslators =   Map.fromList [ (DEQ_A,  [e| fmap asEquationSystem . modelAsCore [] |])
                                     , (ESL_C,  [e| Right . show . APr.printModel |]) ]
    deqCTranslators =   Map.fromList [ (DEQ_A,  [e| DL.lexDEQs >=> DP.parseDEQs |]) ]
    deqATranslators =   Map.fromList [ (DEQ_C,  [e| Right . show . DPr.ppDiffEqs |])
                                     , (LATEX_C, [e| Right . show . LPr.printLatex |]) ]
    latexCTranslators = Map.fromList [ (DEQ_A,  [e| LL.lexLatex >=> LP.parseLatex |]) ]
    rnetCTranslators =  Map.fromList [ (RNET_A, [e| RL.lexRNet >=> RP.parseRNet |] ) ]
    rnetATranslators =  Map.empty

    mkNode :: Map ModelType (Q Exp) -> ModelType -> [ModelType] -> (Map ModelType (Q Exp), ModelType, [ModelType])
    mkNode translators model links
      | and [Map.member link translators | link <- links] = (translators, model, links)
      | otherwise = panic "Error in format graph construction" [ "When creating node:", show (Map.keys translators), show model, show links]


-- | For testing - synthesize all possible model-to-model journeys and, for all
-- that produce a valid path, declare them with a fresh name. The only occurrence
-- of `fail` that this `recover` would catch is triggered when no path can be
-- found between the model types.
allConverters :: Q [Dec]
allConverters = concat <$> mapM (\pair -> pure [] `recover` mkConv pair) pairs
  where
    mkConv :: (Vertex, Vertex) -> Q [Dec]
    mkConv pair =
      do  conv <- uncurry converter (verticesToNodes pair)
          name <- gensym "f"
          pure [FunD name [Clause [] (NormalB conv) []]]

    gensym pfx = mkName . show <$> newName pfx
    snd3 (_,y,_) = y
    pairs = mapMaybe pairToVertices $ allPairs allModels

    allPairs :: [a] -> [(a, a)]
    allPairs xs = concatMap (\x -> map (x,) xs) xs

    allModels :: [ModelType]
    allModels = [ESL_C .. LATEX_C]

    verticesToNodes :: (Vertex, Vertex) -> (ModelType, ModelType)
    verticesToNodes (v1, v2) = (snd3 $ nodeFromVertex v1, snd3 $ nodeFromVertex v2)

    pairToVertices :: (ModelType, ModelType) -> Maybe (Vertex, Vertex)
    pairToVertices (from, to) =
      case (vertexFromKey from, vertexFromKey to) of
        (Nothing, Nothing) -> Nothing
        (Just _, Nothing) -> Nothing
        (Nothing, Just _) -> Nothing
        (Just fromV, Just toV) -> Just (fromV, toV)