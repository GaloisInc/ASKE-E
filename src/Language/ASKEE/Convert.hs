{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Language.ASKEE.Convert where
{-
import Control.Monad ( (>=>) )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char  ( isSpace )
import           Data.Map   ( Map )
import qualified Data.Map   as Map
import           Data.Maybe ( mapMaybe )
import           Data.Graph ( graphFromEdges, Edge, Graph, Vertex )

import Language.ASKEE.Core.DiffEq      ( asEquationSystem )
import Language.ASKEE.Core.ImportASKEE ( modelAsCore )
import Language.ASKEE.Graph            ( shortestPath )
import Language.ASKEE.Panic            ( panic )
import Language.ASKEE.Types            ( ModelType(..), Representation(..) )

import qualified Language.ASKEE.DEQ.GenLexer  as DEQLex
import qualified Language.ASKEE.DEQ.GenParser as DEQParse
import qualified Language.ASKEE.DEQ.Print     as DEQPrint
import qualified Language.ASKEE.DEQ.Syntax    as DEQSyntax

import qualified Language.ASKEE.ESL.GenLexer  as ESLLex
import qualified Language.ASKEE.ESL.GenParser as ESLParse
import qualified Language.ASKEE.ESL.Print     as ESLPrint
import qualified Language.ASKEE.ESL.Syntax    as ESLSyntax

import qualified Language.ASKEE.Latex.GenLexer  as LatexLex
import qualified Language.ASKEE.Latex.GenParser as LatexParse
import qualified Language.ASKEE.Latex.Print     as LatexPrint
import qualified Language.ASKEE.Latex.Syntax    as LatexSyntax

import qualified Language.ASKEE.RNet.GenLexer  as RNetLex
import qualified Language.ASKEE.RNet.GenParser as RNetParse
import qualified Language.ASKEE.RNet.Syntax    as RNetSyntax
import           Language.ASKEE.RNet.Reaction  ( reactionsAsModel )

import qualified Language.ASKEE.ModelStratify.Syntax   as TopoSyntax
import           Language.ASKEE.ModelStratify.Topology ( modelAsTopology
                                                       , topologyAsModel )

import Language.ASKEE.ModelStratify.Syntax ( Net(..) )

import Language.Haskell.TH

class Tagged m where
  tagOf :: Representation -> ModelType

instance Tagged ESLSyntax.Model where
  tagOf Abstract = ESL Abstract
  tagOf Concrete = ESL Concrete

instance Tagged DEQSyntax.DiffEqs where
  tagOf Abstract = DEQ Abstract
  tagOf Concrete = DEQ Concrete

instance Tagged RNetSyntax.ReactionNet where
  tagOf Abstract = RNET Abstract
  tagOf Concrete = RNET Concrete

instance Tagged LatexSyntax.Latex where
  tagOf Concrete = LATEX Concrete
  tagOf Abstract = LATEX Abstract

instance Tagged Net where
  tagOf Concrete = TOPO Concrete
  tagOf Abstract = TOPO Abstract

typeOf :: ModelType -> Q Type
typeOf m =
  case m of
    ESL Concrete -> [t| String |]
    ESL Abstract -> [t| ESLSyntax.Model |]
    DEQ Concrete -> [t| String |]
    DEQ Abstract -> [t| DEQSyntax.DiffEqs |]
    RNET Concrete -> [t| String |]
    RNET Abstract -> [t| RNetSyntax.ReactionNet |]
    TOPO Concrete -> [t| String |]
    TOPO Abstract -> [t| TopoSyntax.Net |]
    LATEX Concrete -> [t| String |]
    LATEX Abstract -> [t| LatexSyntax.Latex |]
    ESLMETA Concrete -> [t| String |]
    ESLMETA Abstract -> [t| ESLSyntax.ModelMeta |]
    GROMET _ -> undefined

converter' :: String -> ModelType -> ModelType -> Q [Dec]
converter' nm from to =
  do  let conv = converter from to
      funDec <- funD (mkName nm) [clause [] (normalB conv) []]
      typeDec <- sigD (mkName nm) [t| $(typeOf from) -> Either String $(typeOf to) |]
      pure [typeDec, funDec]

-- | Synthesize a converter between the two provided ModelTypes,
-- `fail`ing if no path exists between the two
converter :: ModelType -> ModelType -> Q Exp 
converter from to = 
  case pairToVertices (from, to) >>= uncurry sp of
    Just p -> functionFromPath p
    Nothing -> fail $ "No path from "++show from++" to "++show to

  where
    functionFromPath :: [Edge] -> Q Exp
    functionFromPath = 
      foldr (\(f, t) nxt -> [e| $(translator f t) >=> $nxt |]) [e| Right |]

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



graph :: Graph
nodeFromVertex :: Vertex -> (Map ModelType (Q Exp), ModelType, [ModelType])
vertexFromKey :: ModelType -> Maybe Vertex
(graph, nodeFromVertex, vertexFromKey) = graphFromEdges nodes
  where
    nodes = [ mkNode eslCTranslators   (ESL Concrete)   [ESL Abstract]
            , mkNode eslATranslators   (ESL Abstract)   [DEQ Abstract, ESL Concrete, TOPO Abstract]
            , mkNode eslMetaCTranslators (ESLMETA Concrete) [ESLMETA Abstract]
            , mkNode eslMetaATranslators (ESLMETA Abstract) []
            , mkNode deqCTranslators   (DEQ Concrete)   [DEQ Abstract]
            , mkNode deqATranslators   (DEQ Abstract)   [DEQ Concrete, LATEX Abstract]
            , mkNode latexCTranslators (LATEX Concrete) [LATEX Abstract]
            , mkNode latexATranslators (LATEX Abstract) [LATEX Concrete, DEQ Abstract]
            , mkNode rnetCTranslators  (RNET Concrete)  [RNET Abstract]
            , mkNode rnetATranslators  (RNET Abstract)  [ESL Abstract]
            , mkNode topoCTranslators  (TOPO Concrete)  [TOPO Abstract]
            , mkNode topoATranslators  (TOPO Abstract)  [TOPO Concrete, ESL Abstract]
            ]

    eslCTranslators =   Map.fromList [ (ESL Abstract,   [e| ESLLex.lexModel >=> ESLParse.parseModel |]) ]
    eslATranslators =   Map.fromList [ (DEQ Abstract,   [e| fmap asEquationSystem . modelAsCore |])
                                     , (ESL Concrete,   [e| Right . show . ESLPrint.printModel |])
                                     , (TOPO Abstract,  [e| Right . modelAsTopology |]) ]
    eslMetaCTranslators = Map.fromList [ (ESLMETA Abstract, [e| ESLLex.lexModel >=> ESLParse.parseModelMeta |])]
    eslMetaATranslators = Map.empty
    deqCTranslators =   Map.fromList [ (DEQ Abstract,   [e| DEQLex.lexDEQs >=> DEQParse.parseDEQs |]) ]
    deqATranslators =   Map.fromList [ (DEQ Concrete,   [e| Right . show . DEQPrint.ppDiffEqs |])
                                     , (LATEX Abstract, [e| Right . LatexSyntax.Latex |]) ]
    latexATranslators = Map.fromList [ (LATEX Concrete, [e| Right . show . LatexPrint.printLatex |])
                                     , (DEQ Abstract,   [e| Right . LatexSyntax.unLatex |]) ]
    latexCTranslators = Map.fromList [ (LATEX Abstract, [e| LatexLex.lexLatex >=> LatexParse.parseLatex |]) ]
    rnetCTranslators =  Map.fromList [ (RNET Abstract,  [e| RNetLex.lexRNet >=> RNetParse.parseRNet |] ) ]
    rnetATranslators =  Map.fromList [ (ESL Abstract,   [e| reactionsAsModel |]) ]
    topoCTranslators =  Map.fromList [ (TOPO Abstract,  [e| (Aeson.eitherDecode :: B.ByteString -> Either String Net) . B.pack |]) ]
    topoATranslators =  Map.fromList [ (TOPO Concrete,  [e| Right . B.unpack . (Aeson.encode :: Net -> B.ByteString) |])
                                     , (ESL Abstract,   [e| Right . topologyAsModel |]) ]

    mkNode :: Map ModelType (Q Exp) -> ModelType -> [ModelType] -> (Map ModelType (Q Exp), ModelType, [ModelType])
    mkNode translators model links
      | and [Map.member link translators | link <- links] = (translators, model, links)
      | otherwise = panic "Error in format graph construction" [ "When creating node representing "++show model, "with links to "++show links ]


-- | For testing - synthesize all possible model-to-model journeys and, for all
-- that produce a valid path, declare them with a relevant name. The only occurrence
-- of `fail` that this `recover` would catch is triggered when no path can be
-- found between the model types.
allConverters :: Q [Dec]
allConverters = concat <$> mapM (\pair -> pure [] `recover` mkConv pair) pairs
  where
    mkConv :: (Vertex, Vertex) -> Q [Dec]
    mkConv pair =
      do  let (from, to) = verticesToNodes pair
              name = mkName $ filter (not . isSpace) $ show from<>"_to_"<>show to
          converter' (show name) from to

    snd3 (_,y,_) = y
    pairs = mapMaybe pairToVertices $ allPairs allModels

    allPairs :: [a] -> [(a, a)]
    allPairs xs = concatMap (\x -> map (x,) xs) xs

    allModels :: [ModelType]
    allModels = 
      [ ESL Concrete
      , ESL Abstract
      , DEQ Concrete
      , DEQ Abstract
      , RNET Concrete
      , RNET Abstract
      , TOPO Concrete
      , TOPO Abstract
      , LATEX Concrete
      , LATEX Abstract
      ]

    verticesToNodes :: (Vertex, Vertex) -> (ModelType, ModelType)
    verticesToNodes (v1, v2) = (snd3 $ nodeFromVertex v1, snd3 $ nodeFromVertex v2)

    pairToVertices :: (ModelType, ModelType) -> Maybe (Vertex, Vertex)
    pairToVertices (from, to) =
      case (vertexFromKey from, vertexFromKey to) of
        (Nothing, Nothing) -> Nothing
        (Just _, Nothing) -> Nothing
        (Nothing, Just _) -> Nothing
        (Just fromV, Just toV) -> Just (fromV, toV)
-}