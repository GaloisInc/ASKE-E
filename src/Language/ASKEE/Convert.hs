{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Language.ASKEE.Convert where

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

import qualified Language.ASKEE.ABM.GenLexer  as ABMLex
import qualified Language.ASKEE.ABM.GenParser as ABMParse
import qualified Language.ASKEE.ABM.Print     as ABMPrint
import qualified Language.ASKEE.ABM.Syntax    as ABMSyntax
import           Language.ASKEE.ABM.Translate as ABMTrans

import qualified Language.ASKEE.DEQ.GenLexer  as DEQLex
import qualified Language.ASKEE.DEQ.GenParser as DEQParse
import qualified Language.ASKEE.DEQ.Print     as DEQPrint
import qualified Language.ASKEE.DEQ.Syntax    as DEQSyntax

import qualified Language.ASKEE.GenLexer  as ESLLex
import qualified Language.ASKEE.GenParser as ESLParse
import qualified Language.ASKEE.Print     as ESLPrint
import qualified Language.ASKEE.Syntax    as ESLSyntax

import qualified Language.ASKEE.Latex.GenLexer  as LatexLex
import qualified Language.ASKEE.Latex.GenParser as LatexParse
import qualified Language.ASKEE.Latex.Print     as LatexPrint

import qualified Language.ASKEE.RNet.GenLexer  as RNetLex
import qualified Language.ASKEE.RNet.GenParser as RNetParse
import qualified Language.ASKEE.RNet.Syntax    as RNetSyntax
import           Language.ASKEE.RNet.Reaction  ( reactionsAsModel )

import qualified Language.ASKEE.ModelStratify.Syntax   as TopoSyntax
import           Language.ASKEE.ModelStratify.Topology ( modelAsTopology
                                                       , topologyAsModel )

import Language.ASKEE.ModelStratify.Syntax ( Net(..) )

import Language.Haskell.TH

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
  | ABM_C
  | ABM_A
  deriving (Enum, Eq, Ord)

instance Show ModelType where
  show ESL_C = "esl Concrete Syntax"
  show ESL_A = "esl Abstract Syntax"
  show DEQ_C = "diffEq Concrete Syntax"
  show DEQ_A = "diffEq Abstract Syntax"
  show RNET_C = "rNet Concrete Syntax"
  show RNET_A = "rNet Abstract Syntax"
  show TOPO_A = "topology Abstract Syntax"
  show TOPO_C = "topology Concrete Syntax"
  show LATEX_C = "latex Concrete Syntax"
  show ABM_C = "aBM Concrete Syntax"
  show ABM_A = "aBM Abstract Syntax"

data Repr = Abstract | Concrete
  deriving (Eq, Show)

class Tagged m where
  tagOf :: Repr -> ModelType

instance Tagged ESLSyntax.Model where
  tagOf Abstract = ESL_A
  tagOf Concrete = ESL_C

instance Tagged DEQSyntax.DiffEqs where
  tagOf Abstract = DEQ_A
  tagOf Concrete = DEQ_C

instance Tagged RNetSyntax.ReactionNet where
  tagOf Abstract = RNET_A
  tagOf Concrete = RNET_C

data Latex

instance Tagged Latex where
  tagOf Concrete = LATEX_C
  tagOf Abstract = error "no latex abstract syntax exists"

instance Tagged Net where
  tagOf Abstract = TOPO_A
  tagOf Concrete = TOPO_C

instance Tagged ABMSyntax.Model where
  tagOf Abstract = ABM_A
  tagOf Concrete = ABM_C

typeOf :: ModelType -> Q Type
typeOf m =
  case m of
    ESL_C -> [t| String |]
    ESL_A -> [t| ESLSyntax.Model |]
    DEQ_C -> [t| String |]
    DEQ_A -> [t| DEQSyntax.DiffEqs |]
    RNET_C -> [t| String |]
    RNET_A -> [t| RNetSyntax.ReactionNet |]
    TOPO_C -> [t| String |]
    TOPO_A -> [t| TopoSyntax.Net |]
    LATEX_C -> [t| String |]
    ABM_A -> [t| ABMSyntax.Model |]
    ABM_C -> [t| String |]

typedConverter :: String -> ModelType -> ModelType -> Q [Dec]
typedConverter nm from to =
  do  let conv = converter from to
      funDec <- funD (mkName nm) [clause [] (normalB conv) []]
      typeDec <- sigD (mkName nm) [t| $(typeOf from) -> Either String $(typeOf to) |]
      pure [typeDec, funDec]

-- | Synthesize a converter between the two provided ModelTypes,
-- `panic`ing if no path exists between the two
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
    nodes = [ mkNode eslCTranslators   ESL_C   [ESL_A]
            , mkNode eslATranslators   ESL_A   [DEQ_A, ESL_C, TOPO_A]
            , mkNode deqCTranslators   DEQ_C   [DEQ_A]
            , mkNode deqATranslators   DEQ_A   [DEQ_C, LATEX_C]
            , mkNode latexCTranslators LATEX_C [DEQ_A]
            , mkNode rnetCTranslators  RNET_C  [RNET_A]
            , mkNode rnetATranslators  RNET_A  [ESL_A]
            , mkNode topoCTranslators  TOPO_C  [TOPO_A]
            , mkNode topoATranslators  TOPO_A  [TOPO_C, ESL_A]
            , mkNode abmATranslators   ABM_A   [ABM_C, ESL_A]
            , mkNode abmCTranslators   ABM_C   [ABM_A]
            ]

    eslCTranslators =   Map.fromList [ (ESL_A,   [e| ESLLex.lexModel >=> ESLParse.parseModel |]) ]
    eslATranslators =   Map.fromList [ (DEQ_A,   [e| fmap asEquationSystem . modelAsCore [] |])
                                     , (ESL_C,   [e| Right . show . ESLPrint.printModel |])
                                     , (TOPO_A,  [e| Right . modelAsTopology |]) ]
    deqCTranslators =   Map.fromList [ (DEQ_A,   [e| DEQLex.lexDEQs >=> DEQParse.parseDEQs |]) ]
    deqATranslators =   Map.fromList [ (DEQ_C,   [e| Right . show . DEQPrint.ppDiffEqs |])
                                     , (LATEX_C, [e| Right . show . LatexPrint.printLatex |]) ]
    latexCTranslators = Map.fromList [ (DEQ_A,   [e| LatexLex.lexLatex >=> LatexParse.parseLatex |]) ]
    rnetCTranslators =  Map.fromList [ (RNET_A,  [e| RNetLex.lexRNet >=> RNetParse.parseRNet |] ) ]
    rnetATranslators =  Map.fromList [ (ESL_A,   [e| reactionsAsModel |]) ]
    topoCTranslators =  Map.fromList [ (TOPO_A,  [e| Aeson.eitherDecode @Net . B.pack |]) ]
    topoATranslators =  Map.fromList [ (TOPO_C,  [e| Right . B.unpack . Aeson.encode @Net |])
                                     , (ESL_A,   [e| Right . topologyAsModel |]) ]
    abmATranslators  = Map.fromList  [ (ABM_C,   [e| Right . show . ABMPrint.printABM |])
                                     , (ESL_A,   [e| Right . ABMTrans.abmToModel |]) ]
    abmCTranslators  = Map.fromList  [ (ABM_A,   [e| ABMLex.lexABM >=> ABMParse.parseABM |]) ]

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
          typedConverter (show name) from to

    snd3 (_,y,_) = y
    pairs = mapMaybe pairToVertices $ allPairs allModels

    allPairs :: [a] -> [(a, a)]
    allPairs xs = concatMap (\x -> map (x,) xs) xs

    allModels :: [ModelType]
    allModels = [ESL_C ..]

    verticesToNodes :: (Vertex, Vertex) -> (ModelType, ModelType)
    verticesToNodes (v1, v2) = (snd3 $ nodeFromVertex v1, snd3 $ nodeFromVertex v2)

    pairToVertices :: (ModelType, ModelType) -> Maybe (Vertex, Vertex)
    pairToVertices (from, to) =
      case (vertexFromKey from, vertexFromKey to) of
        (Nothing, Nothing) -> Nothing
        (Just _, Nothing) -> Nothing
        (Nothing, Just _) -> Nothing
        (Just fromV, Just toV) -> Just (fromV, toV)