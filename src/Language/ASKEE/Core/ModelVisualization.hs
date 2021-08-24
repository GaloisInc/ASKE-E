{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Language.ASKEE.Core.ModelVisualization where

import           Control.Monad.IO.Class

import           Data.Text  ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe ( fromJust, mapMaybe )

import           Data.Aeson( (.=) )
import qualified Data.Aeson as JSON

import           System.Directory ( removeFile )
import           System.Exit ( ExitCode(ExitSuccess) )
import           System.IO.Temp ( withSystemTempFile, emptySystemTempFile )
import           System.IO ( hClose )
import qualified System.Process as Proc

import           Language.ASKEE.Core.Expr
import           Language.ASKEE.Core.Syntax

-------------------------------------------------------------------------

data NodeType = EventNode | StateNode
  deriving(Show, Eq, Ord)

data Node =
  Node  { nodeName :: Text
        , nodeType :: NodeType
        }
  deriving(Show, Eq, Ord)

data EdgeType = DirectEdge | IndirectEdge
  deriving(Show, Eq, Ord)

data Edge =
  Edge { edgeSource :: Int
       , edgeTarget :: Int
       , edgeType   :: EdgeType
       }
  deriving(Show, Eq, Ord)

data Graph =
  Graph { nodes :: [Node]
        , edges :: [Edge]
        }
  deriving(Show, Eq, Ord)


stateNode :: Ident -> Node
stateNode t = Node t StateNode

eventNode :: Ident -> Node
eventNode t = Node t EventNode

directEdge :: Int -> Int -> Edge
directEdge source target = Edge source target DirectEdge

indirectEdge :: Int -> Int -> Edge
indirectEdge source target = Edge source target IndirectEdge

makeGraph :: Set.Set Node -> Set.Set (Node, Node) -> Set.Set (Node, Node) -> Graph
makeGraph nodes directEdges indirectEdges = Graph (Set.toList nodes) allEdges
  where
    nodesWithIndices = Map.fromList $ zip (Set.toList nodes) [0..]
    direct = Set.map (edge DirectEdge) directEdges
    indirect = Set.map (edge IndirectEdge) indirectEdges
    allEdges = Set.toList $ Set.union direct indirect
    edge edgeType (n1, n2) = Edge (nodesWithIndices Map.! n1) (nodesWithIndices Map.! n2) edgeType

class RenderedModel m where
  asString :: m -> BS.ByteString
  
  
data MVShowIndirectEdges = MVNoIndirectEdges | MVIndirectEdgesFull | MVIndirectEdgesMinimal
  deriving (Eq, Ord, Show)

newtype MVConfig = MVConfig { mvShowIndirectEdges :: MVShowIndirectEdges }
  deriving (Eq, Ord, Show)

---------------------------------------------------------------------------------  

toNetworkGraph :: Model -> Graph
toNetworkGraph model =
  let allNodes = Set.fromList $ Map.elems stateNodesMap ++ Map.elems eventNodesMap
      allEdgesRaw = Set.unions $ map eventEdges (modelEvents model)
  in makeGraph allNodes allEdgesRaw Set.empty
  where
    stateNameSet = Set.unions $ map (Map.keysSet . eventEffect) (modelEvents model)
    stateNodesMap = Map.fromSet stateNode stateNameSet
    eventNodesMap = Map.fromList $ map (\ev -> (ev, eventNode $ eventName ev)) (modelEvents model)
    stateDependencies e = Set.intersection stateNameSet (collectVars e)
    eventEdges :: Event -> Set.Set (Node, Node)
    eventEdges ev =
      let evNode = eventNodesMap Map.! ev
          edgeToState st = (evNode, stateNodesMap Map.! st)
          edgeToEvent st = (stateNodesMap Map.! st, evNode)
          outGoingEdges = Set.fromList $ map edgeToState $ Map.keys (eventEffect ev)
          allstateDependencies = Set.unions $ map stateDependencies $ Map.elems (eventEffect ev)
          incomingEdges = Set.map edgeToEvent allstateDependencies
      in Set.union incomingEdges outGoingEdges

toFlowGraph :: MVConfig -> Model -> Graph
toFlowGraph MVConfig{..} model =
  let allNodes = Set.fromList $ Map.elems stateNodesMap ++ Map.elems eventNodesMap
      dirEdgesRaw = Set.unions $ map dirEventEdges (modelEvents model)
      indirectEdgesRaw = Set.unions $ map indirectEventEdges (modelEvents model)
      filteredIndirectEdgesRaw = filterIndirectEdges indirectEdgesRaw dirEdgesRaw
  in makeGraph allNodes dirEdgesRaw filteredIndirectEdgesRaw
  where
    stateNameSet = Set.unions $ map (Map.keysSet . eventEffect) (modelEvents model)
    stateNodesMap = Map.fromSet stateNode stateNameSet
    eventNodesMap = Map.fromList $ map (\ev -> (ev, eventNode $ eventName ev)) (modelEvents model)
    dirEventEdges ev = Set.fromList $ mapMaybe (uncurry $ effectEdge ev) $ Map.assocs (eventEffect ev)
      where
        effectEdge evt _ e =
          case e of
            Var v :+: NumLit n | n > 0 ->
              Just (eventNode (eventName evt), stateNode v)
            Var v :-: NumLit n | n > 0 ->
              Just (stateNode v, eventNode (eventName evt))
            _ ->
              Nothing
    indirectEventEdges ev =
      let evNode = eventNodesMap Map.! ev
          sourceVars = collectVars $ eventRate ev
          sourceVarNodes = mapMaybe (`Map.lookup` stateNodesMap) $ Set.toList sourceVars
      in Set.fromList $ map (,evNode) sourceVarNodes
    filterIndirectEdges indirectEdges directEdges =
      let invertedEdges = Set.map (\(n1, n2) -> (n2, n1)) directEdges
          allEdgesToRemove = Set.union directEdges invertedEdges
      in case mvShowIndirectEdges of
        MVNoIndirectEdges -> Set.empty
        MVIndirectEdgesFull -> Set.difference indirectEdges directEdges
        MVIndirectEdgesMinimal -> Set.difference indirectEdges allEdgesToRemove

toSimpleFlowGraph :: MVConfig -> Model -> Graph
toSimpleFlowGraph MVConfig{..} model =
  let allNodes = Set.fromList $ Map.elems stateNodesMap
      dirEdgesRaw = Set.unions $ map dirEventEdges (modelEvents model)
      indirectEdgesRaw = Set.unions $ map indirectEventEdges (modelEvents model)
      filteredIndirectEdgesRaw = filterIndirectEdges indirectEdgesRaw dirEdgesRaw
  in makeGraph allNodes dirEdgesRaw filteredIndirectEdgesRaw
  where
    stateNameSet = Set.unions $ map (Map.keysSet . eventEffect) (modelEvents model)
    stateNodesMap = Map.fromSet stateNode stateNameSet
    dirEventEdges ev = directEffectEdges (eventEffect ev)      
    directEffectEdges effectMap =
      let (increases, decreases) = classify effectMap
          edgelst = edgeList decreases increases
      in Set.fromList edgelst
    classify effectMap =
      let classifiedExprMap = Map.mapMaybe isIncrease effectMap
          (increases, decreases) = Map.partition Prelude.id classifiedExprMap
      in (Map.keys increases, Map.keys decreases)
    isIncrease e =
      case e of
        Var _ :+: NumLit n | n > 0 -> Just True
        Var _ :-: NumLit n | n > 0 -> Just False
        _ -> Nothing
    edgeList fromSet endSet =
      [ (lNode, rNode) | l <- fromSet, let lNode = stateNodesMap Map.! l,
                         r <- endSet, let rNode = stateNodesMap Map.! r,
                         l /= r]
    indirectEventEdges ev = indirectEffectEdges (eventEffect ev) (eventRate ev)
    indirectEffectEdges effectMap rateExpr =
      let (increases, _) = classify effectMap
          sourceVars = Set.toList $ Set.filter (`Map.member` stateNodesMap) $ collectVars rateExpr
          edgelst = edgeList sourceVars increases
      in Set.fromList edgelst
    filterIndirectEdges indirectEdges directEdges =
      case mvShowIndirectEdges of
        MVNoIndirectEdges -> Set.empty
        MVIndirectEdgesFull -> Set.difference indirectEdges directEdges
        MVIndirectEdgesMinimal -> Set.difference indirectEdges directEdges

-------------------------------------------------------------------------------------

convertToVega :: Graph -> JSON.Value
convertToVega g =
  -- Load the template, convert to JSON and add our data to it  !
  let template = JSON.decodeStrict (encodeUtf8 vegaNetworkGraphTemplate) :: Maybe JSON.Value
      JSON.Object obj = fromJust template
      graphValue = HashMap.insert "data" (convertToVegaData g) obj
  in JSON.Object graphValue

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
                 StateNode -> JSON.String "state"
                 EventNode -> JSON.String "event"
      convertEdge edge =
        JSON.object [ "source" .= edgeSource edge
                    , "target" .= edgeTarget edge
                    , "type"   .= JSON.String (edgeType edge)
                    ]

      edgeType edge =
        let src = nodes g !! edgeSource edge
        in case src of { Node _ EventNode -> "out"; _ -> "in" }


---------------------------------------------------------------------

renderModelAsFlowGraphToVega :: MVConfig -> Model -> JSON.Value
renderModelAsFlowGraphToVega config model = convertToVega (toFlowGraph config model)

renderModelAsFlowGraphToVegaIO :: MonadIO m => MVConfig -> Model -> FilePath -> m ()
renderModelAsFlowGraphToVegaIO config model f = liftIO $ JSON.encodeFile f (renderModelAsFlowGraphToVega config model)

renderModelAsSimpleFlowGraphToVega :: MVConfig -> Model -> JSON.Value
renderModelAsSimpleFlowGraphToVega config model = convertToVega (toSimpleFlowGraph config model)

renderModelAsSimpleFlowGraphToVegaIO :: MonadIO m => MVConfig -> Model -> FilePath -> m ()
renderModelAsSimpleFlowGraphToVegaIO config model f = liftIO $ JSON.encodeFile f (renderModelAsSimpleFlowGraphToVega config model)

---------------------------------------------------------------------

-- This really should go in to some config file somewhere!
-- Quoting like this is remarkably ugly ..
vegaNetworkGraphTemplate :: Text
vegaNetworkGraphTemplate = " \
 \ { \
 \   \"$schema\": \"https://vega.github.io/schema/vega/v5.json\", \
 \   \"description\": \"A network representation of a model\", \
 \   \"width\": 1200, \
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
 \           \"shape\": {\"signal\": \"datum.type === 'event' ? 'square' : 'circle'\"}, \
 \           \"size\": {\"signal\": \"datum.type === 'event' ? nodeRadius * nodeRadius / 2 : 2 * nodeRadius * nodeRadius\"}, \
 \           \"name\": {\"field\": \"name\"}, \
 \           \"type\": {\"field\": \"type\"}, \
 \           \"tooltip\": [ {\"field\": \"name\"} ] \
 \         } \
 \       }, \
 \  \
 \       \"transform\": [ \
 \         { \
 \           \"type\": \"force\", \
 \           \"iterations\": 10000, \
 \           \"static\": true, \
 \           \"signal\": \"force\", \
 \           \"forces\": [ \
 \             {\"force\": \"center\", \"x\": {\"signal\": \"cx\"}, \"y\": {\"signal\": \"cy\"}}, \
 \             {\"force\": \"collide\", \"radius\": {\"signal\": \"nodeRadius\"}}, \
 \             {\"force\": \"nbody\", \"strength\": {\"signal\": \"nodeCharge\"}}, \
 \             {\"force\": \"link\", \"links\": \"link-data\", \"distance\": {\"signal\": \"linkDistance\"}}, \
 \             {\"force\": \"y\", \"y\": 0, \"strength\": \"0.01\" } \
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
 \           \"stroke\": {\"signal\": \"datum.type === 'out' ? 'gray' : 'green'\"}, \
 \           \"strokeWidth\": {\"signal\": \"datum.type === 'out' ? 2 : 3.5\"} \
 \         } \
 \       }, \
 \       \"transform\": [ \
 \         { \
 \           \"type\": \"linkpath\", \
 \           \"require\": {\"signal\": \"force\"}, \
 \           \"shape\": \"line\", \
 \           \"sourceX\": \"datum.source.x\", \"sourceY\": \"datum.source.y\", \
 \           \"targetX\": \"datum.target.x\", \"targetY\": \"datum.target.y\" \
 \         } \
 \       ] \
 \     }, \
 \     { \
 \    \"encode\": { \
 \        \"enter\": { \
 \            \"size\": { \"value\": 250}, \
 \            \"base\": { \"signal\": \"datum\" } \
 \        } \
 \    }, \
 \    \"transform\": [ \
 \        { \
 \          \"shape\": \"line\", \
 \          \"sourceX\": \"dummyTX\", \
 \          \"sourceY\": \"dummySY\", \
 \          \"targetX\": \"dummyTX\", \
 \          \"targetY\": \"dummyTY\", \
 \          \"require\": { \"signal\": \"force\" }, \
 \          \"type\": \"linkpath\" \
 \        }, \
 \        { \
 \          \"type\": \"formula\", \
 \          \"expr\": \"(datum.base.target.x + datum.base.source.x) / 2\", \
 \          \"as\": \"x\" \
 \        }, \
 \        { \
 \          \"type\": \"formula\", \
 \          \"expr\": \"(datum.base.target.y + datum.base.source.y) / 2\", \
 \          \"as\": \"y\" \
 \        }, \
 \        { \
 \          \"type\": \"formula\", \
 \          \"expr\": \"180 * atan((datum.base.target.y - datum.base.source.y) / (datum.base.target.x - datum.base.source.x)) / PI\", \
 \          \"as\": \"angle\" \
 \        }, \
 \        { \
 \          \"type\": \"formula\", \
 \          \"expr\": \"datum.base.target.x > datum.base.source.x ? 'triangle-right' : 'triangle-left'\", \
 \          \"as\": \"shape\" \
 \        } \
 \    ], \
 \      \"from\": { \"data\": \"link-data\" }, \
 \      \"type\": \"symbol\", \
 \      \"name\": \"arrows\", \
 \      \"zindex\": 4 \
 \    }, \
 \    { \
 \       \"zindex\": 2, \
 \       \"encode\": { \
 \         \"enter\": { \
 \           \"y\": { \"field \":  \"y\"}, \
 \           \"x\": { \"field \":  \"x\"}, \
 \           \"fill\": { \"value\":  \"white\"}, \
 \           \"fontSize\": { \"value\": 20}, \
 \           \"fontWeight\": { \"value\":  \"bold\"}, \
 \           \"baseline\": { \"value\":  \"middle\"}, \
 \           \"align\": { \"value\":  \"center\"}, \
 \           \"text\": { \"signal\":  \"datum.type === 'event' ? '' : datum.name[0]\"}, \
 \           \"tooltip\": [{ \"field\":  \"name\"}] \
 \        } \
 \      }, \
 \       \"from\": { \"data\":  \"nodes\"}, \
 \       \"type\":  \"text\" \
 \    }, \
 \    { \
 \       \"type\": \"text\", \
 \       \"from\": {\"data\": \"nodes\"}, \
 \       \"zindex\": 2, \
 \       \"encode\": { \
 \         \"enter\": { \
 \           \"text\": { \"signal\": \"datum.type === 'event' ? '' : datum.name[0] \"}, \
 \           \"tooltip\": [ {\"field\": \"name\"} ], \
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

---------------------------------------------------------------------

newtype GVId = GVId { unId :: Text }
  deriving(Show, Eq, Ord)

data GVValue = GVValInt Int | GVValFloat Double | GVValString Text
  deriving(Show, Eq, Ord)

data GVNode = GVNode { gvNodeId    :: GVId
                     , gvNodeLabel :: Text
                     , gvNodeAttr  :: Map.Map Text GVValue
                     }
  deriving(Show, Eq, Ord)

data GVEdge = GVEdge { gvEdgeStart :: GVId
                     , gvEdgeEnd   :: GVId
                     , gvEdgeAttr  :: Map.Map Text GVValue
                     }
  deriving(Show, Eq, Ord)

data GVDiGraph = GVDiGraph { gvGraphId    :: GVId
                           , gvGraphAttr  :: Map.Map Text GVValue
                           , gvGraphNodes :: [GVNode]
                           , gvGraphEdges :: [GVEdge]
                           }
  deriving(Show, Eq, Ord)


generateDotText :: GVDiGraph -> Text
generateDotText g =
  T.concat [ "digraph "
           , idText (gvGraphId g)
           , wrapped " {\n" "\n}" graphContents
           ]
  where
    attrsText sep attrMap = T.intercalate sep $ map attrText $ Map.assocs attrMap
    attrText (key, value) = T.concat [key, "=", valueText value]
    valueText (GVValInt i) = T.pack (show i)
    valueText (GVValString s) = T.snoc (T.cons '"' s) '"'
    valueText (GVValFloat f) = T.pack (show f)
    idText i = unId i
    nodeText n = T.intercalate " "  [ "node"
                                    , wrappedAttrsText (augmentedNodeAttrMap n)
                                    , ";"
                                    , idText (gvNodeId n)
                                    ]
    edgeText e = T.intercalate " " [ idText (gvEdgeStart e)
                                   , "->"
                                   , idText (gvEdgeEnd e)
                                   , wrappedAttrsText (gvEdgeAttr e)
                                   ]
    graphContents = T.intercalate " ;\n" [ attrsText " ;\n" (gvGraphAttr g)
                                         , T.intercalate " ;\n" (map nodeText $ gvGraphNodes g)
                                         , T.intercalate " ;\n" (map edgeText $ gvGraphEdges g)
                                         ]
    augmentedNodeAttrMap n = Map.insert "label" (GVValString $ gvNodeLabel n) $ gvNodeAttr n
    wrappedAttrsText attrMap = wrapped "[" "]" (attrsText ", " attrMap)
    wrapped l r s = T.concat [l, s, r]

---------------------------------------------------------------------

convertToDot :: Graph -> GVDiGraph
convertToDot g =
  let gvGraphId = GVId "Model"
      gvGraphAttr = Map.fromList [ ("layout", GVValString "dot")
                                 , ("fontsize", GVValInt 20)
                                 , ("rankdir", GVValString "LR")
                                 ]
      gvGraphNodes = zipWith makeGVNode (nodes g) [0..]
      gvGraphEdges = map makeGVEdge (edges g)
  in GVDiGraph{..}
  where
    makeGVNode n i =
      let gvNodeId = nodeId i
          gvNodeLabel = case nodeType n of
            EventNode -> ""
            StateNode -> maybe "" (T.singleton . fst) $ T.uncons (nodeName n)
          gvNodeAttr = nodeAttr n
      in GVNode{..}
    makeGVEdge e =
      let gvEdgeStart = nodeId (edgeSource e)
          gvEdgeEnd = nodeId (edgeTarget e)
          gvStyle = case edgeType e of
                      DirectEdge   -> "solid"
                      IndirectEdge -> "dashed"
          gvEdgeAttr = Map.fromList [ ("color", GVValString "darkslategray4")
                                    , ("penwidth", GVValFloat 2.5)
                                    , ("arrowsize", GVValFloat 0.5)
                                    , ("style", GVValString gvStyle)
                                    ]
      in GVEdge{..}
    nodeAttr n = case nodeType n of
      EventNode -> Map.fromList [ ("shape", GVValString "square")
                                , ("width", GVValFloat 0.2)
                                , ("style", GVValString "filled")
                                , ("fillcolor", GVValString "orange")
                                , ("penwidth", GVValFloat 1.5)
                                , ("tooltip", GVValString $ nodeName n)
                                ]
      StateNode -> Map.fromList [ ("shape", GVValString "circle")
                                , ("width", GVValFloat 0.5)
                                , ("style", GVValString "filled")
                                , ("fillcolor", GVValString "blue4")
                                , ("fontcolor", GVValString "white")
                                , ("fontname", GVValString "Arial")
                                , ("penwidth", GVValFloat 1.5)
                                , ("tooltip", GVValString $ nodeName n)
                                ]
    nodeId :: Int -> GVId
    nodeId i = GVId $ T.cons 'n' (T.pack $ show i)

-----------------------------------------------------------------------

data ImageType = ImagePng | ImageJpg | ImageSvg
  deriving (Eq, Ord, Show)

renderGraphToImage :: MonadIO m => Graph -> ImageType -> FilePath -> m (Either Text ())
renderGraphToImage g iType f =
  liftIO (withSystemTempFile "model.dot" renderToImage)
  where
    renderToImage srcPath handle = writeDotFile handle >> execGraphViz srcPath f
    writeDotFile handle = do
      T.hPutStrLn handle $ generateDotText $ convertToDot g
      hClose handle
    execGraphViz :: FilePath -> FilePath -> IO (Either Text ())
    execGraphViz sourceFile destFile = do
      let params = [ "-T", imageType iType
                   , "-o", destFile
                   , sourceFile
                   ]
      (code, _, err) <- Proc.readProcessWithExitCode "dot" params ""
      return $ case code of
        ExitSuccess -> Right ()
        _           -> Left (T.pack err)
    imageType t = T.unpack $ case t of
      ImagePng -> "png"
      ImageJpg -> "jpg"
      ImageSvg -> "svg"

renderGraphToRawImage :: MonadIO m => Graph -> ImageType -> m (Either Text BS.ByteString)
renderGraphToRawImage g iType = do
  destFile <- liftIO $ emptySystemTempFile "model.output"
  perhapsResult <- renderGraphToImage g iType destFile
  case perhapsResult of
    Right () -> do
      dta <- liftIO $ Right <$> BS.readFile destFile
      liftIO $ removeFile destFile
      return dta
    Left t   -> return $ Left t


renderModelAsFlowGraphToDot :: MVConfig -> Model -> Text
renderModelAsFlowGraphToDot config model = generateDotText $ convertToDot $ toFlowGraph config model

renderModelAsFlowGraphToDotIO :: MonadIO m => MVConfig -> Model -> FilePath -> m ()
renderModelAsFlowGraphToDotIO config model f = liftIO $ T.writeFile f $ renderModelAsFlowGraphToDot config model

renderModelAsFlowGraphToImageIO :: MonadIO m => MVConfig -> Model -> ImageType -> FilePath -> m (Either Text ())
renderModelAsFlowGraphToImageIO config model iType f = renderGraphToImage (toFlowGraph config model) iType f

renderModelAsFlowGraphToRawImageIO :: MonadIO m => MVConfig -> Model -> ImageType -> m (Either Text BS.ByteString)
renderModelAsFlowGraphToRawImageIO config model iType = renderGraphToRawImage (toFlowGraph config model) iType


renderModelAsSimpleFlowGraphToDot :: MVConfig -> Model -> Text
renderModelAsSimpleFlowGraphToDot config model = generateDotText $ convertToDot $ toSimpleFlowGraph config model

renderModelAsSimpleFlowGraphToDotIO :: MonadIO m => MVConfig -> Model -> FilePath -> m ()
renderModelAsSimpleFlowGraphToDotIO config model f = liftIO $ T.writeFile f $ renderModelAsSimpleFlowGraphToDot config model

renderModelAsSimpleFlowGraphToImageIO :: MonadIO m => MVConfig -> Model -> ImageType -> FilePath -> m (Either Text ())
renderModelAsSimpleFlowGraphToImageIO config model iType f = renderGraphToImage (toSimpleFlowGraph config model) iType f

renderModelAsSimpleFlowGraphToRawImageIO :: MonadIO m => MVConfig -> Model -> ImageType -> m (Either Text BS.ByteString)
renderModelAsSimpleFlowGraphToRawImageIO config model iType = renderGraphToRawImage (toSimpleFlowGraph config model) iType