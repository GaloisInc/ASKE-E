{-# Language ApplicativeDo, RecordWildCards, BlockArguments, OverloadedStrings, GADTs, FlexibleInstances #-}
module Schema where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Maybe(fromMaybe)
import Data.Functor(($>))

import qualified Data.Aeson as JS
import           Data.Aeson ((.=))
import SchemaJS

import Language.ASKEE(DataSource(..), StratificationType(..), StratificationInfo(..))
import Language.ASKEE.DataSeries
import Language.ASKEE.Core ( Ident )
import Language.ASKEE.Print (printModel)
import Language.ASKEE.Types


data Input =
    Simulate SimulateCommand
  | Fit FitCommand
  | CheckModel CheckModelCommand
  | ConvertModel ConvertModelCommand
  | GenerateCPP GenerateCPPCommand
  | Stratify StratifyCommand
  | ListModels ListModelsCommand
  | ModelSchemaGraph ModelSchemaGraphCommand
  | GetModelSource GetModelSourceCommand
  | UploadModel UploadModelCommand
  | DescribeModelInterface DescribeModelInterfaceCommand
    deriving Show

instance HasSpec Input where
  anySpec =  (Simulate <$> anySpec)
         <!> (CheckModel <$> anySpec)
         <!> (ConvertModel <$> anySpec)
         <!> (Fit <$> anySpec)
         <!> (GenerateCPP <$> anySpec)
         <!> (Stratify <$> anySpec)
         <!> (ListModels <$> anySpec)
         <!> (ModelSchemaGraph <$> anySpec)
         <!> (GetModelSource <$> anySpec)
         <!> (UploadModel <$> anySpec)
         <!> (DescribeModelInterface <$> anySpec)

instance JS.FromJSON Input where
  parseJSON v =
    case importJSON anySpec v of
      Right a  -> pure a
      Left err -> fail (show err) -- XXX: better reporting?

--------------------------------------------------------------------------------
data FitCommand = FitCommand
  { fitModel     :: ModelDef
  , fitData      :: DataSource
  , fitParams    :: [Text]
  } deriving Show


--------------------------------------------------------------------------------
data SimulateCommand = SimulateCommand
  { simModel           :: ModelDef
  , simStart           :: Double
  , simStep            :: Double
  , simEnd             :: Double
  , simParameterValues :: Map Text Double
  } deriving Show

data StratifyCommand = StratifyCommand
  { stratModel       :: DataSource
  , stratConnections :: DataSource
  , stratStates      :: Maybe DataSource
  , stratType        :: StratificationType
  }
  deriving Show

instance HasSpec StratifyCommand where
  anySpec =
    sectionsSpec "stratify-command"
    do  reqSection' "command" (jsAtom "stratify-command") "Stratify a model"
        stratModel       <- reqSection' "definition" dataSource
                            "specification of the model"

        stratConnections <- reqSection' "connection-graph" dataSource
                            "JSON connection graph specifying stratification pattern"

        stratStates      <- optSection' "state-metadata" dataSource 
                            "JSON metadata describing infectious states ??? document more"

        stratType        <- reqSection' "stratification-type" stratTypeSpec 
                            "type of stratification to perform"
        pure StratifyCommand {..}

instance HasSpec SimulateCommand where
  anySpec =
    sectionsSpec "simulate-command"
    do reqSection' "command" (jsAtom "simulate") "Run a simulation"
       simModel   <- reqSection' "definition" modelDef
                       "Specification of the model to simulate"

       simStart     <- reqSection "start"
                       "Start time of simulation"
       simStep      <- fromMaybe 1 <$>
                       optSection "step"
                       "Time step (defaults to 1)"
       simEnd       <- reqSection "end"
                       "End time of simulation"

       simParameterValues <- maybe Map.empty Map.fromList <$>
                       optSection' "parameters" (assocSpec anySpec)
                       "Use these values for model parameters"

       pure SimulateCommand { .. }

instance HasSpec FitCommand where
  anySpec =
    sectionsSpec "fit-command"
    do  reqSection' "command" (jsAtom "fit") "Fit a model to data"
        fitModel      <- reqSection' "defintion" modelDef
                         "Specificaiton of the model to simulate"
         
        fitData       <- reqSection' "data" dataSource
                         "Data to which to fit the model"
 
        fitParams     <- reqSection' "parameters" (listSpec textSpec)
                         "Parameters to use in model fitting"

        pure FitCommand {..}

-- data ModelType = 
--     AskeeModel 
--   | DiffEqs 
--   | Gromet
--   | ReactionNet
--   | LatexEqnarray
--   deriving (Show, Eq)

data ModelDef =
    ModelDef { modelDefSource :: DataSource
             , modelDefType   :: ModelType
             }
    deriving (Show, Eq)

instance JS.ToJSON ModelDef where
  toJSON m =
    JS.object [ "source" .= dataSourceToJSON (modelDefSource m)
              , "type" .= modelDefType m
              ]

dataSourceToJSON :: DataSource -> JS.Value
dataSourceToJSON ds =
  case ds of
    FromFile f ->
      JS.object [ "file" .= f ]
    Inline s ->
      JS.String s

instance HasSpec ModelType where
  anySpec =  (jsAtom "easel"    $> ESL Concrete)
         <!> (jsAtom "diff-eqs" $> DEQ Concrete)
         <!> (jsAtom "reaction-net" $> RNET Concrete)
         <!> (jsAtom "latex-eqnarray" $> LATEX Concrete)
         <!> (jsAtom "gromet" $> GROMET Concrete)

instance JS.ToJSON ModelType where
  toJSON mt =
    case mt of
      ESL _ -> JS.String "easel"
      DEQ _ -> JS.String "diff-eqs"
      RNET _ -> JS.String "reaction-net"
      LATEX _ -> JS.String "latex-eqnarray"
      GROMET _ -> JS.String "gromet"
      TOPO _ -> JS.String "topology"


dataSource :: ValueSpec DataSource
dataSource =
  namedSpec "data-source" $
  sectionsSpec "file"
    do f <- reqSection "file" "A file path"
       pure (FromFile (Text.unpack f))
  <!>
    Inline <$> anySpec

modelDef :: ValueSpec ModelDef
modelDef =
  namedSpec "model-def" $
  sectionsSpec "model-def"
    do  modelDefSource <- reqSection' "source" dataSource "specification of the model"
        modelDefType <- reqSection "type" "model type - valid types are: easel, gromet(coming soon!), diff-eqs, reaction-net, latex-eqnarray"
        pure ModelDef { .. }



stratTypeSpec :: ValueSpec StratificationType 
stratTypeSpec =  (jsAtom "spatial"     $> Spatial)
             <!> (jsAtom "demographic" $> Demographic)

helpHTML :: Lazy.ByteString
helpHTML = docsJSON (anySpec :: ValueSpec Input)

data Output =
  OutputData (DataSeries Double)
  | OutputResult Result
  | OutputJSON JS.Value
  | OutputModelList [ModelDef]
  | FitResult (Map Ident (Double, Double))
  | StratificationResult StratificationInfo

instance JS.ToJSON Output where
  toJSON out =
    case out of
      OutputData d -> dsToJSON d
      OutputModelList ms ->
        JS.object [ "models" .= ms ]
      OutputResult result -> JS.toJSON result
      FitResult r -> pointsToJSON r
      StratificationResult info -> stratResultToJSON info
      OutputJSON json -> json

-- XXX: how do we document this?
dsToJSON :: DataSeries Double -> JS.Value
dsToJSON ds = JS.object
  [ "times"  .= times ds
  , "values" .= JS.object [ x .= ys | (x,ys) <- Map.toList (values ds) ]
  ]

-------------------------------------------------------------------------------

newtype CheckModelCommand = CheckModelCommand
  { checkModelModel     :: ModelDef
  } deriving Show

instance HasSpec CheckModelCommand where
  anySpec =
    sectionsSpec "check-model"
    do  reqSection' "command" (jsAtom "check-model") "Validate that a model is runnable"
        checkModelModel     <- reqSection' "definition" modelDef
                       "Specification of the model"

        pure CheckModelCommand { .. }


-------------------------------------------------------------------------------

data ConvertModelCommand = ConvertModelCommand
  { convertModelSource     :: ModelDef
  , convertModelDestType   :: ModelType
  } deriving Show

data Result where
  SuccessResult :: (JS.ToJSON a, Show a) => a -> Result
  FailureResult :: Text -> Result

instance Show Result where
  show (SuccessResult s) = "SuccessResult " ++ show s
  show (FailureResult f) = "FailureResult " ++ show f

class AsResult a where
  asResult :: a -> Result

instance (JS.ToJSON a, Show a) => AsResult (Either Text a) where
  asResult (Right val) = SuccessResult val
  asResult (Left err)  = FailureResult err

instance (JS.ToJSON a, Show a) => AsResult (Either String a) where
  asResult (Right val) = SuccessResult val
  asResult (Left err)  = FailureResult (Text.pack err)

instance JS.ToJSON Result where
  toJSON v =
    case v of
      SuccessResult val ->
        JS.object [ "status" .= JS.String "success"
                  , "result" .= JS.toJSON val
                  ]
      FailureResult err ->
        JS.object [ "status" .= JS.String "error"
                  , "error"  .= JS.String err
                  ]

instance HasSpec ConvertModelCommand where
  anySpec =
    sectionsSpec "convert-model"
    do  reqSection' "command" (jsAtom "convert-model") "Convert a model from one form to another"
        convertModelSource      <- reqSection' "definition" modelDef
                       "Specification of the source model"

        convertModelDestType <- reqSection "dest-type" "Type of model to produce"

        pure ConvertModelCommand { .. }

pointsToJSON :: Map Ident (Double, Double) -> JS.Value
pointsToJSON ps = JS.object
  [ point .= JS.object ["value" .= value, "error" .= err] 
  | (point, (value, err)) <- Map.toList ps]


-------------------------------------------------------------------------------

-- TODO: maybe delete after demo?
newtype GenerateCPPCommand = GenerateCPPCommand
  { generateCPPModel     :: ModelDef
  } deriving Show

instance HasSpec GenerateCPPCommand where
  anySpec =
    sectionsSpec "generate-cpp"
    do  reqSection' "command" (jsAtom "generate-cpp") "Render a model as C++ program implementing a simulator"
        generateCPPModel <- reqSection' "definition" modelDef
                            "Specification of the model to render"

        pure GenerateCPPCommand { .. }


stratResultToJSON :: StratificationInfo -> JS.Value 
stratResultToJSON StratificationInfo{..} =
  JS.object [ "raw-model" .= show (printModel rawModel)
            , "pretty-model" .= show (printModel prettyModel)
            , "topology" .= rawTopology
            , "parameters" .= holes
            , "vertices" .= vertices
            ]

---------------------------------------------------------------------------

data ListModelsCommand = ListModelsCommand
  deriving Show

instance HasSpec ListModelsCommand where
  anySpec =
    sectionsSpec "list-models"
    do  reqSection' "command" (jsAtom "list-models") "List available models"
        pure ListModelsCommand


---------------------------------------------------------------------------

newtype ModelSchemaGraphCommand = ModelSchemaGraphCommand
  { modelSchemaGraphModel :: ModelDef
  }
  deriving Show
instance HasSpec ModelSchemaGraphCommand where
  anySpec =
    sectionsSpec "get-model-schematic"
    do  reqSection' "command" (jsAtom "get-model-schematic")
                    "Get simplified graph representation for a model"
        modelSchemaGraphModel <- reqSection' "definition" modelDef
                                              "Specification of the source model"

        pure ModelSchemaGraphCommand { .. }

---------------------------------------------------------------------------

newtype GetModelSourceCommand = GetModelSourceCommand
  { getModelSource :: ModelDef }
  deriving Show

instance HasSpec GetModelSourceCommand where
  anySpec =
    sectionsSpec "get-model-source"
    do  reqSection' "command"  (jsAtom "get-model-source")
                    "Get model source as a string"
        getModelSource <- reqSection' "definition" modelDef "Specification of the source model"

        pure GetModelSourceCommand { .. }


-------------------------------------------------------------------------------

data UploadModelCommand = UploadModelCommand
  { uploadModelName :: Text
  , uploadModelType :: ModelType
  , uploadModelSource :: Text
  }
  deriving (Show)

instance HasSpec UploadModelCommand where
  anySpec =
    sectionsSpec "upload-model"
    do  reqSection' "command" (jsAtom "upload-model") 
                    "Upload new named model"
        uploadModelName <- reqSection' "name" textSpec "Name of the model"
        uploadModelType <- reqSection "type" "Format of the model"
        uploadModelSource <- reqSection' "definition" textSpec "The model itself"
        pure UploadModelCommand{..}
newtype DescribeModelInterfaceCommand =
  DescribeModelInterfaceCommand { describeModelInterfaceSource :: ModelDef  }
  deriving Show

instance HasSpec DescribeModelInterfaceCommand where
  anySpec =
    sectionsSpec "describe-model-interface"
    do  reqSection' "command" (jsAtom "describe-model-interface")
                    "Describe a model's parameters and state variables"
        describeModelInterfaceSource  <- reqSection' "definition" modelDef "Specification of the model"

        pure DescribeModelInterfaceCommand { .. }
