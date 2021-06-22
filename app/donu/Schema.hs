{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Schema where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Maybe(fromMaybe, maybeToList)
import Data.Functor(($>))

import qualified Data.Aeson as JS
import           Data.Aeson ((.=))
import SchemaJS

import Language.ASKEE
import Language.ASKEE.ESL.Print (printModel)

-------------------------------------------------------------------------------
-- Input

data Input =
    SimulateDiscrete SimulateDiscreteCommand
  | SimulateGSL SimulateGSLCommand
  | SimulateAJ SimulateAJCommand
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
  anySpec =  (SimulateDiscrete <$> anySpec)
         <!> (SimulateGSL <$> anySpec)
         <!> (SimulateAJ <$> anySpec)
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

-------------------------------------------------------------------------------
-- Output

newtype FitResult = FitResult (Map Text (Double, Double)) 
  deriving Show

instance JS.ToJSON FitResult where
  toJSON (FitResult ps) = JS.object
    [ point .= JS.object ["value" .= value, "error" .= err] 
    | (point, (value, err)) <- Map.toList ps]

instance JS.ToJSON StratificationInfo where
  toJSON StratificationInfo{..} =
    JS.object [ "raw-model" .= show (printModel rawModel)
              , "pretty-model" .= show (printModel prettyModel)
              , "topology" .= rawTopology
              , "parameters" .= holes
              , "vertices" .= vertices
              ]

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

instance JS.ToJSON ModelDef where
  toJSON m =
    JS.object [ "source" .= dataSourceToJSON (modelDefSource m)
              , "type" .= modelDefType m
              ]

-- ToJSON ModelDef and ToJSON (MetaAnn ModelDef) probably ought to be merged somehow

instance JS.ToJSON (MetaAnn ModelDef) where
  toJSON MetaAnn{..} = 
    let name = maybeToList $ ("name" .=) <$> lookup "name" metaData
        desc = maybeToList $ ("description" .=) <$> lookup "description" metaData
    in  JS.object $
          name ++
          desc ++
          [ "source" .= dataSourceToJSON (modelDefSource metaValue)
          , "type" .= modelDefType metaValue
          ]

instance HasSpec ModelType where
  anySpec =  (jsAtom "easel"      $> EaselType)
         <!> (jsAtom "diff-eqs"   $> DeqType )
         <!> (jsAtom "core"       $> CoreType)
         <!> (jsAtom "gromet-prt" $> GrometPrtType)
         <!> (jsAtom "gromet-pnc" $> GrometPncType)
         <!> (jsAtom "gromet-fnet" $> GrometFnetType)

instance JS.ToJSON ModelType where
  toJSON mt = JS.String $ describeModelType mt

dataSourceToJSON :: DataSource -> JS.Value
dataSourceToJSON ds =
  case ds of
    FromFile f ->
      JS.object [ "file" .= f ]
    Inline s ->
      JS.String s

modelDef :: ValueSpec ModelDef
modelDef =
  namedSpec "model-def" $
  sectionsSpec "model-def"
    do  modelDefSource <- reqSection' "source" dataSource "specification of the model"
        modelDefType <- reqSection "type" "model type - valid types are: easel, gromet(coming soon!), diff-eqs, reaction-net, latex-eqnarray"
        pure ModelDef { .. }

dataSource :: ValueSpec DataSource
dataSource =
  namedSpec "data-source" $
  sectionsSpec "file"
    do f <- reqSection "file" "A file path"
       pure (FromFile (Text.unpack f))
  <!>
    Inline <$> anySpec

helpHTML :: Lazy.ByteString
helpHTML = docsJSON (anySpec :: ValueSpec Input)

--------------------------------------------------------------------------------
-- Fit

data FitCommand = FitCommand
  { fitModel     :: ModelDef
  , fitData      :: DataSource
  , fitParams    :: [Text]
  } deriving Show

instance HasSpec FitCommand where
  anySpec =
    sectionsSpec "fit-command"
    do  reqSection' "command" (jsAtom "fit") "Fit a model to data"
        fitModel      <- reqSection' "definition" modelDef
                         "Specificaiton of the model to simulate"
         
        fitData       <- reqSection' "data" dataSource
                         "Data to which to fit the model"
 
        fitParams     <- reqSection' "parameters" (listSpec textSpec)
                         "Parameters to use in model fitting"

        pure FitCommand {..}

--------------------------------------------------------------------------------
-- Simulate

data SimulateDiscreteCommand = SimulateDiscreteCommand
  { simModelDiscrete           :: ModelDef
  , simStartDiscrete           :: Double
  , simStepDiscrete            :: Double
  , simEndDiscrete             :: Double
  } deriving Show

instance HasSpec SimulateDiscreteCommand where
  anySpec =
    sectionsSpec "simulate-discrete-command"
    do reqSection' "command" (jsAtom "simulate-discrete") "Run a simulation using a discrete event simulator"
       simModelDiscrete   <- reqSection' "definition" modelDef
                       "Specification of the model to simulate"

       simStartDiscrete     <- reqSection "start"
                       "Start time of simulation"
       simStepDiscrete      <- fromMaybe 1 <$>
                       optSection "step"
                       "Time step (defaults to 1)"
       simEndDiscrete       <- reqSection "end"
                       "End time of simulation"

       pure SimulateDiscreteCommand { .. }
data SimulateGSLCommand = SimulateGSLCommand
  { simModelGSL           :: ModelDef
  , simStartGSL           :: Double
  , simStepGSL            :: Double
  , simEndGSL             :: Double
  , simParameterValuesGSL :: Map Text Double
  } deriving Show


instance HasSpec SimulateGSLCommand where
  anySpec =
    sectionsSpec "simulate-gsl-command"
    do reqSection' "command" (jsAtom "simulate-gsl") "Run a simulation"
       simModelGSL   <- reqSection' "definition" modelDef
                       "Specification of the model to simulate"

       simStartGSL     <- reqSection "start"
                       "Start time of simulation"
       simStepGSL      <- fromMaybe 1 <$>
                       optSection "step"
                       "Time step (defaults to 1)"
       simEndGSL       <- reqSection "end"
                       "End time of simulation"

       simParameterValuesGSL <- maybe Map.empty Map.fromList <$>
                       optSection' "parameters" (assocSpec anySpec)
                       "Use these values for model parameters"

       pure SimulateGSLCommand { .. }


data SimulateAJCommand = SimulateAJCommand
  { simModelAJ           :: ModelDef
  , simStartAJ           :: Double
  , simStepAJ            :: Double
  , simEndAJ             :: Double
  , simParameterValuesAJ :: Map Text Double
  } deriving Show


instance HasSpec SimulateAJCommand where
  anySpec =
    sectionsSpec "simulate-aj-command"
    do reqSection' "command" (jsAtom "simulate-aj") "Run a simulation"
       simModelAJ   <- reqSection' "definition" modelDef
                       "Specification of the model to simulate"

       simStartAJ     <- reqSection "start"
                       "Start time of simulation"
       simStepAJ      <- fromMaybe 1 <$>
                       optSection "step"
                       "Time step (defaults to 1)"
       simEndAJ       <- reqSection "end"
                       "End time of simulation"

       simParameterValuesAJ <- maybe Map.empty Map.fromList <$>
                       optSection' "parameters" (assocSpec anySpec)
                       "Use these values for model parameters"

       pure SimulateAJCommand { .. }

--------------------------------------------------------------------------------
-- Stratify

data StratifyCommand = StratifyCommand
  { stratModel       :: ModelDef
  , stratConnections :: String
  , stratStates      :: Maybe String
  , stratType        :: StratificationType
  }
  deriving Show

instance HasSpec StratifyCommand where
  anySpec =
    sectionsSpec "stratify-command"
    do  reqSection' "command" (jsAtom "stratify-command") "Stratify a model"
        stratModel       <- reqSection' "definition" modelDef
                            "specification of the model"

        stratConnections <- reqSection' "connection-graph" stringSpec
                            "connection graph specifying stratification pattern"

        stratStates      <- optSection' "state-metadata" stringSpec
                            "JSON metadata describing infectious states" -- XXX document desired format somewhere

        stratType        <- reqSection' "stratification-type" stratTypeSpec 
                            "type of stratification to perform"
        pure StratifyCommand {..}


stratTypeSpec :: ValueSpec StratificationType 
stratTypeSpec =  (jsAtom "spatial"     $> Spatial)
             <!> (jsAtom "demographic" $> Demographic)

-------------------------------------------------------------------------------
-- Check

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
-- Convert

data ConvertModelCommand = ConvertModelCommand
  { convertModelSource     :: ModelDef
  , convertModelDestType   :: ModelType
  } deriving Show

instance HasSpec ConvertModelCommand where
  anySpec =
    sectionsSpec "convert-model"
    do  reqSection' "command" (jsAtom "convert-model") "Convert a model from one form to another"
        convertModelSource      <- reqSection' "definition" modelDef
                       "Specification of the source model"

        convertModelDestType <- reqSection "dest-type" "Type of model to produce"

        pure ConvertModelCommand { .. }

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

---------------------------------------------------------------------------
-- List

data ListModelsCommand = ListModelsCommand
  deriving Show

instance HasSpec ListModelsCommand where
  anySpec =
    sectionsSpec "list-models"
    do  reqSection' "command" (jsAtom "list-models") "List available models"
        pure ListModelsCommand


---------------------------------------------------------------------------
-- Schematic

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
-- Source

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
-- Upload

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

-------------------------------------------------------------------------------
-- Interface

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
