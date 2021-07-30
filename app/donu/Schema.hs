{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Schema where

import qualified Data.List as List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as Lazy
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Maybe(fromMaybe, maybeToList)
import Data.Functor(($>))

import qualified Data.Aeson as JS
import           Data.Aeson ((.=))
import SchemaJS

import           Language.ASKEE
import           Language.ASKEE.ESL.Print (printModel)
import           Language.ASKEE.Exposure.Syntax
import qualified Language.ASKEE.Core.Syntax as Core
import           Language.ASKEE.Latex.Print (printLatex)

-------------------------------------------------------------------------------
-- Input

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
  | QueryModels QueryModelsCommand
  | ExecuteExposureCode ExecuteExposureCodeCommand
  | ResetExposureState ResetExposureStateCommand
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
         <!> (QueryModels <$> anySpec)
         <!> (ExecuteExposureCode <$> anySpec)
         <!> (ResetExposureState <$> anySpec)

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
    FromStore f ->
      JS.object [ "model" .= f ]

modelDef :: ValueSpec ModelDef
modelDef =
  sectionsSpec "model-def"
    do  modelDefSource <- reqSection' "source" dataSource "specification of the model"
        modelDefType <- reqSection "type" "model type - valid types are: easel, gromet(coming soon!), diff-eqs, reaction-net, latex-eqnarray"
        pure ModelDef { .. }

dataSource :: ValueSpec DataSource
dataSource =
  namedSpec "data-source" $
  sectionsSpec "named-model"
    do f <- reqSection "model" "A model name"
       pure (FromStore f)
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

simTypeSpec :: ValueSpec SimulationType
simTypeSpec =
  (jsAtom "aj" $> AJ) <!>
  (jsAtom "discrete" $> Discrete) <!>
  (jsAtom "gsl" $> GSL) <!>
  (jsAtom "automates" $> AutomatesSvc)

data SimulateCommand = SimulateCommand
  { simModel              :: ModelDef
  , simStart              :: Double
  , simStep               :: Double
  , simEnd                :: Double
  , simDomainParam        :: Maybe Text
  , simParameterValues    :: Map Text Double
  , simOutputs            :: Set Text
  , simType               :: Maybe SimulationType
  , simSeed               :: Maybe Int
  } deriving Show


instance HasSpec SimulateCommand where
  anySpec = sectionsSpec
    "simulate-command"
    do  reqSection' "command" (jsAtom "simulate") "Run a simulation"
        simModel <- reqSection'
          "definition"
          modelDef
          "Specification of the model to simulate"

        simStart <- reqSection
          "start"
          "Start time of simulation"

        simStep <- fromMaybe 1 <$>
          optSection
            "step"
            "Time step (defaults to 1)"

        simEnd <- reqSection
          "end"
          "End time of simulation"

        simDomainParam <-
          optSection
            "domain_parameter"
            "Domain parameter (for function network gromets, not yet supported)"

        simParameterValues <- maybe Map.empty Map.fromList <$>
          optSection'
            "parameters"
            (assocSpec anySpec)
            "Use these values for model parameters"

        simOutputs <- Set.fromList . fromMaybe [] <$>
          optSection
            "outputs"
            "Which values to output from the simulation"

        simType <-
          optSection'
            "sim-type"
            simTypeSpec
            "Simulation engine to use (defaults to model specific simulation)"

        simSeed <- optSection
          "seed"
          "Seed for simulation (meaningful only for discrete event simulation)"

        pure SimulateCommand { .. }

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

---------------------------------------------------------------------------
-- Query

newtype QueryModelsCommand = QueryModelsCommand
  { queryText :: Text }
  deriving Show

instance HasSpec QueryModelsCommand where
  anySpec =
    sectionsSpec "query-models"
    do  reqSection' "command" (jsAtom "query-models") "Query available models"
        queryText <- reqSection' "text" textSpec "Text to search"
        pure QueryModelsCommand { .. }

-------------------------------------------------------------------------------
-- TODO RGS: Document all of this

newtype ExecuteExposureCodeCommand = ExecuteExposureCodeCommand
  { executeExposureCode :: Text -- ^ The exposure program to parse and execute
  }
  deriving Show

instance HasSpec ExecuteExposureCodeCommand where
  anySpec =
    sectionsSpec "execute-exposure-code"
    do  reqSection' "command" (jsAtom "execute-exposure-code")
                    "Execute an Exposure command"
        executeExposureCode <- reqSection' "code" textSpec "The code to execute"
        pure ExecuteExposureCodeCommand{..}

newtype ResetExposureStateCommand = ResetExposureStateCommand ()
  deriving Show

instance HasSpec ResetExposureStateCommand where
  anySpec =
    sectionsSpec "clear-exposure" $
      do reqSection' "command" (jsAtom "clear-exposure-state") "Reset the current session's interpreter state"
         pure $ ResetExposureStateCommand ()

-- | Wrapper for Value to control precisely how these are passed
-- to clients of Donu
newtype DonuValue =
  DonuValue { unDonuDisplay :: Value }
  deriving Show

instance JS.ToJSON DonuValue where
  -- N.B. review any changes/additions here to see if they should
  -- make their way to Language.ASKEE.Exposure.Print
  toJSON (DonuValue dv) =
    case dv of
      VInt i         -> typedPrim "int" i
      VBool b        -> typedPrim "bool" b
      VDouble dbl    -> typedPrim "double" dbl
      VString str    -> typedPrim "string" str

      VTimed v t ->
        typed "timed" $
          JS.object [ "time"  .= JS.toJSON t
                    , "value" .= JS.toJSON (DonuValue v)
                    ]

      VDataSeries ds ->
        typed "data-series" $
          JS.object [ "time"   .= JS.toJSON (times ds)
                    , "values" .= JS.toJSON (values ds)
                    ]

      VHistogram low hi sz bins ->
        typed "histogram" $
          JS.object [ "min" .= JS.toJSON low
                    , "max" .= JS.toJSON hi
                    , "size" .= JS.toJSON sz
                    , "bins" .= JS.toJSON bins
                    ]

      VPlot xlab ylabs xs yss ->
        typed "plot" $
          JS.object [ "xlabel"  .= JS.toJSON xlab
                    , "ylabels" .= JS.toJSON ylabs
                    , "xs"      .= JS.toJSON xs
                    , "yss"      .= JS.toJSON yss
                    ]
      VScatter xlab ylabs xs yss ->
        typed "scatter" $
          JS.object [ "xlabel"  .= JS.toJSON xlab
                    , "ylabels" .= JS.toJSON ylabs
                    , "xs"      .= JS.toJSON xs
                    , "yss"     .= JS.toJSON yss
                    ]

      VTable labels columns ->
        typed "table" $
          JS.object [ "labels" .= JS.toJSON labels
                    , "rows"   .= JS.toJSON (List.transpose (map (map DonuValue) columns))
                    ]

      VArray vs ->
        typed "array" (JS.toJSON (JS.toJSON . DonuValue <$> vs))

      VLatex l ->
        typedPrim "latex" $ show $ printLatex l

      VModel mdl          -> typedPrim "string" $ "<model " <> Core.modelName mdl <> ">"
      VModelExpr (EVal v) -> JS.toJSON (DonuValue v)

      VModelExpr {}  -> unimplVal "<modelexpr>"
      VDFold {}      -> unimplVal "<dynfold>"
      VSFold {}      -> unimplVal "<sfold>"
      VSuspended     -> unimplVal "<suspended>"

      VSampledData{} -> tbd
      VPoint{}       -> tbd

    where
      typedPrim :: JS.ToJSON a => Text -> a -> JS.Value
      typedPrim ty v = typed ty (JS.toJSON v)

      typed :: Text -> JS.Value -> JS.Value
      typed ty v = JS.object [ "type"  .= JS.toJSON ty
                             , "value" .= JS.toJSON v
                             ]

      unimplVal :: String -> JS.Value
      unimplVal str = typedPrim "string" str

      tbd :: JS.Value
      tbd = error "TBD"
