{-# Language ApplicativeDo, RecordWildCards, BlockArguments, OverloadedStrings, GADTs, FlexibleInstances #-}
module Schema where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Maybe(fromMaybe)
import Data.Functor(($>))
import Data.Functor.Alt ((<!>))

import qualified Data.Aeson as JS
import           Data.Aeson ((.=))
import SchemaJS

import Language.ASKEE(DataSource(..), StratificationType(..), StratificationInfo(..))
import Language.ASKEE.DataSeries
import Language.ASKEE.Core ( Ident )
import Language.ASKEE.Print (printModel)


data Input =
    Simulate SimulateCommand
  | Fit FitCommand
  | CheckModel CheckModelCommand
  | ConvertModel ConvertModelCommand
  | GenerateCPP GenerateCPPCommand
  | Stratify StratifyCommand
    deriving Show

instance HasSpec Input where
  anySpec =   (Simulate <$> anySpec)
         <!> (CheckModel <$> anySpec)
         <!> (ConvertModel <$> anySpec)
         <!> (Fit      <$> anySpec)
         <!> (GenerateCPP <$> anySpec)
         <!>  (Stratify <$> anySpec)

instance JS.FromJSON Input where
  parseJSON v =
    case importJSON anySpec v of
      Right a  -> pure a
      Left err -> fail (show err) -- XXX: better reporting?

--------------------------------------------------------------------------------
data FitCommand = FitCommand
  { fitModelType :: ModelType
  , fitModel     :: DataSource
  , fitData      :: DataSource
  , fitParams    :: [Text]
  } deriving Show


--------------------------------------------------------------------------------
data SimulateCommand = SimulateCommand
  { simModelType :: ModelType
  , simModel     :: DataSource
  , simStart     :: Double
  , simStep      :: Double
  , simEnd       :: Double
  , simOverwrite :: Map Text Double
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
       simModelType <- reqSection "model"
                       "Type of model to simulate"

       simModel     <- reqSection' "definition" dataSource
                       "Specification of the model"

       simStart     <- reqSection "start"
                       "Start time of simulation"
       simStep      <- fromMaybe 1 <$>
                       optSection "step"
                       "Time step (defaults to 1)"
       simEnd       <- reqSection "end"
                       "End time of simulation"

       simOverwrite <- maybe Map.empty Map.fromList <$>
                       optSection' "overwrite" (assocSpec anySpec)
                       "Use these values for model parameters"

       pure SimulateCommand { .. }

instance HasSpec FitCommand where
  anySpec =
    sectionsSpec "fit-command"
    do  reqSection' "command" (jsAtom "fit") "Fit a model to data"
        fitModelType  <- reqSection "model" 
                         "Type of model to simulate"
         
        fitModel      <- reqSection' "definition" dataSource
                         "Specification of the model"
         
        fitData       <- reqSection' "data" dataSource
                         "Data to which to fit the model"
 
        fitParams     <- reqSection' "parameters" (listSpec textSpec)
                         "Parameters to use in model fitting"

        pure FitCommand {..}

data ModelType = AskeeModel | DiffEqs | ReactionNet | LatexEqnarray
  deriving Show

instance HasSpec ModelType where
  anySpec =  (jsAtom "askee"    $> AskeeModel)
         <!> (jsAtom "diff-eqs" $> DiffEqs)
         <!> (jsAtom "reaction-net" $> ReactionNet)
         <!> (jsAtom "latex-eqnarray" $> LatexEqnarray)


dataSource :: ValueSpec DataSource
dataSource =
  namedSpec "data-source" $
  sectionsSpec "file"
    do f <- reqSection "file" "A file path"
       pure (FromFile (Text.unpack f))
  <!>
    Inline <$> anySpec

stratTypeSpec :: ValueSpec StratificationType 
stratTypeSpec =  (jsAtom "spatial"     $> Spatial)
             <!> (jsAtom "demographic" $> Demographic)

helpHTML :: Lazy.ByteString
helpHTML = docsJSON (anySpec :: ValueSpec Input)

data Output =
  OutputData (DataSeries Double)
  | OutputResult Result
  | FitResult (Map Ident (Double, Double))
  | StratificationResult StratificationInfo


instance JS.ToJSON Output where
  toJSON out =
    case out of
      OutputData d -> dsToJSON d
      OutputResult result -> JS.toJSON result
      FitResult r -> pointsToJSON r
      StratificationResult info -> stratResultToJSON info

-- XXX: how do we document this?
dsToJSON :: DataSeries Double -> JS.Value
dsToJSON ds = JS.object
  [ "times"  .= times ds
  , "values" .= JS.object [ x .= ys | (x,ys) <- Map.toList (values ds) ]
  ]

-------------------------------------------------------------------------------

data CheckModelCommand = CheckModelCommand
  { checkModelModelType :: ModelType
  , checkModelModel     :: DataSource
  } deriving Show

instance HasSpec CheckModelCommand where
  anySpec =
    sectionsSpec "check-model"
    do  reqSection' "command" (jsAtom "check-model") "Validate that a model is runnable"
        checkModelModelType <- reqSection "model"
                       "Type of model to check"

        checkModelModel     <- reqSection' "definition" dataSource
                       "Specification of the model"

        pure CheckModelCommand { .. }


-------------------------------------------------------------------------------

data ConvertModelCommand = ConvertModelCommand
  { convertModelSourceType :: ModelType
  , convertModelDestType   :: ModelType
  , convertModelModel      :: DataSource
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
        convertModelSourceType <- reqSection "model"
                       "Type of source model"

        convertModelModel      <- reqSection' "definition" dataSource
                       "Specification of the source model"

        convertModelDestType <- reqSection "dest-model" "Type of model to produce"

        pure ConvertModelCommand { .. }

pointsToJSON :: Map Ident (Double, Double) -> JS.Value
pointsToJSON ps = JS.object
  [ point .= JS.object ["value" .= value, "error" .= err] 
  | (point, (value, err)) <- Map.toList ps]


-------------------------------------------------------------------------------

-- TODO: maybe delete after demo?
data GenerateCPPCommand = GenerateCPPCommand
  { generateCPPModelType :: ModelType
  , generateCPPModel     :: DataSource
  } deriving Show

instance HasSpec GenerateCPPCommand where
  anySpec =
    sectionsSpec "generate-cpp"
    do  reqSection' "command" (jsAtom "generate-cpp") "Render a model as C++ program implementing a simulator"
        generateCPPModelType <- reqSection "model" "Type of model to render"
        generateCPPModel <- reqSection' "definition" dataSource
                            "Specification of the model to render"

        pure GenerateCPPCommand { .. }


stratResultToJSON :: StratificationInfo -> JS.Value 
stratResultToJSON StratificationInfo{..} =
  JS.object [ "raw-model" .= show (printModel rawModel)
            , "pretty-model" .= show (printModel prettyModel)
            , "topology" .= rawTopology
            , "parameters" .= params
            , "vertices" .= vertices
            ]
