{-# Language ApplicativeDo, RecordWildCards, BlockArguments, OverloadedStrings #-}
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

import Language.ASKEE(DataSource(..), StratificationType(..))
import Language.ASKEE.DataSeries
import Language.ASKEE.Core ( Ident )
import Language.ASKEE.Syntax ( Model(..) )
import Language.ASKEE.Print (printModel)


data Input =
    Simulate SimulateCommand
  | Fit FitCommand
  | Stratify StratifyCommand
    deriving Show

instance HasSpec Input where
  anySpec =   (Simulate <$> anySpec)
         <!>  (Fit      <$> anySpec)
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
    do  stratModel       <- reqSection' "definition" dataSource
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

data ModelType = AskeeModel | DiffEqs
  deriving Show

instance HasSpec ModelType where
  anySpec =  (jsAtom "askee"    $> AskeeModel)
         <!> (jsAtom "diff-eqs" $> DiffEqs)


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
  | FitResult (Map Ident (Double, Double))
  | StratificationResult Model [Text]


instance JS.ToJSON Output where
  toJSON out =
    case out of
      OutputData d -> dsToJSON d
      FitResult r -> pointsToJSON r
      StratificationResult model params -> stratResultToJSON model params

-- XXX: how do we document this?
dsToJSON :: DataSeries Double -> JS.Value
dsToJSON ds = JS.object
  [ "times"  .= times ds
  , "values" .= JS.object [ x .= ys | (x,ys) <- Map.toList (values ds) ]
  ]

pointsToJSON :: Map Ident (Double, Double) -> JS.Value
pointsToJSON ps = JS.object
  [ point .= JS.object ["value" .= value, "error" .= err] 
  | (point, (value, err)) <- Map.toList ps]

stratResultToJSON :: Model -> [Text] -> JS.Value 
stratResultToJSON m ps =
  JS.object [ "model" .= show (printModel m), "parameters" .= ps ]
