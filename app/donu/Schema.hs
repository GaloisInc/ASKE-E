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

import Language.ASKEE(DataSource(..))
import Language.ASKEE.DataSeries



data Input =
    Simulate SimulateCommand
  | Fit FitCommand
    deriving Show

instance HasSpec Input where
  anySpec =   (Simulate <$> anySpec)
         -- <!>  (Fit      <$> anySpec)

instance JS.FromJSON Input where
  parseJSON v =
    case importJSON anySpec v of
      Right a  -> pure a
      Left err -> fail (show err) -- XXX: better reporting?

--------------------------------------------------------------------------------
data FitCommand = FitCommand
  { fitModelType :: ModelType
  , fitModel     :: DataSource
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


helpHTML :: Lazy.ByteString
helpHTML = docsJSON (anySpec :: ValueSpec Input)

data Output =
  OutputData (DataSeries Double)


instance JS.ToJSON Output where
  toJSON out =
    case out of
      OutputData d -> dsToJSON d

-- XXX: how do we document this?
dsToJSON :: DataSeries Double -> JS.Value
dsToJSON ds = JS.object
  [ "times"  .= times ds
  , "values" .= JS.object [ x .= ys | (x,ys) <- Map.toList (values ds) ]
  ]


