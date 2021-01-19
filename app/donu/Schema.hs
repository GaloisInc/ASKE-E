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

import Language.ASKEE(DataSource(..))
import Language.ASKEE.DataSeries



data Input =
    Simulate SimulateCommand
  | Fit FitCommand
  | CheckModel CheckModelCommand
  | ConvertModel ConvertModelCommand
    deriving Show

instance HasSpec Input where
  anySpec =   (Simulate <$> anySpec)
          <!> (CheckModel <$> anySpec)
          <!> (ConvertModel <$> anySpec)
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
  | OutputResult Result
instance JS.ToJSON Output where
  toJSON out =
    case out of
      OutputData d -> dsToJSON d
      OutputResult result -> JS.toJSON result

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

