{-# Language OverloadedStrings, RecordWildCards #-}
module Language.ASKEE.Model.Interface where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Aeson as JSON
import Data.Aeson ((.=),ToJSON(..))

import Language.ASKEE.Model.Basics


-- | This describes the possible inputs and outputs of a model
data ModelInterface = ModelInterface
  { modelInputs   :: [ Port ]
  , modelOutputs  :: [ Port ]
  } deriving (Show, Eq, Ord)

emptyModelInterface :: ModelInterface
emptyModelInterface = ModelInterface { modelInputs = [], modelOutputs = [] }

-- | This describes an input or an output to a model
data Port = Port
  { portName      :: Text                -- ^ Identifies the port
  , portValueType :: ValueType           -- ^ Type of values for this port
  , portDefault   :: Maybe Value         -- ^ Only for input ports
  , portMeta      :: Map Text [Text]     -- ^ Extra information
  } deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

instance ToJSON ModelInterface where
  toJSON mi = JSON.object
    [ "parameters" .= modelInputs mi
    , "measures"   .= modelOutputs mi
    ]

instance ToJSON Port where
  toJSON p = JSON.object $
    dflt ++
    [ "uid"         .= portName p
    , "value_type"  .= portValueType p
    , "metadata"    .=
        JSON.object [ x .= js y
                    | (x,y) <- Map.toList (portMeta p), not (null y)
                    ]
    ]
    where
    js y = case y of
             [v] -> toJSON v
             _   -> toJSON y

    dflt = case portDefault p of
             Nothing -> []
             Just d  -> [ "default" .= d ]

