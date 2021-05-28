module Language.ASKEE.Types where
  
import Data.Text ( Text )
import Control.Exception (Exception)
import qualified Language.ASKEE.Syntax as ESL
import qualified Language.ASKEE.ModelStratify.Syntax as Topology
import Data.Map (Map)

data Representation = Abstract | Concrete
  deriving (Eq, Ord, Show)

data ModelType =
    ESL Representation
  | ESLMETA Representation
  | DEQ Representation
  | RNET Representation
  | TOPO Representation
  | LATEX Representation
  | GROMET Representation
  deriving (Eq, Ord, Show)

data DataSource =
    FromFile FilePath
  | Inline Text
  deriving (Eq, Show)

data ModelDef =
    ModelDef { modelDefSource :: DataSource
             , modelDefType   :: ModelType
             }
  deriving (Eq, Show)

data StratificationType = Demographic | Spatial
  deriving Show

data StratificationInfo = StratificationInfo
  { rawModel    :: ESL.Model
  , prettyModel :: ESL.Model
  , rawTopology :: Topology.Net 
  , holes       :: [Text]
  , vertices    :: Map Int Text
  }
  deriving Show

newtype ParseError = ParseError String
  deriving (Show)

instance Exception ParseError

newtype ValidationError = ValidationError String
  deriving (Show)

instance Exception ValidationError

newtype ConversionError = ConversionError String
  deriving (Show)

instance Exception ConversionError

newtype NotImplementedError = NotImplementedError String
  deriving (Show)

instance Exception NotImplementedError