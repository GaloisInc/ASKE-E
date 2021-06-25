module Language.ASKEE.ESL.Syntax where

import qualified Data.Map  as Map
import           Data.Text ( Text )

import           Language.ASKEE.Metadata ( MetaAnn(..) )

import Language.ASKEE.Expr ( Expr )

import Prelude hiding (LT, EQ, GT)

data Model = Model { modelName :: MetaAnn Text
                   , modelDecls :: [MetaAnn Decl]
                   , modelEvents :: [Event]
                   }
  deriving (Show, Eq)


-- data ModelMeta = ModelMeta
--   { modelMetaName :: Text
--   , modelMetaDecls :: [MetaAnn Decl]
--   , modelMetaEvents :: [Event]
--   }
--   deriving Show

-- stripMeta :: ModelMeta -> Model
-- stripMeta mm = Model (modelMetaName mm)
--                      (Meta.metaValue <$> modelMetaDecls mm)
--                      (modelMetaEvents mm)

data Decl = Let   Text Expr
          | State Text Expr
          | Parameter Text (Maybe Double)
          | Assert Expr
  deriving (Show, Eq)   

data Event = Event { eventName     :: Text
                   , eventWhen     :: Maybe Expr
                   , eventRate     :: Expr
                   , eventEffect   :: [Statement]
                   , eventMetadata :: Maybe Text -- XXX: Use MetaAnn
                   }
  deriving (Show, Eq)

type Statement = (Text, Expr)

-------------------------------------------------------------------------------

-- utility functions

stateDecls :: [MetaAnn Decl] -> [(Text, Expr)]
stateDecls ds = [(n,v) | State n v <- map metaValue ds ]

letDecls :: [MetaAnn Decl] -> [(Text, Expr)]
letDecls ds = [(n,v) | Let n v <- map metaValue ds ]

varDecls :: [Decl] -> [(Text, Expr)]
varDecls ds = ds >>= vd
  where
    vd (Let n v) = [(n, v)]
    vd (State n v) = [(n, v)]
    vd _ = []

parameterDecls :: [MetaAnn Decl] -> [(Text, Maybe Double)]
parameterDecls ds = [(n,v) | Parameter n v <- map metaValue ds ]

parameterMap :: Model -> Map.Map Text Double
parameterMap mdl = Map.fromList [(n, v) | (n, Just v) <- parameterDecls (modelDecls mdl)]

    
    
