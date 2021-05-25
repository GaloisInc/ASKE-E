{-# LANGUAGE DeriveFunctor #-}
module Language.ASKEE.Metadata where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map

data MetaAnn a = MetaAnn
  { metaData :: [(Text,Text)]
  , metaValue :: a
  }
  deriving(Eq, Ord, Show, Functor)

instance Applicative MetaAnn where
  pure a = MetaAnn [] a
  MetaAnn md1 f <*> MetaAnn md2 a  = MetaAnn (md1 ++ md2) (f a)

metaMap :: MetaAnn a -> Map Text Text
metaMap = Map.fromList . metaData

withMeta :: Text -> Text -> MetaAnn a -> MetaAnn a
withMeta k v = (MetaAnn [(k,v)] id <*>)

fromValue :: a -> MetaAnn a
fromValue = pure
