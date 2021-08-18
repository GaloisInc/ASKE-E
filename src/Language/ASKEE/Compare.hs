{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.Compare where

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  ( find )
import           Data.Map                   ( Map )
import qualified Data.Map                   as Map
import           Data.Text                  ( Text )
import qualified Data.Text                  as Text

import           Language.ASKEE.Error ( die, ASKEEError(..) )
import qualified Language.ASKEE.Storage as Storage

type UID = Text

data Comparison = Comparison
  { compareSource :: UID
  , compareTargets :: Map UID [VariableMapping] }
  deriving (Show)

type VariableMapping = Map Text Text

instance JSON.FromJSON Comparison where
  parseJSON = JSON.withObject "Comparison" \c -> Comparison
    <$> c JSON..: "apex"
    <*> c JSON..: "legs"

loadComparisonBySourceUID :: UID -> IO Comparison
loadComparisonBySourceUID source =
  do  comps <- mapM (JSON.eitherDecode . BS.pack . Text.unpack) <$> Storage.listComparisons
      comps' <- case comps of
        Left err -> die (ComparisonError $ "Failed to parse a comparison file: "<>err)
        Right cs -> pure cs
      case find (\Comparison{..} -> compareSource == source) comps' of
        Nothing -> die (ComparisonError $ "No comparison found for source UID "<>Text.unpack source)
        Just c -> pure c

compareModels :: UID -> UID -> IO [VariableMapping]
compareModels source target =
  do  Comparison{..} <- loadComparisonBySourceUID source
      case compareTargets Map.!? target of
        Just mapping -> pure mapping
        Nothing -> die (ComparisonError $ "Cannot compare "<>Text.unpack source<>" with "<>Text.unpack target)