{-# LANGUAGE TemplateHaskell #-}
module Language.ASKEE.Simulate where

import qualified Data.Map  as Map
import           Data.Map  ( Map )
import           Data.Text ( Text )

import qualified Language.ASKEE.DEQ.Simulate as Sim
import           Language.ASKEE.DataSeries ( DataSeries(..) )
-- import           Language.ASKEE.Types ( DataSource )
import           Language.ASKEE.Storage ( loadModel, DataSource )
import Language.ASKEE.ModelType
import Language.ASKEE.Model ( parseModel, toDeqs )
import Language.ASKEE.Error ( ASKEEError(..), throwLeft )

simulateModel :: 
  ModelType -> 
  DataSource -> 
  Double -> 
  Double ->
  Double ->
  Map Text Double ->
  IO (DataSeries Double)
simulateModel modelType modelSource start end step overwrite =
  do  modelString <- loadModel modelType modelSource
      model <- throwLeft ParseError (parseModel DeqType modelString)
      equations <- throwLeft ConversionError (toDeqs model)
      let times' = takeWhile (<= end)
                 $ iterate (+ step) start
      pure $ Sim.simulate equations Map.empty times'