{-# LANGUAGE TemplateHaskell #-}
module Language.ASKEE.Simulate where

import qualified Data.Map  as Map
import           Data.Map  ( Map )
import           Data.Text ( Text )

import           Language.ASKEE.DataSeries   ( DataSeries(..) )
import           Language.ASKEE.DEQ          ( loadDiffEqsFrom )
import qualified Language.ASKEE.DEQ.Simulate as Sim
import           Language.ASKEE.ModelType    ( ModelType )
import           Language.ASKEE.Storage      ( DataSource )

simulateModel :: 
  ModelType -> 
  DataSource -> 
  Double -> 
  Double ->
  Double ->
  Map Text Double ->
  IO (DataSeries Double)
simulateModel format source start end step overwrite =
  do  equations <- loadDiffEqsFrom format overwrite [] source
      let times' = takeWhile (<= end)
                 $ iterate (+ step) start
      pure $ Sim.simulate equations Map.empty times'