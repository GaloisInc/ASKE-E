{-# LANGUAGE TemplateHaskell #-}
module Language.ASKEE.Simulate where

import qualified Data.Map  as Map
import           Data.Map  ( Map )
import           Data.Text ( Text )

import qualified Language.ASKEE.Core.GSLODE as ODE
import           Language.ASKEE.DataSeries ( DataSeries(..) )
import           Language.ASKEE.Types
import           Language.ASKEE.Storage
import qualified Language.ASKEE.Convert as Convert
import Control.Exception (throwIO, Exception)
import Language.ASKEE.Panic (panic)

newtype SimulationError = SimulationError String
  deriving (Show)

instance Exception SimulationError

simulateModel :: 
  ModelType -> 
  DataSource -> 
  Double -> 
  Double ->
  Double ->
  Map Text Double ->
  IO (DataSeries Double)
simulateModel modelType modelSource start end step overwrite =
  do  model <- loadModel modelType modelSource
      conv <- 
        case modelType of
          ESL Concrete   -> pure $(Convert.converter (ESL Concrete) (DEQ Abstract))
          DEQ Concrete   -> pure $(Convert.converter (DEQ Concrete) (DEQ Abstract))
          RNET Concrete  -> pure $(Convert.converter (RNET Concrete) (DEQ Abstract))
          LATEX Concrete -> pure $(Convert.converter (LATEX Concrete) (DEQ Abstract))
          _ -> throwIO $ SimulationError $ "unsupported model type to simulate" ++ show modelType
      let times' = takeWhile (<= end)
                 $ iterate (+ step) start
      eqs <- case conv model of
        Left err -> throwIO (SimulationError err)
        Right eqs' -> pure eqs'
      pure $ ODE.simulate eqs Map.empty times'