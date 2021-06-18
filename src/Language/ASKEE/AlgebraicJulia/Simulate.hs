{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.AlgebraicJulia.Simulate ( simulate ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Map                   ( Map )
import           Data.Text                  ( Text )

import Language.ASKEE.AlgebraicJulia.Interact ( queryServer )
import Language.ASKEE.Gromet                  ( PetriNetClassic )
import Language.ASKEE.DataSeries              ( DataSeries(..) )

data Parameterization =
  Parameterization 
    { parameters :: Map Text Double
    , start :: Double
    , stop :: Double
    , step :: Double
    }

instance ToJSON Parameterization where
  toJSON Parameterization{..} =
    object
      [ "parameters" .= parameters
      , "start" .= start
      , "stop" .= stop
      , "step" .= step
      ]

data SimulationResult =
  SimulationResult
    { values :: Map Text [Double]
    , times :: [Double]}

instance FromJSON SimulationResult where
  parseJSON = withObject "SimulationResult" \o ->
    do  times <- o .: "times"
        values <- o .: "values"
        pure SimulationResult{..}

simulate :: 
  PetriNetClassic -> 
  Double {- ^ start -} ->
  Double {- ^ stop -} -> 
  Double {- ^ step -} -> 
  Map Text Double -> 
  IO (DataSeries Double)
simulate model start stop step parameters =
  do  let payload = 
            object
              [ "model" .= model
              , "sim" .= Parameterization parameters start stop step ]
      result <- queryServer payload
      case decode $ B.pack result of
        Nothing -> error ""
        Just SimulationResult{..} -> pure DataSeries{..}
