{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Language.ASKEE.CPP ( genModel, simulate ) where

import           Data.Aeson                 ( eitherDecode )
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List                  as List
import           Data.Map                   ( Map )
import qualified Data.Map                   as Map
import           Data.Text                  ( Text )

import Language.ASKEE.Core             ( Model )
import Language.ASKEE.CPP.Compile      ( compileAndRun, Compiler(..) )
import Language.ASKEE.CPP.SimulatorGen ( genModel, genDriver )
import Language.ASKEE.DataSeries       ( DataSeries(..) )
import Language.ASKEE.Panic            ( panic )

import Prettyprinter ( (<+>) )

simulate :: 
  Model -> 
  Double {- ^ start time -} ->
  Double {- ^ end time -} -> 
  Double {- ^ time step -} -> 
  IO (DataSeries Double)
simulate model start end step =
  do  let modelCPP = genModel model
          modelDriver = genDriver model start end step
          program = modelCPP <+> modelDriver
      res <- compileAndRun GCC [("model.cpp", program)]
      json <- 
        case eitherDecode @[Map Text Double] (B.pack res) of
          Left err -> panic "simulateCPP" ["Couldn't decode C++-produced JSON", err, res]
          Right json' -> pure json'
      let stateVars = List.delete "time" (Map.keys (head json))
      pure $ buildDataSeries stateVars json

buildDataSeries :: [Text] -> [Map Text Double] -> DataSeries Double
buildDataSeries stateVars json =
  let times = map (Map.! "time") json
      values = Map.fromList $ map (\sv -> (sv, map (Map.! sv) json)) stateVars
  in  DataSeries{..}