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
import Language.ASKEE.CPP.Compile      ( compileAndRunN, Compiler(..) )
import Language.ASKEE.CPP.SimulatorGen ( genModel, genDriver )
import Language.ASKEE.DataSeries       ( DataSeries(..), emptyDataSeries )
import Language.ASKEE.Panic            ( panic )

import Prettyprinter ( (<+>) )
import System.Random (randomIO)

simulate ::
  Model ->
  Double {- ^ start time -} ->
  Double {- ^ end time -} ->
  Double {- ^ time step -} ->
  Maybe Int {- ^ seed -} ->
  Int {- ^ Number of iterations -} ->
  IO [DataSeries Double]
simulate model start end step seed iters =
  do  seed' <- maybe randomIO pure seed
      let modelCPP = genModel model
          modelDriver = genDriver model start end step
          program = modelCPP <+> modelDriver
          args = Right [[show i] | i <- take iters [seed' ..]]
      results <- compileAndRunN GCC [("model.cpp", program)] args iters
      mapM asDataSeries results

  where
    asDataSeries ds =
      do  points <-
            case eitherDecode @[Map Text Double] (B.pack ds) of
              Left err -> panic "simulate" ["Couldn't decode C++-produced JSON", err, ds]
              Right points' -> pure points'
          case points of
            [] -> pure $ emptyDataSeries []
            (point:_) ->
              let stateVars = List.delete "time" (Map.keys point)
              in  pure $ buildDataSeries stateVars points

buildDataSeries :: [Text] -> [Map Text Double] -> DataSeries Double
buildDataSeries stateVars json =
  let times = map (Map.! "time") json
      values = Map.fromList $ map (\sv -> (sv, map (Map.! sv) json)) stateVars
  in  DataSeries{..}
