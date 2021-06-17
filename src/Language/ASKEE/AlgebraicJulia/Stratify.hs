{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Language.ASKEE.AlgebraicJulia.Stratify where

import           Data.Aeson ( encode
                            , decode
                            , object
                            -- , Value(..)
                            , FromJSON
                            , ToJSON
                            , (.=) )
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Map   ( Map )
import           Data.Text  ( Text )

import GHC.Generics ( Generic )

import qualified Language.ASKEE.ESL.Syntax as ESL
import           Language.ASKEE.AlgebraicJulia.Topology ( modelAsTopology
                                                       , topologyAsModel
                                                       , insertHoles
                                                       , nameHoles )

import System.Process ( readProcess )
import qualified Language.ASKEE.AlgebraicJulia.Syntax as Topology
import Language.ASKEE.AlgebraicJulia.GeoGraph

data States = States
  { sus :: Text
  , exp :: Text
  , inf :: [Text]
  }
  deriving (Generic, Show)

instance FromJSON States
instance ToJSON States

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
  
stratifyModel :: 
  ESL.Model -> 
  ConnGraph ->
  Map Int Text -> 
  Maybe States -> 
  StratificationType ->
  IO StratificationInfo
stratifyModel model connections vertices states strat =
  do  let topology = modelAsTopology model
      -- (gtriConnections, vertices) <- loadConnectionGraph connections
      -- states <- case statesM of 
      --             Just d -> decode @States . B.pack <$> loadString d
      --             Nothing -> pure Nothing
      let payload = 
            object $  
              [ "top" .= topology 
              , "conn" .= connections
              , "type" .= case strat of { Demographic -> "dem" ; Spatial -> "spat" :: String }
              ] ++ 
              maybe [] (\s -> [ "states" .= s ]) states
      result <- readProcess "curl"  [ "-X", "POST"
                                    , "-H", "Content-type: application/json"
                                    , "-d", B.unpack $ encode payload
                                    , "localhost:8001"
                                    ] ""
      rawTopology <- case decode (B.pack result) of
        Just t -> pure t
        Nothing -> error $ "failed to parse JSON of returned topology "++result
      let (rawModel, holes) = insertHoles $ topologyAsModel rawTopology
          prettyModel = nameHoles vertices rawModel
      pure $ StratificationInfo{..}