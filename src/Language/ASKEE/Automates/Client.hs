{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.Automates.Client where

import qualified Data.Aeson as JSON
import Data.Aeson((.=))
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as Encoding
import Data.Set(Set)
import qualified Data.Set as Set

import qualified Language.ASKEE.Gromet.FunctionNetwork as FNet
import qualified Language.ASKEE.HTTPClient as Client
import qualified Language.ASKEE.DataSeries as DS
import qualified Language.ASKEE.Model.Basics as MB


automatesUrl :: String
automatesUrl = "https://hopper.sista.arizona.edu/api/v1"

apiKey :: String
apiKey = "kojzLVi21XrcHVJ4f7M82L0g"

apiHeaders :: Map String String
apiHeaders =
  Map.fromList [ ("apikey", apiKey) ]

simulateFnet :: SimulationRequest -> IO (DS.LabeledDataSeries Double)
simulateFnet = Client.postJSON' apiHeaders url
  where
    url = automatesUrl <> "/execute/gromet_experiment"

-- TODO: kinda crappy that this is more or less the same as the schema stuff
--       in donu, although there's no serializer for it
data SimulationRequest = SimulationRequest
  { simReqModel :: FNet.FunctionNetwork {-^ gromet json -}
  , simReqStart :: Double {- ^ start -}
  , simReqEnd :: Double {- ^ end -}
  , simReqStep :: Double {- ^ step -}
  , simReqDomainParam :: Text {- ^ domain parameter -}
  , simReqParams :: Map Text Double {- ^ parameterization -}
  , simReqOutput :: Set Text {- ^ variables to measure (empty to measure all) -}
  }

instance JSON.ToJSON SimulationRequest where
  toJSON SimulationRequest {..} =
    JSON.object
      [ "experiment" .=
        JSON.object
        [ "definition" .=
          JSON.object
            [ "type" .= MB.describeModelType MB.GrometFnetType
            , "source" .= JSON.String (Encoding.decodeUtf8 $ LBS.toStrict (JSON.encode simReqModel))
            ]
        , "start" .= simReqStart
        , "stop" .= simReqEnd
        , "step" .= simReqStep
        , "domain_parameter" .= simReqDomainParam
        , "parameters" .= simReqParams
        , "outputs" .= Set.toList simReqOutput
        ]
      ]