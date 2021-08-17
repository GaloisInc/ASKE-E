{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.Automates.Client where

import qualified Data.Aeson as JSON
import Data.Aeson((.=))
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.HashMap.Lazy as HMap
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Exception(throwIO)

import Language.ASKEE.Error(throwLeft, ASKEEError(..))
import qualified Language.ASKEE.Gromet.FunctionNetwork as FNet
import qualified Language.ASKEE.HTTPClient as Client
import qualified Language.ASKEE.DataSeries as DS
import qualified Language.ASKEE.Model.Basics as MB


automatesUrl :: String
automatesUrl = "http://hopper.sista.arizona.edu/api/v1"

apiKey :: String
apiKey = "kojzLVi21XrcHVJ4f7M82L0g"

apiHeaders :: Map String String
apiHeaders =
  Map.fromList [ ("apikey", apiKey) ]

simulateFnet :: SimulationRequest -> IO (DS.LabeledDataSeries Double)
simulateFnet req =
  do  response <- Client.postJSON' apiHeaders url req
      result <- throwLeft HttpCallException (parseResponse response)
      case JSON.fromJSON result of
        JSON.Error s -> throwIO (HttpCallException s)
        JSON.Success v -> pure v

  where
    url = automatesUrl <> "/execute/gromet_experiment"

-- TODO: there's probably a FromJSON instance lurking in here
parseResponse :: JSON.Value -> Either String JSON.Value
parseResponse val =
  do  root <- obj "root" val
      status <- objKey root "status"
      statusText <- text status

      case statusText of
        "success" -> snd <$> objKey root "result"
        _ -> err (errMsg root)

  where
    err = Left . Text.unpack

    errMsg :: (Text, JSON.Object) -> Text
    errMsg o =
      case objKey o "error" >>= text of
        Right e -> e
        Left _ -> "unspecified error"

    text (n, o) =
      case o of
        JSON.String t -> Right t
        _ -> err ("JSON parse error: expecting text at " <> n)

    obj name a =
      case a of
        JSON.Object o -> Right (name, o)
        _ -> err ("JSON parse error: expecting object at " <> name)

    objKey (n, o) key =
      case HMap.lookup key o of
        Just a -> Right (n <> "." <> key, a)
        Nothing -> err ("JSON parse error: expecting " <> n <> " to have key " <> key)

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
      [ "command" .= ("simulate" :: Text)
      , "definition" .=
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
