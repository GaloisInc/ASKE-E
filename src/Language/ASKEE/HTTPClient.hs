{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.HTTPClient(postJSON, postJSON') where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Aeson as JSON
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Control.Exception as Exception
import qualified Language.ASKEE.Error as AE
import qualified Data.CaseInsensitive as CI

requestJSONBody :: (JSON.ToJSON from, JSON.FromJSON to) => String -> Map String String -> String -> from -> IO to
requestJSONBody method headers url val =
  do  manager <- HTTP.newManager TLS.tlsManagerSettings
      reqBase <- HTTP.parseUrlThrow url
      let req = reqBase { HTTP.method = UTF8.fromString method
                        , HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode val
                        , HTTP.requestHeaders = headerLbs
                        }

      response <- HTTP.httpLbs req manager

      case JSON.eitherDecode (HTTP.responseBody response) of
        Left err -> Exception.throw $ AE.HttpCallException ("HTTP call failed " <> describeCall <> " - could not parse response: " <> err )
        Right a -> pure a
  where
    mkHeader (hkey, hval) = (CI.mk $ UTF8.fromString hkey, UTF8.fromString hval)
    headerLbs =  mkHeader <$> Map.toList headers
    describeCall = "[" ++ method ++ "] " ++ url


postJSON' :: (JSON.FromJSON to, JSON.ToJSON from) => Map String String -> String -> from -> IO to
postJSON' = requestJSONBody "POST"

postJSON :: (JSON.FromJSON to, JSON.ToJSON from) => String -> from -> IO to
postJSON = postJSON' Map.empty


