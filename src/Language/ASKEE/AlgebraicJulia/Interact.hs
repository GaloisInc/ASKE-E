module Language.ASKEE.AlgebraicJulia.Interact where

import Data.Aeson                 ( encode, Value, FromJSON, decode )
import Data.ByteString.Lazy.Char8 ( unpack, pack )

import System.Process ( readProcess )

-- | Send the provided Aeson `Value` to a local instance of
-- AlgebraicJulia running as a webservice on 8001
queryServer :: Value -> IO String
queryServer = queryServer' . unpack . encode

-- | Send the provided string-encoded JSON value to a local instance
-- of AlgebraicJulia running as a webservice on 8001
queryServer' :: String -> IO String
queryServer' payload = 
  readProcess
    "curl"
    [ "-X", "POST"
    , "-H", "Content-type: application/json"
    , "-d", payload
    , "localhost:8001" ]
    ""

queryServerForValue :: FromJSON a => Value -> IO (Maybe a)
queryServerForValue = fmap (decode . pack) . queryServer

queryServerForValue' :: FromJSON a => String -> IO (Maybe a)
queryServerForValue' = fmap (decode . pack) . queryServer'