{-# LANGUAGE BlockArguments #-}
module Language.ASKEE.AlgebraicJulia.Interact ( queryServer ) where

import Data.Aeson                 ( encode, Value )
import Data.ByteString.Lazy.Char8 ( unpack )
import qualified Data.Map as Map

import Language.ASKEE.Error ( die, ASKEEError(..) )
import Language.ASKEE.HTTPClient

import System.Exit        ( ExitCode(..) )
import System.FilePath    ( (</>) )
import System.IO.Temp     ( withSystemTempDirectory )
import System.Process     ( readProcessWithExitCode )

-- | Send the provided Aeson `Value` to a local instance of
-- AlgebraicJulia running as a webservice on 8001
queryServer :: Value -> IO String
queryServer val = 
  do  res <- postJSON' headers url val
      pure (unpack (encode (res :: Value)))
  where
    headers = Map.fromList
      [ ("Content-Type", "application/json")
      , ("Connection", "Keep-Alive")
      , ("Keep-Alive", "timeout=120")
      ]
    url = "http://localhost:8001"

-- | Send the provided string-encoded JSON value to a local instance
-- of AlgebraicJulia running as a webservice on 8001
queryServer' :: String -> IO String
queryServer' payload = 
  do  (code, stdout, stderr) <- withSystemTempDirectory "AJ-SIM" \dir ->
        do  let file = dir </> "payload.json"
            -- XXX if models ever start measuring in gigabytes we may
            -- want to reconsider writing to file
            writeFile file payload
            readProcessWithExitCode
              "curl"
              [ "-X", "POST"
              , "-H", "Content-type: application/json"
              , "-d", "@"<>file
              , hostname<>":8001" ]
              ""
      case code of
        ExitSuccess -> pure stdout
        ExitFailure n -> die (AlgebraicJuliaError $ unlines ["Failed to interact with AlgebraicJulia", stdout, stderr, "Exit code "<>show n])

  where
    hostname = "localhost"