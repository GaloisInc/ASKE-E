{-# Language BlockArguments, OverloadedStrings #-}
module Main(main) where

import qualified Data.Aeson as JS
import Snap.Core(Snap)
import qualified Snap.Core as Snap
import Snap.Http.Server (quickHttpServe)

import qualified Data.ByteString.Char8 as BS8

import Schema

main :: IO ()
main = quickHttpServe
  $ Snap.method Snap.GET
    do let limit = 8 * 1024 * 1024    -- 8 megs
       body <- Snap.readRequestBody limit
       case JS.decode' body of
         Just a ->
           do Snap.modifyResponse (Snap.setResponseStatus 200 "OK")
              Snap.writeLBS (JS.encode (handleRequest a))
         Nothing ->
           do Snap.modifyResponse (Snap.setResponseStatus 400 "Bad request")
              showHelp

data Input = XXXin
  deriving Show

data Output = XXXout
  deriving Show

instance JS.FromJSON Input where
  parseJSON = JS.withObject "request" \_o -> pure XXXin


instance JS.ToJSON Output where
  toJSON _ = JS.object []


showHelp :: Snap ()
showHelp = Snap.writeBS (BS8.pack $ generateDocsJS demoSpec)

--------------------------------------------------------------------------------

handleRequest :: Input -> Output
handleRequest r = XXXout


