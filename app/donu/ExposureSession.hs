{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module ExposureSession ( exposureServer ) where

import           Control.Monad.State

import           Snap (MonadSnap)

import           Network.WebSockets      as Sockets
import           Network.WebSockets.Snap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JS
import           Data.Aeson ((.=), (.:))
import qualified Data.Text.Lazy.Encoding as Text
import qualified Control.Exception as X
import Data.Maybe (fromMaybe)
import qualified Language.ASKEE.Exposure.Interpreter as Exposure
import qualified Language.ASKEE.Exposure.Syntax as Exposure
import qualified Data.Text as Text
import qualified Language.ASKEE.Exposure.GenLexer as Exposure
import qualified Language.ASKEE.Exposure.GenParser as Exposure
import Schema
import Data.String (fromString)

-- | Messages sent by the server
data ExposureServerMessage =
    ReadFile FilePath -- ^ Instruct the client to retrieve a file's contents
  | WriteFile FilePath LBS.ByteString -- ^ Ask the client to write to a file
  | Success [DonuValue] -- ^ Successfully evaluated program
  | Failure Text.Text -- ^ Some error occurred

-- | Messages sent by client
data ExposureClientMessage =
    RunProgram [Exposure.Stmt] -- ^ Ask the server to run some code
  | FileContents LBS.ByteString -- ^ Provide file contents
  | Error LBS.ByteString -- ^ This is actually an error deciphering a client message
  deriving Show

-- | The main entry point into a websocket-based Exposure session
exposureServer :: MonadSnap m => m ()
exposureServer = runWebSocketsSnap (acceptRequest >=> exposureServerLoop)

-- | The main server loop, the toplevel of which waits for programs to try and
-- run in the exposure interpreter.
exposureServerLoop :: Connection -> IO ()
exposureServerLoop c = go Exposure.initialEnv
  where
    go env =
      do msg <- X.try (receiveData c)
         case msg of
           Right msg' -> onReceive msg' env
           Left  ex   -> onExcept ex

    onReceive msg env =
      case msg of
        RunProgram prog ->
          do (res, env') <- liftIO $ eval env prog
             case res of
               Left err ->
                 do sendTextData c (Failure err)
               Right (displays, _) ->
                 do let display = fmap (DonuValue . Exposure.unDisplayValue) displays
                    sendTextData c (Success display)
             go env'
        _ ->
          -- This is the toplevel, so we don't expect any filecontents, for
          -- example
          sendTextData c (Failure "Unexpected message")

    eval = Exposure.evalLoop evr
    evr  = Exposure.mkEvalReadEnv (readClientFile c) (writeClientFile c)

    onExcept :: ConnectionException -> IO ()
    onExcept _ = return ()

-- | The implementation of @getFileFn@ for Exposure. This implementation sends a
-- message to the client to fetch a file.
readClientFile :: Connection -> FilePath -> IO LBS.ByteString
readClientFile c f =
  do sendTextData c (ReadFile f)
     msg <- receiveData c
     case msg of
       FileContents contents -> pure contents
       _ -> pure "" -- TODO: Should this throw an error?

-- | The implementation of @getFileFn@ for Exposure. This implementation sends a
-- message to the client to fetch a file.
writeClientFile :: Connection -> FilePath -> LBS.ByteString -> IO ()
writeClientFile c f bs = sendTextData c (WriteFile f bs)

-- Instances for serializing/deserializing protocol messages ---------

instance JS.ToJSON ExposureServerMessage where
  toJSON (ReadFile fp) =
    JS.object [ "type" .= ("read-file" :: String)
              , "path" .= JS.toJSON fp
              ]

  toJSON (WriteFile fp contents) =
    JS.object [ "type"     .= ("write-file" :: String)
              , "path"     .= JS.toJSON fp
              , "contents" .= JS.toJSON (Text.decodeUtf8 contents)
              ]

  toJSON (Success displays) =
    JS.object [ "type"     .= ("success" :: String)
              , "displays" .= JS.toJSON displays
              ]

  toJSON (Failure err) =
    JS.object [ "type"     .= ("failure" :: String)
              , "message"  .= JS.toJSON err
              ]

instance WebSocketsData ExposureServerMessage where
  fromDataMessage _ = error "We don't receive server messages!"
  fromLazyByteString _ = error "We don't receive server messages!"
  toLazyByteString msg = JS.encode msg

instance JS.FromJSON ExposureClientMessage where
  parseJSON js =
    case js of
      JS.Object o ->
        do ty <- o .: "type"
           case ty :: String of

             "run-program" ->
               do code <- o .: "code"
                  case Exposure.lexExposure (Text.unpack code) >>= Exposure.parseExposureStmts of
                    Left err -> pure $ Error (fromString err)
                    Right stmts -> pure $ RunProgram stmts

             "file-contents" ->
               do contents <- o.:"contents"
                  let bs = Text.encodeUtf8 contents
                  pure $ FileContents bs

             _ -> pure $ Error "Unrecognized command"

      _ ->
        pure (Error "Expected object")

instance WebSocketsData ExposureClientMessage where
  fromDataMessage (Text bs _) =
    fromMaybe (Error bs) (JS.decode bs)
  fromDataMessage (Binary bindata) =
    fromMaybe (Error bindata) (JS.decode bindata)

  fromLazyByteString bs = fromMaybe (Error bs) (JS.decode bs)
  toLazyByteString _msg = error "We don't send client messages"
