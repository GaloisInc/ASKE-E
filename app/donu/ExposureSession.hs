{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ExposureSession ( exposureServer ) where

import           Control.Monad.State

import           Snap (MonadSnap)

import           Network.WebSockets      as Sockets
import           Network.WebSockets.Snap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JS
import           Data.Aeson ((.=), (.:))
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text.Lazy.Encoding as TL
import qualified Control.Exception as X
import Data.Maybe (fromMaybe)
import qualified Language.ASKEE.Exposure.Interpreter as Exposure
import qualified Language.ASKEE.Exposure.Syntax as Exposure
import qualified Data.Text as Text
import qualified Language.ASKEE.Exposure.GenLexer as Exposure
import qualified Language.ASKEE.Exposure.GenParser as Exposure
import Schema
import Data.String (fromString)
import Language.ASKEE.Exposure.Syntax

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
           Right msg' ->
             X.handle (exHandler env) (onReceive msg' env) >>= go
           Left  ex   ->
             onExcept ex

    exHandler :: Exposure.Env -> X.SomeException -> IO Exposure.Env
    exHandler env se =
      do X.handle onExcept (sendTextData c (Failure msg))
         return env
      where
        msg =
          "Caught exception: " <> Text.pack (X.displayException se)

    onReceive msg env =
      case msg of
        RunProgram prog ->
          do (res, env') <- eval env prog
             case res of
               Left err ->
                 sendTextData c (Failure err)
               Right (displays, _) ->
                 do let force = evalValue . Exposure.unDisplayValue
                    -- We want to force any exceptions (such as bugs in the interpreter)
                    -- before sending the response, otherwise the connection will be closed
                    -- and we don't want that, now do we?
                    displays' <- fmap DonuValue <$> traverse force displays
                    sendTextData c (Success displays')
             return env'
        FileContents{} ->
          -- This is the toplevel, so we don't expect any filecontents
          do sendTextData c (Failure "Unexpected FileContents message")
             return env
        Error err ->
          do sendTextData c $ Failure $ TL.toStrict $ TL.decodeUtf8 err
             return env

    eval = Exposure.evalLoop evr
    evr  = Exposure.mkEvalReadEnv (readClientFile c) (writeClientFile c)

    onExcept :: ConnectionException -> IO ()
    onExcept _ce = return ()

-- A poor substitute for deepseq
evalValue :: Value -> IO Value
evalValue v =
  case v of
    VInt i    -> VInt <$> X.evaluate i
    VBool b   -> VBool <$> X.evaluate b
    VString t -> VString <$> X.evaluate t
    VDouble d -> VDouble <$> X.evaluate d
    VArray vs -> VArray <$> traverse evalValue vs
    VPoint pt -> VPoint <$> traverse evalValue pt
    VHistogram x y z m ->
      VHistogram <$> X.evaluate x <*> X.evaluate y <*> X.evaluate z <*> traverse X.evaluate m
    VPlot t ls xs ys ->
      VPlot <$> X.evaluate t <*> traverse X.evaluate ls <*> traverse X.evaluate xs <*> traverse (traverse X.evaluate) ys
    _ -> X.evaluate v

-- | The implementation of @getFileFn@ for Exposure. This implementation sends a
-- message to the client to fetch a file.
readClientFile :: Connection -> FilePath -> IO LBS.ByteString
readClientFile c f =
  do sendTextData c (ReadFile f)
     msg <- receiveData c
     case msg of
       FileContents contents ->
         pure contents
       _ ->
         error $ "readClientFile: expecting FileContents but received "
              ++ show msg

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
              , "contents" .= JS.toJSON (TL.decodeUtf8 contents)
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
                  let bs = TL.encodeUtf8 contents
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
