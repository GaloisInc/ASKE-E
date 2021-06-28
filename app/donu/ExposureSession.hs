{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module ExposureSession where

import Snap.Snaplet.Session

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Serialize as S

import Web.ClientSession
import Data.ByteString
import Data.Time.Clock (getCurrentTime)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)

import Snap (Snap, SnapletInit, makeSnaplet, Handler, liftSnap)

data ExposureSessionManager st = ExposureSessionManager
  { stateMap        :: MVar (Map Text st)
  , timeoutMap      :: MVar (Map Text UTCTime)
  , key             :: Key
  , cookieName      :: ByteString
  , timeout         :: Int
  , session         :: Maybe ExposureSession
  , randomNumGen    :: RNG
  }

newtype ExposureSession = ExposureSession
  { esToken :: Text }
  deriving (Eq, Show)

newtype Payload = Payload ByteString
  deriving (Eq, Show, Ord, S.Serialize)

instance S.Serialize ExposureSession where
  put (ExposureSession t) = S.put (encodeUtf8 t)
  get = ExposureSession . decodeUtf8 <$> S.get

initExposureSessionManager :: FilePath -> ByteString -> Int -> SnapletInit b (ExposureSessionManager s)
initExposureSessionManager keyPath c t =
  makeSnaplet
    "ExposureSession"
    "A snaplet providing sessions for the Exposure REPL"
    Nothing $
    liftIO $
      do k <- getKey keyPath
         rng <- liftIO mkRNG
         sm <- newMVar mempty
         tm <- newMVar mempty
         return $! ExposureSessionManager sm tm k c t Nothing rng

cullOldSessions :: Handler b (ExposureSessionManager s) ()
cullOldSessions =
  do esm <- get
     to  <- liftIO $ readMVar (timeoutMap esm)
     -- If someone adds more then we'll miss them here,
     -- but that's ok
     forM_ (Map.toList to) $ \(token, time)  ->
       do timedout <- checkTimeout (Just (timeout esm)) time
          when timedout
            (liftSnap $ deleteSession (ExposureSession token) esm)
     put esm { session = Nothing }

getExposureSessionState :: Handler b (ExposureSessionManager s) (Maybe s)
getExposureSessionState =
  do esm <- get
     esm' <- liftSnap $ loadSession esm
     put esm'
     case session esm' of
       Just s ->
         do sm <- liftIO $ readMVar (stateMap esm')
            return $ Map.lookup (esToken s) sm
       Nothing ->
         return Nothing

putExposureSessionState :: st -> Handler b (ExposureSessionManager st) ()
putExposureSessionState st =
  do esm <- get
     esm' <- liftSnap $ loadSession esm
     put esm'
     case session esm' of
       Just s ->
         liftIO $ modifyMVar_ (stateMap esm') (pure . Map.insert (esToken s) st)
       Nothing ->
         return ()

mkExposureSession :: RNG -> IO ExposureSession
mkExposureSession rng =
  ExposureSession <$> liftIO (mkCSRFToken rng)

pokeSession :: ExposureSessionManager st -> Snap (ExposureSessionManager st)
pokeSession esm =
  case session esm of
    Just s ->
      do now <- liftIO getCurrentTime
         liftIO $ modifyMVar_ (timeoutMap esm) (pure . Map.insert (esToken s) now)
         return esm
    Nothing ->
      return esm

deleteSession :: ExposureSession -> ExposureSessionManager st -> Snap ()
deleteSession s esm = liftIO $
  do modifyMVar_ (timeoutMap esm) (pure . Map.delete (esToken s))
     modifyMVar_ (stateMap esm) (pure . Map.delete (esToken s))
     return ()

loadSession :: ExposureSessionManager st -> Snap (ExposureSessionManager st)
loadSession esm =
  case session esm of
    Just _  -> return esm
    Nothing ->
      do tk   <- getSecureCookie (cookieName esm) (key esm) (Just $ timeout esm)
         case tk of
           -- We have a thing
           Just (Payload (S.decode -> Right s)) ->
             pokeSession esm { session = Just s }
           _ ->
             -- New session
             do sess <- liftIO $ mkExposureSession (randomNumGen esm)
                setSecureCookie (cookieName esm) Nothing (key esm) (Just $ timeout esm) (Payload (S.encode sess))
                pokeSession esm { session = Just sess }

commitExposureSession :: ExposureSessionManager st -> Snap ()
commitExposureSession esm =
  do pl <- case session esm of
             Just s  ->
               do void $ pokeSession esm
                  return $ Payload (S.encode s)
             Nothing ->
               do sess <- liftIO $ mkExposureSession (randomNumGen esm)
                  void $ pokeSession esm { session = Just sess }
                  return $ Payload (S.encode sess)
     setSecureCookie (cookieName esm) Nothing (key esm) (Just $ timeout esm) pl
