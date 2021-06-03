{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.Error where

import Control.Exception ( throwIO, Exception )

data ASKEEError =
    ParseError      String
  | ValidationError String
  | ConversionError String
  deriving (Show)

instance Exception ASKEEError

-- tryParse :: String -> Maybe String -> Either String a -> IO a
-- tryParse = tryWithArtifact ParseError

-- tryValidate :: String -> Maybe String -> Either String a -> IO a
-- tryValidate = tryWithArtifact ValidationError

-- tryWithArtifact :: (String -> String -> ASKEEError) -> String -> Maybe String -> Either String a -> IO a
-- tryWithArtifact mkErr context artifactM action =
--   case (action, artifactM) of
--     (Left err, Nothing) -> throwIO (mkErr context err)
--     (Left _, Just artifact) -> throwIO (mkErr context artifact)
--     (Right a, _) -> pure a

throwLeft :: (String -> ASKEEError) -> Either String a -> IO a
throwLeft mkErr action =
  case action of
    Left err -> throwIO (mkErr err)
    Right a -> pure a



-- data ASKEEError where
--   ParseError      :: Show a => { context :: Maybe String, artifact :: Maybe a } -> ASKEEError
--   ValidationError :: Show a => { context :: Maybe String, artifact :: Maybe a } -> ASKEEError

-- instance Show ASKEEError where
--   show e =
--     case e of
--       ParseError{..} -> showCtx context <> "parse error: " <> showArt artifact
--       ValidationError{..} -> showCtx context <> "validation error: " <> showArt artifact

-- showCtx :: Maybe String -> String
-- showCtx ctxM =
--   case ctxM of
--     Nothing -> "<no context>: "
--     Just ctx -> ctx<>": "

-- showArt :: (Show a) => Maybe a -> String
-- showArt ctxM =
--   case ctxM of
--     Nothing -> "<no artifact>: "
--     Just ctx -> show ctx<>": "

-- instance Exception ASKEEError

-- die :: ASKEEError -> IO a
-- die = throwIO

-- asIO ::
--   (Show b) =>
--   ASKEEError ->
--   Maybe String ->
--   Maybe b ->
--   Either String c ->
--   IO c
-- asIO err ctxM artM action =
--   case (action, artM) of
--     (Left _, Just art) -> die $ err { context = ctxM, artifact = (Just art) }
--     (Left e, _) -> die $ mkErr ctxM (Just e)
--     (Right a, _) -> pure a

-- parseIO :: (Show a) => Maybe String -> Maybe a -> Either String b -> IO b
-- parseIO = asIO ParseError

-- validateIO :: (Show a) => Maybe String -> Maybe a -> Either String b -> IO b
-- validateIO = asIO ParseError
