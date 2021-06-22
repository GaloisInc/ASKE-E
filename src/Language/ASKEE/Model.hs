{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Language.ASKEE.Model
  ( Model(..)
  , toDeqs
  , toCore
  , toEasel
  , toGrometPrt
  , toGrometPnc
  , toGrometFnet
  , parseModel
  , printModel
  ) where

import Data.Text(Text)
import qualified Data.Text.Encoding as Text
import Control.Monad ( (>=>) )
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Language.ASKEE.Core as Core
import qualified Language.ASKEE.DEQ as DEQ
import qualified Language.ASKEE.ESL as ESL

import qualified Language.ASKEE.Model.Basics as MT
import qualified Language.ASKEE.Gromet as GPRT
import qualified Language.ASKEE.Gromet.PetriNetClassic as GPNC

data Model =
    Easel     ESL.Model
  | Core      Core.Model
  | Deq       DEQ.DiffEqs
  | GrometPrt GPRT.Gromet
  | GrometPnc GPNC.PetriNetClassic
  | GrometFnet JSON.Value

modelTypeOf :: Model -> MT.ModelType
modelTypeOf m =
  case m of
    Easel _ -> MT.EaselType
    Core _ -> MT.CoreType
    Deq _ -> MT.DeqType
    GrometPrt _ -> MT.GrometPrtType
    GrometPnc _ -> MT.GrometPncType
    GrometFnet _ -> MT.GrometFnetType

-------------------------------------------------------------------------------

asEasel :: Model -> ConversionResult ESL.Model
asEasel = tryConvs [ unEasel, notExist MT.EaselType ]

asCore :: Model -> ConversionResult Core.Model
asCore = tryConvs [ unCore, asEasel >=> easelToCore, notExist MT.CoreType ]
  where
    easelToCore e = fromEither (ESL.modelAsCore e)

asDeq :: Model -> ConversionResult DEQ.DiffEqs
asDeq = tryConvs [ unDeq, asCore >=> coreToDeqs, notExist MT.DeqType ]
  where
    coreToDeqs c = pure $ Core.asDiffEqs c

asGrometPrt :: Model -> ConversionResult GPRT.Gromet
asGrometPrt = tryConvs [unGrometPrt, asCore >=> fromCore, notExist MT.GrometPncType ]
  where
    fromCore = pure . GPRT.convertCoreToGromet

notExist :: MT.ModelType -> Model -> ConversionResult a
notExist tgt mdl =
  ConversionFailed ("could not convert model '" ++ MT.describeModelType' (modelTypeOf mdl) ++ "' to '" ++ MT.describeModelType' tgt ++ "'")

unEasel :: Model -> ConversionResult ESL.Model
unEasel (Easel e) = ConversionSucceded e
unEasel _ = ConversionPass

unCore :: Model -> ConversionResult Core.Model
unCore (Core c) = ConversionSucceded c
unCore _ = ConversionPass

unDeq :: Model -> ConversionResult DEQ.DiffEqs
unDeq (Deq d) = ConversionSucceded d
unDeq _ = ConversionPass

unGrometPrt :: Model -> ConversionResult GPRT.Gromet
unGrometPrt (GrometPrt g) = ConversionSucceded g
unGrometPrt _ = ConversionPass

unGrometPnc :: Model -> ConversionResult JSON.Value
unGrometPnc (GrometPnc v) = ConversionSucceded v
unGrometPnc _ = ConversionPass

unGrometFNet :: Model -> ConversionResult JSON.Value
unGrometFNet (GrometFnet v) = ConversionSucceded v
unGrometFNet _ = ConversionPass

-------------------------------------------------------------------------------
-- ConversionResult
data ConversionResult a =
    ConversionPass
  | ConversionFailed String
  | ConversionSucceded a

instance Functor ConversionResult where
  fmap f a = a >>= (pure . f)

instance Applicative ConversionResult where
  pure a = ConversionSucceded a
  a <*> b = a >>= (<$> b)

instance Monad ConversionResult where
  a >>= f =
   case a of
     ConversionPass -> ConversionPass
     ConversionFailed s -> ConversionFailed s
     ConversionSucceded r -> f r

tryConvs :: [a -> ConversionResult b] -> a -> ConversionResult b
tryConvs fs a =
  case fs of
    [] -> ConversionPass
    f:fs' ->
      case f a of
        ConversionPass -> tryConvs fs' a
        ConversionFailed err -> ConversionFailed err
        ConversionSucceded b -> ConversionSucceded b


asEither :: (a -> ConversionResult b) -> a -> Either String b
asEither f a =
  case f a of
    ConversionFailed err -> Left err
    ConversionPass -> Left "[BUG] could not convert to desired type"
    ConversionSucceded b -> Right b

fromEither :: Either String a -> ConversionResult a
fromEither e =
  case e of
    Left err -> ConversionFailed err
    Right a -> ConversionSucceded a

-------------------------------------------------------------------------------
-- API

toEasel :: Model -> Either String ESL.Model
toEasel = asEither asEasel

toDeqs :: Model -> Either String DEQ.DiffEqs
toDeqs = asEither asDeq

toCore :: Model -> Either String Core.Model
toCore = asEither asCore

toGrometPrt :: Model -> Either String GPRT.Gromet
toGrometPrt = asEither asGrometPrt

toGrometPnc :: Model -> Either String JSON.Value
toGrometPnc = asEither (tryConvs [unGrometPnc, notExist MT.GrometPncType])

toGrometFnet :: Model -> Either String JSON.Value
toGrometFnet = asEither (tryConvs [unGrometFNet, notExist MT.GrometFnetType])

parseModel :: MT.ModelType -> Text -> Either String Model
parseModel mt s =
  case mt of
    MT.EaselType ->
      Easel <$> ESL.parseESL s
    MT.DeqType ->
      Deq <$> DEQ.parseDiffEqs s
    MT.CoreType ->
      Left "Cannot parse into core syntax - core has no concrete syntax"
    MT.GrometPrtType -> Left "Cannot parse gromet-prt - parser is not yet implemented"
    MT.GrometPncType -> GrometPnc <$> loadJSON
    MT.GrometFnetType -> GrometFnet <$> loadJSON
  where
    loadJSON = JSON.eitherDecodeStrict (Text.encodeUtf8 s)


printModel :: Model -> Either String String
printModel m =
  case m of
    Easel esl -> (Right . show . ESL.printESL) esl
    Deq deq -> (Right . show . DEQ.printDiffEqs) deq
    Core _ -> Left "cannot print core - core has no concrete syntax"
    GrometPrt g -> Right $ GPRT.grometString g
    GrometFnet v -> Right $ printJson v
    GrometPnc v -> Right $ printJson v
  where
    printJson v = BS.unpack $ JSON.encode v


