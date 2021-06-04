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
  , parseModel
  , printModel
  ) where

import Control.Monad ( (>=>) )

import qualified Language.ASKEE.Core as Core
import qualified Language.ASKEE.DEQ as DEQ
import qualified Language.ASKEE.ESL as ESL

import qualified Language.ASKEE.ModelType as MT

data Model =
    Easel ESL.ModelMeta
  | Core  Core.Model
  | Deq   DEQ.DiffEqs

-- TODO: model type
describeModelType :: Model -> String
describeModelType m =
  case m of
    Easel _ -> "easel"
    Core _ -> "easel core"
    Deq _ -> "differential equations"

-------------------------------------------------------------------------------

asEasel :: Model -> ConversionResult ESL.ModelMeta
asEasel = tryConvs [ unEasel, notExist "easel" ]

asCore :: Model -> ConversionResult Core.Model
asCore = tryConvs [ unCore, asEasel >=> easelToCore, notExist "core" ]
  where
    easelToCore e = fromEither (ESL.modelAsCore $ ESL.stripMeta e)

asDeq :: Model -> ConversionResult DEQ.DiffEqs
asDeq = tryConvs [ unDeq, asCore >=> coreToDeqs, notExist "deq" ]
  where
    coreToDeqs c = pure $ Core.asDiffEqs c

notExist :: String -> Model -> ConversionResult a
notExist tgt mdl =
  ConversionFailed ("could not convert model '" ++ describeModelType mdl ++ "' to '" ++ tgt ++ "'")

unEasel :: Model -> ConversionResult ESL.ModelMeta
unEasel (Easel e) = ConversionSucceded e
unEasel _ = ConversionPass

unCore :: Model -> ConversionResult Core.Model
unCore (Core c) = ConversionSucceded c
unCore _ = ConversionPass

unDeq :: Model -> ConversionResult DEQ.DiffEqs
unDeq (Deq d) = ConversionSucceded d
unDeq _ = ConversionPass

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

toEasel :: Model -> Either String ESL.ModelMeta
toEasel = asEither asEasel

toDeqs :: Model -> Either String DEQ.DiffEqs
toDeqs = asEither asDeq

toCore :: Model -> Either String Core.Model
toCore = asEither asCore

parseModel :: MT.ModelType -> String -> Either String Model
parseModel mt s =
  case mt of
    MT.EaselType ->
      Easel <$> ESL.parseESLMeta s
    MT.DeqType ->
      Deq <$> DEQ.parseDiffEqs s
    MT.CoreType ->
      Left "Cannot parse into core syntax - core has no concrete syntax"

printModel :: Model -> Either String String
printModel m =
  case m of
    Easel esl -> (Right . show . ESL.printESL . ESL.stripMeta) esl
    Deq deq -> (Right . show . DEQ.printDiffEqs) deq
    Core _ -> Left "cannot print core - core has no concrete syntax"