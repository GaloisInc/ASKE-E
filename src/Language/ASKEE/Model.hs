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
) where

import Control.Monad((>=>))

import qualified Language.ASKEE.ESL.Syntax as Easel
import qualified Language.ASKEE.ESL.GenParser as ESLParser
import qualified Language.ASKEE.ESL.GenLexer as ESLLexer

import qualified Language.ASKEE.DEQ.GenParser as DEQParser
import qualified Language.ASKEE.DEQ.GenLexer as DEQLexer

import qualified Language.ASKEE.Core.Syntax as Core
import qualified Language.ASKEE.ESL.Convert as EaselConvert
import qualified Language.ASKEE.Core.Convert as CoreConvert
import qualified Language.ASKEE.DEQ.Syntax as DEQ
-- import qualified Language.ASKEE.DEQ as DEQ
-- import qualified Language.ASKEE.Core.DiffEq as CDEQ
import qualified Language.ASKEE.ModelType as MT

data Model =
    Easel Easel.ModelMeta
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

asEasel :: Model -> ConversionResult Easel.ModelMeta
asEasel = tryConvs [ unEasel, notExist "easel" ]

asCore :: Model -> ConversionResult Core.Model
asCore = tryConvs [ unCore, asEasel >=> easelToCore, notExist "core" ]
  where
    easelToCore e = fromEither (EaselConvert.modelAsCore $ Easel.stripMeta e)

asDeq :: Model -> ConversionResult DEQ.DiffEqs
asDeq = tryConvs [ unDeq, asCore >=> coreToDeqs, notExist "deq" ]
  where
    coreToDeqs c = pure $ CoreConvert.asEquationSystem c

notExist :: String -> Model -> ConversionResult a
notExist tgt mdl =
  ConversionFailed ("could not convert model '" ++ describeModelType mdl ++ "' to '" ++ tgt ++ "'")

unEasel :: Model -> ConversionResult Easel.ModelMeta
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

toEasel :: Model -> Either String Easel.ModelMeta
toEasel = asEither asEasel

toDeqs :: Model -> Either String DEQ.DiffEqs
toDeqs = asEither asDeq

toCore :: Model -> Either String Core.Model
toCore = asEither asCore

parseModel :: MT.ModelType -> String -> Either String Model
parseModel mt s =
  case mt of
    MT.EaselType ->
      Easel <$> (ESLLexer.lexModel s >>= ESLParser.parseModelMeta)
    MT.DeqType ->
      Deq <$> (DEQLexer.lexDEQs s >>= DEQParser.parseDEQs)
    MT.CoreType ->
      Left "Cannot parse into core syntax - core has no concrete syntax"
