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
  , toSBMLL3V2
  , toSBMLL2V3
  , parseModel
  , printModel
  , modelUID
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Control.Monad ( (>=>) )
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Language.ASKEE.Core as Core
import qualified Language.ASKEE.DEQ as DEQ
import qualified Language.ASKEE.RNet as RNet
import qualified Language.ASKEE.ESL as ESL
import qualified Language.ASKEE.SBML as SBML

import qualified Language.ASKEE.Model.Basics as MT
import qualified Language.ASKEE.Gromet as GPRT
import qualified Language.ASKEE.Gromet.PetriNetClassic as GPNC
import qualified Language.ASKEE.Gromet.FunctionNetwork as FNET

data Model =
    Easel     ESL.Model
  | Core      Core.Model
  | Deq       DEQ.DiffEqs
  | RNet      RNet.ReactionNet
  | GrometPrt GPRT.Gromet
  | GrometPnc GPNC.PetriNetClassic
  | GrometFnet FNET.FunctionNetwork
  | SBMLL3V2  SBML.L3V2
  | SBMLL2V3  SBML.L2V3
  deriving Show

modelTypeOf :: Model -> MT.ModelType
modelTypeOf m =
  case m of
    Easel _ -> MT.EaselType
    Core _ -> MT.CoreType
    Deq _ -> MT.DeqType
    RNet _ -> MT.RNetType
    GrometPrt _ -> MT.GrometPrtType
    GrometPnc _ -> MT.GrometPncType
    GrometFnet _ -> MT.GrometFnetType
    SBMLL3V2 _ -> MT.SBMLL3V2Type
    SBMLL2V3 _ -> MT.SBMLL2V3Type

-------------------------------------------------------------------------------

asEasel :: Model -> ConversionResult ESL.Model
asEasel = tryConvs [ unEasel, notExist MT.EaselType ]

asCore :: Model -> ConversionResult Core.Model
asCore = tryConvs [ unCore
                  , unGrometPnc >=> pncToCore
                  , unRNet >=> rnetToCore
                  , asEasel >=> easelToCore
                  , notExist MT.CoreType ]
  where
    easelToCore e = pure (ESL.modelAsCore e)
    pncToCore x = fromEither (GPNC.pnToCore <$> GPNC.pnFromGromet x)
    rnetToCore x = pure (RNet.rnetToCore x)

asDeq :: Model -> ConversionResult DEQ.DiffEqs
asDeq = tryConvs [ unDeq, asCore >=> coreToDeqs, notExist MT.DeqType ]
  where
    coreToDeqs c = pure $ Core.asDiffEqs Core.NoGuards c

asGrometPrt :: Model -> ConversionResult GPRT.Gromet
asGrometPrt = tryConvs [unGrometPrt, asCore >=> fromCore, notExist MT.GrometPrtType ]
  where
    fromCore = pure . GPRT.convertCoreToGromet

asSBMLL3V2 :: Model -> ConversionResult SBML.L3V2
asSBMLL3V2 = tryConvs [unSBMLL3V2, notExist MT.SBMLL3V2Type]

asSBMLL2V3 :: Model -> ConversionResult SBML.L2V3
asSBMLL2V3 = tryConvs [unSBMLL2V3, notExist MT.SBMLL2V3Type]

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

unRNet :: Model -> ConversionResult RNet.ReactionNet
unRNet x =
  case x of
    RNet r -> ConversionSucceded r
    _      -> ConversionPass

unGrometPrt :: Model -> ConversionResult GPRT.Gromet
unGrometPrt (GrometPrt g) = ConversionSucceded g
unGrometPrt _ = ConversionPass

unGrometPnc :: Model -> ConversionResult GPNC.PetriNetClassic
unGrometPnc (GrometPnc v) = ConversionSucceded v
unGrometPnc _ = ConversionPass

unGrometFNet :: Model -> ConversionResult JSON.Value
unGrometFNet (GrometFnet v) = ConversionSucceded v
unGrometFNet _ = ConversionPass

unSBMLL3V2 :: Model -> ConversionResult SBML.L3V2
unSBMLL3V2 (SBMLL3V2 s) = ConversionSucceded s
unSBMLL3V2 _ = ConversionPass

unSBMLL2V3 :: Model -> ConversionResult SBML.L2V3
unSBMLL2V3 (SBMLL2V3 s) = ConversionSucceded s
unSBMLL2V3 _ = ConversionPass

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

toGrometPnc :: Model -> Either String GPNC.PetriNetClassic
toGrometPnc = asEither (tryConvs [unGrometPnc, notExist MT.GrometPncType])

toGrometFnet :: Model -> Either String JSON.Value
toGrometFnet = asEither (tryConvs [unGrometFNet, notExist MT.GrometFnetType])

toSBMLL3V2 :: Model -> Either String SBML.L3V2
toSBMLL3V2 = asEither asSBMLL3V2

toSBMLL2V3 :: Model -> Either String SBML.L2V3
toSBMLL2V3 = asEither asSBMLL2V3

parseModel :: MT.ModelType -> Text -> Either String Model
parseModel mt s =
  case mt of
    MT.EaselType ->
      Easel <$> ESL.parseESL s
    MT.DeqType ->
      Deq <$> DEQ.parseDiffEqs s
    MT.CoreType ->
      Left "Cannot parse into core syntax - core has no concrete syntax"
    MT.RNetType ->
      RNet <$> RNet.parseRNet s
    MT.GrometPrtType -> Left "Cannot parse gromet-prt - parser is not yet implemented"
    MT.GrometPncType -> GrometPnc <$> loadJSON
    MT.GrometFnetType -> GrometFnet <$> loadJSON
    MT.SBMLL3V2Type -> SBMLL3V2 <$> SBML.parseL3V2 (Text.unpack s)
    MT.SBMLL2V3Type -> SBMLL2V3 <$> SBML.parseL2V3 (Text.unpack s)

  where
    loadJSON :: JSON.FromJSON a => Either String a
    loadJSON = JSON.eitherDecodeStrict (Text.encodeUtf8 s)


printModel :: Model -> Either String String
printModel m =
  case m of
    Easel esl -> (Right . show . ESL.printESL) esl
    Deq deq -> (Right . show . DEQ.printDiffEqs) deq
    Core c -> Right $ show $ Core.ppModel c
    RNet _ -> Left "XXX: no printer for RNet yet"
    GrometPrt g -> Right $ Text.unpack $ GPRT.grometText g
    GrometFnet v -> Right $ printJson v
    GrometPnc v -> Right $ printJson v
    SBMLL3V2 v -> Right $ SBML.printL3V2 v
    SBMLL2V3 v -> Right $ SBML.printL2V3 v
  where
    printJson v = BS.unpack $ JSON.encode v


modelUID :: Model -> Either String Text
modelUID m =
  case m of
    Easel _ -> Left "easel models have no UID"
    Deq _ -> Left "differential equations have no UID"
    Core _ -> Left "core models have no UID"
    RNet _ -> Left "reaction networks have no UID"
    GrometPrt _ -> Left "don't know how to find PRT gromet UID" -- TODO
    GrometPnc gromet -> Right $ GPNC.pncUID gromet
    GrometFnet gromet -> FNET.fnetUID gromet
    SBMLL3V2 _ -> Left "SBML L3V2 models have no UID"
    SBMLL2V3 _ -> Left "SBML L2V3 models have no UID"