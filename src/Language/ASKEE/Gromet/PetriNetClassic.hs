{-# Language OverloadedStrings, BlockArguments, RecordWildCards #-}
module Language.ASKEE.Gromet.PetriNetClassic where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Maybe(fromMaybe)
import Data.Map(Map)
import qualified Data.Map as Map

import Text.PrettyPrint hiding ((<>))

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?))
import qualified Data.Aeson as JSON
import Language.ASKEE.Gromet.Common


--------------------------------------------------------------------------------
data PetriNet = PetriNet
  { pnName        :: Text
  , pnStates      :: Map JunctionUid StateVar
  , pnTransitions :: Map JunctionUid Event
  } deriving Show

data StateVar = StateVar
  { sName    :: Text
  , sInitial :: Maybe Integer
  } deriving Show

data Event = Event
  { evRate   :: Maybe Double
  , evName   :: Text
  , evRemove :: Map JunctionUid Integer
  , evAdd    :: Map JunctionUid Integer
  } deriving Show

ppPetriNet :: PetriNet -> Doc
ppPetriNet pn = vcat
  [ "model" <+> ppT (pnName pn)
  , nest 2 (vcat (map ppS  (Map.elems (pnStates pn))))
  , nest 2 (vcat (map ppEv (Map.elems (pnTransitions pn))))
  ]

  where
  ppT = quotes . text . Text.unpack
  ppU (JunctionUid x) = ppT x
  ppS s = "state" <+> ppT (sName s) <+> case sInitial s of
                                          Nothing -> empty
                                          Just i -> "=" <+> integer i
  ppV uid = case Map.lookup uid (pnStates pn) of
              Just s  -> ppT (sName s)
              Nothing -> ppU uid -- BUG

  ppEv ev = "event" <+> ppT (evName ev) $$ nest 2 (vcat [cond,rate,eff])
    where rate = case evRate ev of
                   Nothing -> empty
                   Just r  -> "rate:" <+> double r
          cond = case Map.toList (evRemove ev) of
                   [] -> empty
                   xs -> "when:"
                      $$ nest 2 (foldr1 (\a b -> a <+> "and" <+> b)
                                [ ppV v <+> ">=" <+> integer n | (v,n) <- xs ])
          eff = case (Map.toList (evRemove ev), Map.toList (evAdd ev)) of
                  ([],[]) -> empty
                  (xs,ys) -> "effect:" $$
                                vcat (map (doOp "-") xs ++ map (doOp "+") ys)

          doOp op (v,n) = ppV v <+> "=" <+> ppV v <+> op <+> integer n

pnFromPNC :: PetriNetClassic -> Either String PetriNet
pnFromPNC pnc =
  foldr addW (foldr addJ (Right emptyPN) (pncJunctions pnc)) (pncWires pnc)
  where
  emptyPN = PetriNet { pnName = pncName pnc
                     , pnStates = mempty, pnTransitions = mempty }
  addS uid nm mb pn =
    pn { pnStates = Map.insert uid (emptyS nm mb) (pnStates pn) }
  setT uid ev pn = pn { pnTransitions = Map.insert uid ev (pnTransitions pn) }
  addEv uid nm mb = setT uid (emptyEv nm mb)
  emptyEv n mb =
    Event { evRate = mb, evName = n, evRemove = mempty, evAdd = mempty }
  emptyS n mb =
    StateVar { sName = n, sInitial = mb }

  toInt l = case l of
              LitInteger i -> Right i
              _            -> Left "Expected an integer"

  toDouble l = case l of
                 LitInteger i -> Right (fromIntegral i)
                 LitReal i    -> Right i
                 _            -> Left "Expecetd a real number"

  addOut uid ev = ev { evAdd    = Map.insertWith (+) uid 1 (evAdd ev) }
  addIn  uid ev = ev { evRemove = Map.insertWith (+) uid 1 (evRemove ev) }


  addJ j pn =
    do p <- pn
       case jType j of
         State ->
           do mb <- traverse toInt (jValue j)
              pure (addS (jUID j) (jName j) mb p)
         Transition ->
           do mb <- traverse toDouble (jValue j)
              pure (addEv (jUID j) (jName j) mb p)

  addW :: Wire -> Either String PetriNet -> Either String PetriNet
  addW w pn =
    do p <- pn
       let src = wireSrc w
           tgt = wireTgt w
       case Map.lookup src (pnTransitions p) of
         Just ev -> pure (setT src (addOut tgt ev) p)
         Nothing ->
           case Map.lookup tgt (pnTransitions p) of
             Just ev -> pure (setT tgt (addIn src ev) p)
             Nothing -> Left "Incorrect wire"

--------------------------------------------------------------------------------
-- Gromet Representation of Petri Nets

data PetriNetClassic = PetriNetClassic
  { pncName      :: Text
  , pncUID       :: Text
  , pncJunctions :: [Junction]
  , pncWires     :: [Wire]
  , pncRoot      :: BoxUid
  } deriving Show

data Wire = Wire
  { wireUID :: WireUid
  , wireSrc :: JunctionUid
  , wireTgt :: JunctionUid
  } deriving Show

data Junction = Junction
  { jName      :: Text
  , jUID       :: JunctionUid
  , jValueType :: ValueType
  , jValue     :: Maybe Literal
  , jType      :: JunctionType
  } deriving Show

data JunctionType = State | Transition
  deriving Show


--------------------------------------------------------------------------------

instance ToJSON Junction where
  toJSON j = JSON.object
    [ "uid"        .= jUID j
    , "syntax"     .= jsText "Junction"
    , "type"       .= jType j
    , "name"       .= jName j
    , "metadata"   .= JSON.Null
    , "value_type" .= jValueType j
    , "value"      .= jValue j
    ]

instance FromJSON Junction where
  parseJSON = JSON.withObject "JUNCTION" \o ->
    do jUID       <- o .: "uid"
       jType      <- o .: "type"
       jName      <- o .: "name"
       jValueType <- fromMaybe Real <$> (o .:? "value_type")
       jValue     <- o .:? "value"
       pure Junction { .. }

instance ToJSON JunctionType where
  toJSON t =
    case t of
      State       -> "State"
      Transition  -> "Rate"

instance FromJSON JunctionType where
  parseJSON = JSON.withText "JUNCTION_TYPE" \txt ->
    case txt of
      "State" -> pure State
      "Rate"  -> pure Transition
      _       -> fail ("Unknown JUNCTION_TYPE: " <> Text.unpack txt)

instance ToJSON Wire where
  toJSON w = JSON.object
    [ "uid"      .= wireUID w
    , "src"      .= wireSrc w
    , "tgt"      .= wireTgt w
    ]

instance FromJSON Wire where
  parseJSON = JSON.withObject "WIRE" \o ->
    do wireUID  <- o .: "uid"
       wireSrc  <- o .: "src"
       wireTgt  <- o .: "tgt"
       pure Wire { .. }

instance ToJSON PetriNetClassic where
  toJSON p = JSON.object
    [ "syntax"    .= jsText "Gromet"
    , "type"      .= jsText "PetriNetClassic"
    , "name"      .= pncName p
    , "metadata"  .= JSON.Null
    , "uid"       .= pncUID p
    , "root"      .= pncRoot p
    , "junctions" .= pncJunctions p
    , "wires"     .= pncWires p
    , "boxes"     .=
        [ JSON.object
            [ "uid"       .= pncRoot p
            , "metadata"  .= JSON.Null
            , "syntax"    .= jsText "Relation"
            , "type"      .= jsText "PetriNetClassic"
            , "name"      .= pncName p
            , "wires"     .= map wireUID (pncWires p)
            , "junctions" .= map jUID (pncJunctions p)
            ]
        ] 
    ]

instance FromJSON PetriNetClassic where
  parseJSON = JSON.withObject "PETRI_NET_CLASSIC" \o ->
    do pncName <- o .: "name"
       pncUID  <- o .: "uid"
       pncRoot <- o .: "root"
       pncJunctions <- o .: "junctions"
       pncWires     <- o .: "wires"
       pure PetriNetClassic { .. }




