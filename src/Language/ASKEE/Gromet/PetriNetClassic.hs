{-# Language OverloadedStrings, BlockArguments, RecordWildCards #-}
module Language.ASKEE.Gromet.PetriNetClassic
  ( -- * Classic Petri Nets
    PetriNet(..)
  , StateVar(..)
  , Event(..)
  , ppPetriNet
  , pnFromGromet
  , pnToCore

    -- * Gromet representation of Petri Nets
  , PetriNetClassic(..)
  , Wire(..)
  , Junction(..)
  , JunctionType(..)
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Maybe(fromMaybe)
import Data.Map(Map)
import qualified Data.Map as Map

import Text.PrettyPrint hiding ((<>))

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?))
import qualified Data.Aeson as JSON

import Language.ASKEE.Gromet.Common
import qualified Language.ASKEE.Core.Syntax as Core
import qualified Language.ASKEE.Core.Expr as Core


--------------------------------------------------------------------------------
data PetriNet = PetriNet
  { pnName        :: Text
  , pnStates      :: Map JunctionUid StateVar
  , pnTransitions :: Map JunctionUid Event
  } deriving Show

data StateVar = StateVar
  { sName    :: Text
  , sInitial :: Maybe Double
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
                                          Just i -> "=" <+> double i
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

pnFromGromet :: PetriNetClassic -> Either String PetriNet
pnFromGromet pnc =
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
           do mb <- traverse toDouble (jValue j)
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

pnToCore :: PetriNet -> Core.Model
pnToCore pn =
  Core.Model
    { modelName      = pnName pn
    , modelParams    = sParams ++ rParams
    , modelInitState = Map.fromList (zip sUIds sInit)
    , modelEvents    = coreEvs
    , modelLets      = Map.fromList
                     $ [ (x,y) | (x, Just y) <- zip sParamLets sLets ] ++
                       [ (x,y) | (x, Just y) <- zip rParamLets rLets ]
    , modelMeta      = Map.fromList
                     $ zipWith mkMeta sParamLets spMeta ++
                       zipWith mkMeta rParamLets rpMeta ++
                       zipWith mkMeta sUIds      sMeta  ++
                       zipWith mkMeta rUID       eMeta
    }
  where
  ss  = Map.toList (pnStates pn)
  evs = Map.toList (pnTransitions pn)
  mkMeta uid ms = (uid, Map.fromList [ (k,[v]) | (k,v) <- ms ])

  initName (JunctionUid x) = x <> "_init"
  rateName (JunctionUid x) = x <> "_rate"
  jToName  (JunctionUid x) = x

  sParams = [ x | (x,Nothing) <- zip sParamLets sLets ]
  rParams = [ x | (x,Nothing) <- zip rParamLets rLets ]

  (sParamLets,sLets,spMeta) =
    unzip3 [ (uid, def,theMeta)
           | (x, s) <- ss
           , let uid     = initName x
                 def     = Core.NumLit <$> sInitial s
                 theMeta = [ ("group", "Initial State")
                           , ("name",   sName s)
                           ]
           ]

  (rParamLets,rLets,rpMeta) =
    unzip3 [ (uid, def, theMeta)
           | (x, t) <- evs
           , let uid     = rateName x
                 def     = Core.NumLit <$> evRate t
                 theMeta = [ ("group", "Rate")
                           , ("name",  evName t)
                           ]
           ]


  (sUIds, sInit, sMeta) =
    unzip3 [ (uid, e, theMeta)
           | (x, s) <- ss
           , let uid     = jToName x
                 e       = Core.Var (initName x)
                 theMeta = [ ("name", sName s) ]
           ]

  (rUID, coreEvs, eMeta) =
    unzip3 [ (uid, ev, theMeta)
           | (x, t) <- evs
           , let uid = jToName x
                 ev = Core.Event
                        { eventName = uid
                        , eventRate =
                            let base = Core.Var (rateName x)
                            in foldr (Core.:*:) base
                                 [ Core.Var (jToName y)
                                 | y <- Map.keys (evRemove t)
                                 ]
                        , eventWhen = whenCond (evRemove t)
                        , eventEffect =
                            mkEff (Map.unionWith (-) (evAdd t) (evRemove t))
                        }
                 theMeta = [ ("name", evName t) ]
           ]

  whenCond xs =
    case Map.toList xs of
      [] -> Core.BoolLit True
      ys -> foldr1 (Core.:&&:)
            [ Core.NumLit (fromInteger n) Core.:<=: Core.Var (jToName x)
            | (x,n) <- ys
            ]

  mkEff xs = Map.fromList
               [ (uid, op (Core.Var uid))
               | (x,n) <- Map.toList xs, n /= 0
               , let uid = jToName x
                     op
                      | n > 0     = (Core.:+: Core.NumLit (fromInteger n))
                      | otherwise = (Core.:-: Core.NumLit (fromInteger (-n)))
               ]

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
    , "syntax"   .= ("Wire" :: String)
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
    , "ports" .= JSON.Null
    , "types" .= JSON.Null
    , "variables" .= JSON.Null
    , "literals" .= JSON.Null
    ]

instance FromJSON PetriNetClassic where
  parseJSON = JSON.withObject "PETRI_NET_CLASSIC" \o ->
    do pncName <- o .: "name"
       pncUID  <- o .: "uid"
       pncRoot <- o .: "root"
       pncJunctions <- o .: "junctions"
       pncWires     <- o .: "wires"
       pure PetriNetClassic { .. }




