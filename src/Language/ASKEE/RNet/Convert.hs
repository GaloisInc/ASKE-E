{-# Language OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Language.ASKEE.RNet.Convert (rnetToCore) where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Language.ASKEE.RNet.Syntax as Src
import qualified Language.ASKEE.ESL.Convert as ESL
import           Language.ASKEE.Core.Expr
import           Language.ASKEE.Core.Syntax

rnetToCore :: Src.ReactionNet -> Model
rnetToCore rn = Model
  { modelName   = "reaction-net"    -- XXX: maybe  these should have a name?

  , modelParams = Map.fromList params

  , modelInitState =
    Map.fromList [ (x, Var (initName x)) | x <- Set.toList states ]

  , modelEvents = zipWith rToEvent [1..] (Src.reactions rn)
  , modelLets   = Map.fromList lets
  , modelMeta   = Map.empty -- XXX
  }
  where
  params = [ (initName x, Nothing)
           | x <- Set.toList (states `Set.difference` Map.keysSet initBinds)
           ] ++
           [ (x, Just e) | (x,e) <- letParams ]

  (letParams,lets) = partOrderDecls states
                   $ doLets initName initBinds ++
                     doLets id       otherBinds

  states = Set.unions (map rVars (Src.reactions rn))

  (initBinds,otherBinds) = Map.partitionWithKey (\x _ -> x `Set.member` states)
                                                (Src.bindings rn)

  doLets f mp = [ (f x, ESL.expAsCore e) | (x,e) <- Map.toList mp ]



initName :: Text -> Text
initName name = name <> "_init"

rToEvent :: Int -> Src.Reaction -> Event
rToEvent name r = Event
  { eventName   = "event_" <> Text.pack (show name)

  , eventRate =
    let val = ESL.expAsCore (Src.reactionRate r)
    in case Src.reactionType r of
         Src.FixedRate -> val
         Src.MassAction ->
           foldr (:*:) val
            (Var <$> (Set.toList (reVars (Src.reactionSubstrates r))))

  , eventWhen =
    case (ESL.expAsCore <$> Src.reactionEnabled r, enoughInput) of
      (Nothing,Nothing) -> BoolLit True
      (Just a, Nothing) -> a
      (Nothing, Just b) -> b
      (Just a, Just b)  -> a :&&: b

  , eventEffect =
    let eqn x y = if y > 0 then Var x :+: NumLit (fromIntegral y)
                           else Var x :-: NumLit (fromIntegral (negate y))
    in Map.mapWithKey eqn (Map.unionWith (+) outs (negate <$> ins))
  }
  where
  ins  = groupRE (Src.reactionSubstrates r)
  outs = groupRE (Src.reactionProducts r)

  enoughInput =
    case [ NumLit (fromIntegral n) :<=: Var x | (x,n) <- Map.toList ins
                                              , n /= 0 ] of
      [] -> Nothing
      es -> Just (foldr1 (:&&:) es)


groupRE :: Src.ReactionExp -> Map Text Int
groupRE re =
  case re of
    Src.Nil -> Map.empty
    Src.ReactionTerms xs ->
      Map.fromListWith (+) [ (x,n) | Src.ReactionTerm n x <- xs ]

rtVar :: Src.ReactionTerm -> Text
rtVar (Src.ReactionTerm _ x) = x

reVars :: Src.ReactionExp -> Set Text
reVars e =
  case e of
    Src.Nil -> Set.empty
    Src.ReactionTerms rts -> Set.fromList (map rtVar rts)

rVars :: Src.Reaction -> Set Text
rVars r = Set.union (reVars (Src.reactionSubstrates r))
                    (reVars (Src.reactionProducts r))


