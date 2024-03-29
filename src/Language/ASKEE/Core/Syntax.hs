{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.ASKEE.Core.Syntax where

import           GHC.Generics (Generic)
import           Control.DeepSeq (NFData)
import           Data.Map  ( Map )
import qualified Data.Map  as Map
import           Data.Set (Set)
import qualified Data.Set  as Set
import           Data.Text ( Text )
import qualified Data.Text as Text

import Language.ASKEE.Core.Expr

data Model =
  Model { modelName      :: Text
        , modelParams    :: Map Ident (Maybe Expr)
        , modelInitState :: Map Ident Expr
        , modelEvents    :: [Event]
        , modelLets      :: Map Ident Expr
          -- ^ These are the `let` bound variables from the original model
          -- We keep them here, in case one wants to observe them
          -- (e.g., measure their values)
          -- These should not be recursive.

        , modelMeta      :: !(Map Ident (Map Text [Text]))
          -- ^ Optional metadata (key/value pairs) for named things
          -- (parameters, lets, states, events)
          -- Model level metadata is under the empty identifier ""
        }
  deriving (Show, Eq, Ord, Generic, NFData)


data Event =
  Event { eventName   :: Ident
        , eventRate   :: Expr
        , eventWhen   :: Expr
        , eventEffect :: Map Ident Expr
        }
  deriving (Show, Eq, Ord, Generic, NFData)





modelStateVars :: Model -> [Ident]
modelStateVars mdl = Map.keys (modelInitState mdl)

isStateVar :: Ident -> Model -> Bool
isStateVar x m = Map.member x (modelInitState m)



instance TraverseExprs Model where
  traverseExprs f m =
    do ins  <- traverse f                 (modelInitState m)
       evs  <- traverse (traverseExprs f) (modelEvents m)
       lets <- traverse f                 (modelLets m)
       pure m { modelInitState = ins, modelEvents = evs, modelLets = lets }

instance TraverseExprs Event where
  traverseExprs f ev =
    do rate <- f (eventRate ev)
       cond <- f (eventWhen ev)
       eff  <- traverse f (eventEffect ev)
       pure ev { eventRate = rate, eventWhen = cond, eventEffect = eff }

-- One-level let-inlining
inlineLets :: Model -> Model
inlineLets m@Model{..} = m
  { modelEvents = substEvent <$> modelEvents
  , modelInitState = substExpr substitution <$> modelInitState
  }
  where
    substitution = modelLets
    substEvent = mapExprs (substExpr substitution)

-- Multi-level parameter-inlining
inlineParams :: Model -> Model
inlineParams m@Model{..} = m
  { modelEvents = substEvent <$> modelEvents
  , modelInitState = substExpr substitution <$> modelInitState
  , modelLets = substExpr substitution <$> modelLets
  , modelParams = fmap (substExpr substitution) <$> modelParams
  --  ^ chase down chains of parameter dependencies
  }
  where
    withInitCond = Map.mapMaybe id modelParams
    substitution = substExpr substitution <$> withInitCond
    substEvent = mapExprs (substExpr substitution)

-- | Instantiate some of the model parameters
applyParams' :: Map Ident Expr -> Model -> Model
applyParams' su = dropParams . mapExprs (substExpr su)
  where
  dropParams m = m
    { modelParams = Map.filterWithKey (\p _ -> not (p `Set.member` pSet)) (modelParams m) }
  pSet = Map.keysSet su


applyParams :: Map Text Double -> Model -> Model
applyParams parameters = applyParams' parameters'
  where
    parameters' = Map.map NumLit parameters


addParams' :: Map Ident Expr -> Model -> Model
addParams' newParams m@Model{..} = m { modelParams = params' }
  where
    -- Map.union is left-biased, so this picks new parameters over existing
    params' = Map.union (Map.map Just newParams) modelParams

addParams :: Map Text Double -> Model -> Model
addParams parameters = addParams' parameters'
  where
    parameters' = Map.map NumLit parameters

-- Give a model's (let | parameter | state) variables and event names C++-legal
-- identifiers, and also record the mapping of new names to old
-- TODO rename things in metadata as well?
legalize :: Model -> (Model, Map Text Ident)
legalize m@Model{..} =
  ( m'
  , Map.unions $
    map flipMap
    [stateRenaming, paramRenaming, letRenaming, eventRenaming]
  )
  where
    m' = subst $ m
      { modelEvents    = freshEventDecls
      , modelParams    = freshParamDecls
      , modelLets      = freshLetDecls
      , modelInitState = freshStateDecls }

    subst =
      mapExprs $
      substExpr $
      Map.map Var (Map.unions [stateRenaming, paramRenaming, letRenaming])

    eventRenaming = Map.fromList (zip (map eventName modelEvents) (map ("e"<>) freshening))
    paramRenaming = Map.fromList (zip (Map.keys modelParams)      (map ("p"<>) freshening))
    stateRenaming = Map.fromList (zip (modelStateVars m)          (map ("s"<>) freshening))
    letRenaming   = Map.fromList (zip (Map.keys modelLets)        (map ("l"<>) freshening))

    freshEventDecls = map modifyEvent modelEvents
    freshLetDecls   = Map.mapKeys (letRenaming Map.!) modelLets
    freshParamDecls = Map.mapKeys (paramRenaming Map.!) modelParams
    freshStateDecls = Map.mapKeys (stateRenaming Map.!) modelInitState

    modifyEvent e@Event{..} = e
      { eventName = eventRenaming Map.! eventName
      , eventEffect = Map.mapKeys (stateRenaming Map.!) eventEffect }

    freshening = map (Text.pack . show) [1::Integer ..]

    flipMap = Map.fromList . map (\(a, b) -> (b, a)) . Map.toList

-- | Prune the model to only include events influencing or influenced by,
-- directly or indirectly, the specified state variables, and the parameters
-- and let bindings necessary to let those events fire.
pruneModel :: Set Ident -> Model -> Model
pruneModel states model@Model{..} =
  model
    { modelLets = relevantLets
    , modelEvents = relevantEvents
    , modelParams = relevantParams
    , modelInitState = relevantStates
    , modelMeta = relevantMeta }
  where
    relevantVariables = Set.unions $ map collectVarsDeref relevantEvents
    relevantSources = 
      Set.unions $ 
      Set.map (maybe mempty collectVarsDeref . dereference) relevantVariables 

    dereference v =
      case (modelInitState Map.!? v, modelParams Map.!? v) of
        (Just x, _) -> Just x
        (_, Just x) -> x
        _ -> Nothing
    
    restrict what = 
      Map.restrictKeys what (relevantVariables `Set.union` relevantSources)

    relevantParams = restrict modelParams
    relevantLets   = restrict modelLets
    relevantStates = restrict modelInitState
    relevantMeta   = restrict modelMeta

    overlaps s1 s2 = not (Set.disjoint s1 s2)

    relevantEvents = pruneRelevantVia modelEvents states

    pruneRelevantVia events vars =
      case events of
        (e:es) ->
          let iVars = influencingVars e
              eVars = affectedVars e
              newVars = vars `Set.union` iVars
          in  if vars `overlaps` eVars && vars /= newVars
                then pruneRelevantVia modelEvents newVars
                else pruneRelevantVia es          vars
        [] -> filter (\e -> affectedVars e `overlaps` vars) modelEvents

    -- Collect the variables that influence execution of an event
    influencingVars Event{..} =
      collectVarsDeref eventRate `Set.union` 
      collectVarsDeref eventWhen

    affectedVars Event{..} =
      Set.unions $ map (collectVarsDeref . Var) (Map.keys eventEffect)

    -- Collect any variables that appear in `thing`, recursively dereferencing
    -- through any let-bound variables mentioned
    collectVarsDeref :: TraverseExprs t => t -> Set Ident
    collectVarsDeref thing =
      let vars = collectVars thing
          for = flip Set.map
      in  Set.unions $
            for vars \v ->
              case modelLets Map.!? v of
                Just e -> Set.insert v (collectVarsDeref e)
                Nothing -> Set.singleton v
