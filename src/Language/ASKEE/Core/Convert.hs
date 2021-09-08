{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.Core.Convert where

import qualified Data.Map      as Map
import           Data.Maybe    ( mapMaybe )
import qualified Data.Text     as Text

import           Language.ASKEE.Core.Expr
import qualified Language.ASKEE.Core.Expr         as Core
import           Language.ASKEE.Core.Syntax
import           Language.ASKEE.DEQ.Syntax        ( DiffEqs(..) )
import qualified Language.ASKEE.ESL.Syntax        as ESL
import           Language.ASKEE.Metadata ( MetaAnn(..) )

-------------------------------------------------------------------------------
-- DiffEqs

data ToDiffEqMethod = WithGuards | NoGuards

asDiffEqs :: ToDiffEqMethod -> Model -> DiffEqs
asDiffEqs how mdl =
  DiffEqs { deqParams  = modelParams mdl
          , deqInitial = modelInitState mdl
          , deqRates   = Map.mapWithKey stateEq (modelInitState mdl)
          , deqLets    = modelLets mdl
          }
  where
  stateEq sv _ = simplifyExpr
               $ foldr (:+:) (NumLit 0)
               $ mapMaybe (eventTerm how sv)
               $ modelEvents mdl

-- Eventually we may want to consider approaches to try to make guard
-- continues (e.g., sigmoid?)
eventTerm :: ToDiffEqMethod -> Core.Ident -> Event -> Maybe Expr
eventTerm how sv event =
  do stExp <- Map.lookup sv (eventEffect event)
     let e = eventRate event :*: (stExp :-: Var sv)
     pure case how of
            WithGuards -> If (eventWhen event) e (NumLit 0)
            NoGuards   -> e


coreAsModel :: Model -> ESL.Model
coreAsModel m = ESL.Model newName newDecls newEvents []
  where
    newName = modelName m
    newDecls = 
      [ MetaAnn (meta v) (ESL.Let v (asExpr e)) 
      | (v, e) <- Map.toList (modelLets m) 
      ] ++
      [ MetaAnn (meta v) (ESL.State v (asExpr e)) 
      | (v, e) <- Map.toList (modelInitState m) 
      ] ++
      [ MetaAnn (meta v) (ESL.Parameter v (asExpr <$> e)) 
      | (v, e) <- Map.toList (modelParams m) 
      ]

    newEvents = 
      [ ESL.Event 
          { ESL.eventName = eventName 
          , ESL.eventWhen = Just (asExpr eventWhen)
          , ESL.eventRate = asExpr eventRate
          , ESL.eventEffect = map (asExpr <$>) (Map.toList eventEffect)
          , ESL.eventMetadata = Nothing -- XXX do better
          }
      | Event{..} <- modelEvents m
      ]

    meta v = 
      case modelMeta m Map.!? v of
        Just info -> map (Text.intercalate "\n" <$>) (Map.toList info)
        Nothing -> []