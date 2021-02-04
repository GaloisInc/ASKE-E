{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ModelStratify.Topology where

import Control.Monad.RWS

import qualified Data.Map as Map
import           Data.Maybe ( mapMaybe )
import           Data.Text ( Text, pack )

import qualified Language.ASKEE.Core as Core
import           Language.ASKEE.Core.ImportASKEE ( modelAsCore )
import           Language.ASKEE.Expr ( Expr(..) )
import           Language.ASKEE.ModelStratify.Syntax
import           Language.ASKEE.Syntax

-- | Lossy conversion from rich model to purely structural topology 
modelAsTopology :: Model -> Net
modelAsTopology Model{..} = Net states transitions stateOutputs transitionOutputs
  where
    states = Map.fromAscList (zipWith (\idx (txt, expr) -> (idx, txt)) [1..] (stateDecls modelDecls))
    transitions = Map.fromAscList (zip [1..] (map eventName modelEvents))

    -- Inverse mappings
    states' = (Map.fromList . map swap . Map.toList) states
    transitions' = (Map.fromList . map swap . Map.toList) transitions
    swap (x, y) = (y, x)

    stateOutputs = concatMap (flowFromStatement I) modelEvents
    transitionOutputs = concatMap (flowFromStatement O) modelEvents

    -- | This relies on checking whether an event results in a literal integral 
    -- change to the value of a state variable
    flowFromStatement :: FlowType -> Event -> [Flow]
    flowFromStatement ft Event{..} = concatMap go eventEffect
      where
        go :: Statement -> [Flow]
        go (txt, expr) = 
          case ft of
            I ->  case expr of
                    (Sub (Var s) (LitD d)) | s == txt && isIntegral d -> 
                      replicate (floor d) (StateToTransition (states' Map.! txt) (transitions' Map.! eventName))
                    _ -> []
            O ->  case expr of
                    (Add (Var s) (LitD d)) | s == txt && isIntegral d -> 
                      replicate (floor d) (TransitionToState (states' Map.! txt) (transitions' Map.! eventName))
                    _ -> []
        
        isIntegral :: RealFrac a => a -> Bool
        isIntegral d = d == fromInteger (round d)

-- | Lift the topology into a proper model, inserting undefined expressions
-- where necessary - rates and initial variable values, namely
topologyAsModel :: Net -> Model
topologyAsModel Net{..} = Model "foo" decls events
  where
    decls = map (flip State undef) (Map.elems netStates)
    events = map mkEvent (Map.toList netTransitions)

    mkEvent :: (TransitionID, Text) -> Event
    mkEvent (tid, name) = Event name Nothing undef (mkStatements tid netStateOutputs netTransitionOutputs) Nothing

    mkStatements :: TransitionID -> [Flow] -> [Flow] -> [Statement]
    mkStatements tid sTT tTS = mapMaybe toStatement (sTT ++ tTS)
      where
        toStatement :: Flow -> Maybe Statement
        toStatement f =
          case f of
            StateToTransition s t | t == tid ->
              let st = netStates Map.! s
              in  Just (st, Var st `Sub` LitD 1)
            TransitionToState s t | t == tid -> 
              let st = netStates Map.! s
              in  Just (st, Var st `Add` LitD 1)
            _ -> Nothing

-- | Produce both the parameterized model and the core model
topologyAsCoreModel :: Net -> Either String (Model, Core.Model)
topologyAsCoreModel n = (mdl,) <$> modelAsCore params mdl
  where
    (mdl, params) = topologyAsParameterizedModel n

-- | Produce the model with newly-created holes and a list of those holes' names
topologyAsParameterizedModel :: Net -> (Model, [Text])
topologyAsParameterizedModel n = evalRWS (parameterize (topologyAsModel n)) () 0
  where
    parameterize :: Model -> RWS () [Text] Int Model
    parameterize Model{..} = Model "foo" <$> newDecls <*> newEvents
      where
        newDecls :: RWS () [Text] Int [Decl]
        newDecls = forM modelDecls $ \d ->
          do  case d of
                Let t e -> Let t <$> maybeFreshen e
                State t e -> State t <$> maybeFreshen e
                Assert e -> Assert <$> maybeFreshen e

        newEvents :: RWS () [Text] Int [Event]
        newEvents = forM modelEvents $ \Event{..} ->
          do  eventWhen' <-
                case eventWhen of
                  Just e -> Just <$> maybeFreshen e
                  Nothing -> pure Nothing
              eventRate' <- maybeFreshen eventRate
              eventEffect' <- forM eventEffect $ \(t, e) ->
                do  e' <- maybeFreshen e
                    pure (t, e')
              pure $ Event eventName eventWhen' eventRate' eventEffect' eventMetadata
            
        maybeFreshen :: Expr -> RWS () [Text] Int Expr
        maybeFreshen e
          | e /= undef = pure e
          | otherwise =
            do  counter <- get
                modify (+ 1)
                let freshVar = pack $ "hole"++show counter
                tell [freshVar]
                pure (Var freshVar)

undef :: Expr
undef = LitD 1 `Div` LitD 0