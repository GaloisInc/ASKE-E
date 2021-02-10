{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ModelStratify.Topology where

import Control.Monad.RWS

import           Data.Char ( isDigit )
import qualified Data.Map as Map
import           Data.Maybe ( mapMaybe )
import           Data.Text ( Text, pack )
import qualified Data.Text as Text

import qualified Language.ASKEE.Core as Core
import           Language.ASKEE.Core.ImportASKEE ( modelAsCore )
import           Language.ASKEE.Expr ( Expr(..) )
import           Language.ASKEE.ModelStratify.Syntax
import           Language.ASKEE.Syntax
import Data.Map (Map)
import Data.Either (fromRight)
import Data.Text.Read (decimal)
import Language.ASKEE.ExprTransform (transformExpr)
import Control.Monad.Identity

-- | Lossy conversion from rich model to purely structural topology 
modelAsTopology :: Model -> Net
modelAsTopology Model{..} = Net states transitions stateOutputs transitionOutputs
  where
    states = Map.fromAscList (zipWith (\idx (txt, _) -> (idx, txt)) [1..] (stateDecls modelDecls))
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
topologyAsCoreModel :: Map Int Text -> Net -> Either String (Model, Core.Model)
topologyAsCoreModel m n = (mdl,) <$> modelAsCore params mdl
  where
    (mdl, params) = topologyAsParameterizedModel m n

topologyAsParameterizedModel :: Map Int Text -> Net -> (Model, [Text])
topologyAsParameterizedModel m n = evalRWS (parameterize (topologyAsModel n)) () 0
  where
    parameterize :: Model -> RWS () [Text] Int Model
    parameterize Model{..} = Model "foo" <$> newDecls <*> newEvents
      where
        newDecls :: RWS () [Text] Int [Decl]
        newDecls = forM modelDecls $ \d ->
          do  case d of
                Let t e -> Let t <$> maybeFreshen (Just $ t<>"_initial") e
                State t e -> State (updateText t) <$> maybeFreshen (Just $ updateText t<>"_initial") e
                Assert e -> Assert <$> maybeFreshen Nothing e

        newEvents :: RWS () [Text] Int [Event]
        newEvents = forM modelEvents $ \Event{..} ->
          do  let eventName' = updateText eventName
              eventWhen' <-
                case eventWhen of
                  Just e -> Just <$> maybeFreshen (Just $ eventName'<>"_when") e
                  Nothing -> pure Nothing
              eventRate' <- maybeFreshen (Just $ eventName'<>"_rate") eventRate
              eventEffect' <- forM eventEffect $ \(t, e) ->
                do  e' <- updateExpr <$> maybeFreshen (Just $ eventName'<>"_effect") e
                    let t' = updateText t
                    pure (t', e')
              pure $ Event eventName' eventWhen' eventRate' eventEffect' eventMetadata
            
        maybeFreshen :: Maybe Text -> Expr -> RWS () [Text] Int Expr
        maybeFreshen t e
          | e /= undef = pure e
          | otherwise =
            do  counter <- get
                modify (+ 1)
                let freshVar = 
                      case t of
                        Just t' -> t'<>"_"<>pack (show counter)
                        Nothing -> "hole"<>"_"<>pack (show counter)
                tell [freshVar]
                pure (Var freshVar)

        updateExpr :: Expr -> Expr
        updateExpr = runIdentity . transformExpr go
          where
            go :: Expr -> Identity Expr
            go e =
              case e of
                Var v -> pure $ Var $ updateText v
                _ -> pure e

        updateText :: Text -> Text
        updateText t =
          let numbersIn = filter (Text.all isDigit) $ Text.splitOn "_" t
              irrefutableTextAsNumber = fst . fromRight (error "Language.ASKEE.ModelStratify.Topology: internal error: not a number") . decimal
          in  foldr (\num res -> Text.replace num (m Map.! irrefutableTextAsNumber num) res) t numbersIn


undef :: Expr
undef = LitD 1 `Div` LitD 0