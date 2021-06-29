{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.AlgebraicJulia.Topology where

import Control.Monad          ( forM )
import Control.Monad.RWS      ( modify
                              , evalRWS
                              , get
                              , tell
                              , RWS )
import Control.Monad.Identity ( Identity(..) )

import           Data.Char      ( isDigit )
import           Data.Either    ( fromRight )
import           Data.Map       ( Map )
import qualified Data.Map       as Map
import           Data.Maybe     ( mapMaybe )
import           Data.Text      ( Text, pack )
import qualified Data.Text      as Text
import           Data.Text.Read ( decimal )

import Language.ASKEE.Expr                 ( Expr(..) )
import Language.ASKEE.ExprTransform        ( transformExpr )
import Language.ASKEE.AlgebraicJulia.Syntax ( Flow(..)
                                           , FlowType(..)
                                           , Net(..)
                                           , TransitionID ) 
import Language.ASKEE.ESL.Syntax           ( stateDecls
                                           , Decl(..)
                                           , Event(..)
                                           , Model(..)
                                           , Statement )
import Language.ASKEE.Metadata

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
    decls = map (pure . flip State undef) (Map.elems netStates)
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

hole :: Text 
hole = "hole_"

isHole :: Text -> Bool
isHole t = Text.take 5 t == "hole_"

insertHoles :: Model -> (Model, [Text])
insertHoles Model{..} = evalRWS fill () 0
  where
    fill :: RWS () [Text] Int Model
    fill = Model "foo" <$> (map pure <$> newDecls) <*> newEvents

    newDecls :: RWS () [Text] Int [Decl]
    newDecls = forM (map metaValue modelDecls) $ \case
      Let t e -> Let t <$> maybeFreshen e
      State t e -> State t <$> maybeFreshen e
      Assert e -> Assert <$> maybeFreshen e
      Parameter t Nothing -> pure $ Parameter t Nothing
      Parameter t (Just e) -> maybeFreshen e >>= \e' -> pure $ Parameter t (Just e')

    newEvents :: RWS () [Text] Int [Event]
    newEvents = forM modelEvents $ \Event{..} ->
      do  eventWhen' <-
            case eventWhen of
              Just e -> Just <$> maybeFreshen e
              Nothing -> pure Nothing
          eventRate' <- maybeFreshen eventRate
          eventEffect' <- forM eventEffect $ \(t, e) ->
            do  e' <- maybeFreshen e
                let t' = t
                pure (t', e')
          pure $ Event eventName eventWhen' eventRate' eventEffect' eventMetadata
        
    maybeFreshen :: Expr -> RWS () [Text] Int Expr
    maybeFreshen e
      | e /= undef = pure e
      | otherwise =
        do  counter <- get
            modify (+ 1)
            let howMany = (counter `div` 26) + 1
                chars = replicate howMany $ toEnum ((counter `mod` 26) + fromEnum 'A')
                var = hole<>pack chars
            tell [var]
            pure (Var var)

nameHoles :: Map Int Text -> Model -> Model
nameHoles nodeNames Model{..} = Model modelName (map pure renamedDecls) renamedEvents
  where
    renamedDecls :: [Decl]
    renamedDecls = for (map metaValue modelDecls) $ \case
      Let t e -> Let (updateText' t) (updateExpr (Just $ updateText' t<>"_initial") e)
      State t e -> State (updateText' t) (updateExpr (Just $ updateText' t<>"_initial") e)
      Assert e -> Assert (updateExpr' e)
      Parameter t e -> Parameter t (updateExpr' <$> e)
    
    renamedEvents :: [Event]
    renamedEvents = flip map modelEvents $ \Event{..} ->
      let eventName' = updateText' eventName
          eventWhen' = updateExpr (Just $ eventName'<>"_when") <$> eventWhen
          eventRate' = updateExpr (Just $ eventName'<>"_rate") eventRate
          eventEffect' = flip map eventEffect $ \(t, e) ->
            let t' = updateText' t
                e' = updateExpr' e
            in  (t', e')
      in  Event eventName' eventWhen' eventRate' eventEffect' eventMetadata

    updateExpr :: Maybe Text -> Expr -> Expr
    updateExpr replacementM = runIdentity . transformExpr go
      where
        go :: Expr -> Identity Expr
        go e =
          case e of
            Var v -> pure $ Var $ updateText replacementM v
            _ -> pure e

    -- Find Ints bracketed by underscores in the given variable name
    -- and replace them with their value in the given name mapping.
    -- Then, check if the resulting variable is a hole, and if so, give
    -- it a more descriptive name, if possible
    updateText :: Maybe Text -> Text -> Text
    updateText replacementM t =
      let numbersIn = filter (Text.all isDigit) $ Text.splitOn "_" t
          textAsNumber = 
            fst . fromRight (error "Topology: internal error: not a number") . decimal
          replace num res = Text.replace num (nodeNames Map.! textAsNumber num) res
          withNames = foldr replace t numbersIn
      in  case replacementM of
            Just replacement -> if isHole withNames then replacement else withNames
            Nothing -> withNames

    updateText' = updateText Nothing
    updateExpr' = updateExpr Nothing

    for = flip map

undef :: Expr
undef = LitD 1 `Div` LitD 0