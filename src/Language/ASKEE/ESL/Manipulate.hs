{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ESL.Manipulate where

import           Data.Map  ( Map )
import qualified Data.Map  as Map
import           Data.Set  ( Set )
import qualified Data.Set  as Set
import           Data.Text ( Text )
import qualified Data.Text as Text

import Language.ASKEE.ESL.Syntax    ( letDecls
                                    , parameterDecls
                                    , stateDecls
                                    , Decl(..)
                                    , Event(..)
                                    , Model(..) )
import Language.ASKEE.Expr          ( Expr(..) )
import Language.ASKEE.ExprTransform ( renameEventVarsWith
                                    , renameExprVarsWith
                                    , renameModelVarsWith )

data CombinationStrategy = Sum | Average

ensemble :: [(Model, Double)] -> CombinationStrategy -> Model
ensemble models combo = megaModel { modelDecls = [pure (Let v e) | (v, e) <- newLets] ++ modelDecls megaModel }
  where
    megaModel = foldr1 (join mempty "" "") (map (\(model,_,_) -> model) freshModels)

    freshModels = flatten $ zipWith (\i (m, d) -> (freshenModel i m, d)) [1::Integer ..] models

    freshenModel i m =
      let varMap = Map.fromList [ (n, freshenVar i n) | n <- Set.toList (modelVars m) ]
      in  (renameModelVarsWith (varMap Map.!) m, varMap)

    freshenVar i n = n<>"_"<>Text.pack (show i)

    newLets = map (\v -> (v, mkLet v)) (Set.toList sharedState)

    mkLet v =
      let vs = map (\(_, newVars, scaling) -> Var (newVars Map.! v) `Mul` LitD scaling) freshModels
      in  case combo of
            Sum -> foldr1 Add vs
            Average -> foldr1 Add vs `Div` LitD (sum $ map (\(_,_,scaling) -> scaling) freshModels)

    sharedState = foldr Set.intersection allVars [ modelVars' m | (m, _) <- models ]

    allVars = Set.unions (map (modelVars' . fst) models)

    flatten = map (\((x,y),z) -> (x,y,z))

    modelVars' m = 
      modelVars m 
      `Set.difference` 
      Set.fromList ([ eventName | Event{..} <- modelEvents m ] ++ [ v | (v,_) <- letDecls (modelDecls m) ])


-- | Serial composition of two models.
--
-- We expose state sharing here to ease the case where, when composing two 
-- models that manipulate similar state, the second model may more easily "pick 
-- up where the first left off" - that is, work with the modifications the 
-- first made to the state they both seek to represent.
compose :: 
  Map Text Text {- ^ Share these states between the models -} -> 
  Model -> 
  Model -> 
  Expr {- ^ Execute the first model while this expression is false -} -> 
  Expr {- ^ Execute the second model while this expression is true -} -> 
  Model
compose stateShare m1 m2 stop1 start2 = join stateShare "" "" m1' m2'
  where
    m1' = m1 { modelEvents = map (doWhen (Not stop1)) (modelEvents m1) }
    m2' = m2 { modelEvents = map (doWhen start2) (modelEvents m2) }

    doWhen e Event{..} =
      case eventWhen of
        Nothing -> Event { eventWhen = Just e, .. }
        Just w -> Event { eventWhen = Just (e `And` w), .. }

-- | To declare states `s1` of the first argument to this function (`model1`) 
-- and `s2` of the second (`model2`) shared, include in this function's `Map` 
-- (`share`) the entry `(s1, s2)`.
--
-- Before attempting to `join`, this function will suffix `model1` variables
-- that won't be affected by `join`ing with `pref1`, and likewise with `model2`.
-- 
-- All variable declarations and references from `model1` are propagated with
-- only the aforementioned suffixing. Variable declarations and references from
-- `model2` are propagated according to some rules:
--
-- > if 'v' is a state variable in 'model2':
-- >     if 'v' is a value in 'share':
-- >         replace it with its corresponding key
-- > else if 'v' is any variable in 'model2':
-- >     if 'v' is also any variable in 'model1' (post-prefixing):
-- >         replace it with a freshened version of its original name
-- > else:
-- >     no change
--
-- These rewriting rules imply that `join (singleton s1 s2) m m` will 
-- differ from `join (singleton s2 s1) m m` when `s1 /= s2`, even for the 
-- same `m`. When `s1` is supplanted by `s2` in the former case, it tells the 
-- resultant model to treat (add to/subtract from) it like it originally 
-- treated any other occurrence of `s2`. In the latter case, naturally, this is 
-- flipped. Since a model is very likely to treat different state variables 
-- differently, it follows that the unions will differ depending on which 
-- variable of a pair is overwritten.
join :: Map Text Text -> Text -> Text -> Model -> Model -> Model
join share pref1 pref2 model1 model2
  | Map.size share' /= Map.size share = undefined
  | otherwise =
    Model (modelName model1 <> "_" <> modelName model2) newDecls newEvents []
    -- TODO propagate metadata somehow?
  where
    m1DontRename = Set.fromList (Map.elems share')
    m2DontRename = Map.keysSet share'
    model1' = renameModelVarsWith (\t -> if t `Set.member` m1DontRename then t else pref1 <> t) model1
    model2' = renameModelVarsWith (\t -> if t `Set.member` m2DontRename then t else pref2 <> t) model2

    newDecls =
      modelDecls model1' ++
      renameDecls  State     nonSharedStates ++
      renameDecls  Let       (letDecls       (modelDecls model2')) ++
      renameDeclsM Parameter (parameterDecls (modelDecls model2'))

    nonSharedStates = 
      [ (v, e) 
      | (v, e) <- stateDecls (modelDecls model2')
      , v `Map.notMember` share'
      ]

    renameDecls mkDecl decls =
      [ pure $ mkDecl (renameMaybe v) (renameExprVarsWith renameMaybe e) 
      | (v, e) <- decls
      ]

    renameDeclsM mkDecl decls =
      [ pure $ mkDecl (renameMaybe v) (renameExprVarsWith renameMaybe <$> e) 
      | (v, e) <- decls
      ]

    newEvents = 
      modelEvents model1' ++ 
      map (renameEventVarsWith renameMaybe) (modelEvents model2')

    renameMaybe t =
      case share' Map.!? t of
        Just t' -> t'
        Nothing -> freshenMaybe t
      
    freshenMaybe t
      | t `elem` m1vars = freshenMaybe (t `Text.append` "_prime")
      | otherwise = t

    m1vars = modelVars model1'
    
    share' = Map.fromList [ (y, x) | (x, y) <- Map.toList share ]

-- | All variables in a model
modelVars :: Model -> Set Text
modelVars Model{..} = Set.fromList $
  map fst (stateDecls modelDecls) ++
  map fst (letDecls modelDecls) ++
  map fst (parameterDecls modelDecls) ++
  map eventName modelEvents
