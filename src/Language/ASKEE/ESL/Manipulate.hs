{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ESL.Manipulate where

import           Data.Map  ( Map )
import qualified Data.Map  as Map
import           Data.Set  ( Set )
import qualified Data.Set  as Set
import           Data.Text ( Text )
import qualified Data.Text as Text

import Language.ASKEE.ESL.Syntax
import Language.ASKEE.Expr
import Language.ASKEE.ExprTransform
import Control.Monad.Identity

composeSerial :: Map Text Text -> Model -> Model -> Expr -> Expr -> Model
composeSerial stateShare m1 m2 stop1 start2 = unionWith stateShare m1' m2'
  where
    m1' = m1 { modelEvents = map (doWhen (Not stop1)) (modelEvents m1) }
    m2' = m2 { modelEvents = map (doWhen start2) (modelEvents m2) }

    doWhen e Event{..} =
      case eventWhen of
        Nothing -> Event { eventWhen = Just e, .. }
        Just w -> Event { eventWhen = Just (e `And` w), .. }

-- | To declare states `s1` of the first argument to this function (`model1`) 
-- and `s2` of the second (`model2`) shared, include in this function's `Map` 
-- (`renaming`) the entry `(s2, s1)`.
--
-- All variable declarations and references from `model1` are propagated with
-- no changes. Variable declarations and references from `model2` are
-- propagated according to some rules:
--
-- > if 'v' is a state variable in 'model2':
-- >     if 'v' is in 'renaming':
-- >         replace it with its corresponding value
-- > else if 'v' is any variable in 'model2':
-- >     if 'v' is also any variable in 'model1':
-- >         replace it with a freshened version of its original name
-- > else:
-- >     no change
--
-- These rewriting rules imply that `unionWith (singleton s1 s2) m m` will 
-- differ from `unionWith (singleton s2 s1) m m` when `s1 /= s2`, even for the 
-- same `m`. When `s1` is supplanted by `s2` in the former case, it tells the 
-- resultant model to treat (add to/subtract from) it like it originally 
-- treated any other occurrence of `s2`. In the latter case, naturally, this is 
-- flipped. Since a model is very likely to treat different state variables 
-- differently, it follows that the unions will differ depending on which 
-- variable of a pair is overwritten.
unionWith :: Map Text Text -> Model -> Model -> Model
unionWith renaming model1 model2 = 
  Model (modelName model1 <> "_" <> modelName model2) newDecls newEvents
  -- TODO propagate metadata somehow?
  where
    newDecls =
      modelDecls model1 ++
      renameDecls  State     nonSharedStates ++
      renameDecls  Let       (letDecls       (modelDecls model2)) ++
      renameDeclsM Parameter (parameterDecls (modelDecls model2))

    nonSharedStates = 
      [ (v, e) 
      | (v, e) <- stateDecls (modelDecls model2)
      , v `Map.notMember` renaming 
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
      modelEvents model1 ++ 
      map (renameEventVarsWith renameMaybe) (modelEvents model2)

    renameMaybe t =
      case renaming Map.!? t of
        Just t' -> t'
        Nothing -> freshenMaybe t
      
    freshenMaybe t
      | t `elem` m1vars = freshenMaybe (t `Text.append` "_prime")
      | otherwise = t

    m1vars = modelVars model1


modelVars :: Model -> Set Text
modelVars Model{..} = Set.fromList $
  map fst (stateDecls modelDecls) ++
  map fst (letDecls modelDecls) ++
  map fst (parameterDecls modelDecls) ++
  map eventName modelEvents

renameExprVarsWith :: (Text -> Text) -> Expr -> Expr
renameExprVarsWith r e = runIdentity $ transformExpr go e
  where
    go ex =
      case ex of
        Var v -> pure $ Var (r v)
        _ -> pure ex

renameEventVarsWith :: (Text -> Text) -> Event -> Event
renameEventVarsWith r = runIdentity . modifyEventVars (pure . r)
  where
    modifyEventVars varT evt =
      do  when'   <- expr `traverse` eventWhen evt
          rate'   <- expr (eventRate evt)
          effect' <- transformStmt `traverse` eventEffect evt
          name' <- varT (eventName evt)
          pure $ evt  { eventWhen = when'
                      , eventRate = rate'
                      , eventEffect = effect'
                      , eventName = name'
                      }
      where
        exprT e =
          case e of
            Var v -> Var <$> varT v
            _     -> pure e

        transformStmt (n, v) = 
          do  n' <- varT n
              v' <- expr v
              pure (n', v')

        expr = transformExpr exprT