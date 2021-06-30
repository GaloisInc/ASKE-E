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
import Language.ASKEE.Metadata

data Renaming =
    Suffix Text
  | Substitute Text
  deriving (Show)

{- 
Rename every variable in every event in B:
  If the variable doesn't appear in the renaming map, just freshen
  If it does, rename it to its partner
-}

-- TODO propagate metadata somehow
-- Map values are state variables in model1
-- Map keys are state variables in model2
unionWith :: Model -> Model -> Map Text Text -> Model
unionWith model1 model2 renaming = 
  Model "foo" newDecls newEvents
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
        Nothing
          | t `elem` m2vars -> t `Text.append` "_prime"
          | otherwise -> t

    m1vars = modelVars model1
    m2vars = modelVars model2


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
renameEventVarsWith r = runIdentity . transformEventVars (pure . r)

transformEventVars ::
  Monad m =>
  (Text -> m Text) ->
  Event ->
  m Event
transformEventVars varT evt =
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