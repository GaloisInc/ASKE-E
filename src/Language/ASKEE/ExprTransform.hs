{-# Language PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ExprTransform where

import qualified Language.ASKEE.ESL.Syntax as Syntax
import qualified Language.ASKEE.Expr as Expr

import qualified Data.Map as Map
import           Data.Map ( Map )
import           Data.Maybe ( fromJust )
import qualified Data.Set as Set
import           Data.Set ( Set )
import           Data.Text ( Text )

import Control.Monad.Identity ( runIdentity, Identity )
import Language.ASKEE.Metadata

transformExpr ::
  Monad m =>
  (Expr.Expr -> m Expr.Expr) ->
  Expr.Expr ->
  m Expr.Expr
transformExpr exprT e =
  case e of
    Expr.Add e1 e2 -> bin Expr.Add e1 e2
    Expr.Sub e1 e2 -> bin Expr.Sub e1 e2
    Expr.Mul e1 e2 -> bin Expr.Mul e1 e2
    Expr.Div e1 e2 -> bin Expr.Div e1 e2
    Expr.Neg e1    -> (Expr.Neg <$> expr e1) >>= exprT
    Expr.Exp e1    -> (Expr.Exp <$> expr e1) >>= exprT
    Expr.Log e1    -> (Expr.Log <$> expr e1) >>= exprT
    Expr.And e1 e2 -> bin Expr.And e1 e2
    Expr.Or  e1 e2 -> bin Expr.Or e1 e2
    Expr.LT  e1 e2 -> cmp Expr.LT e1 e2
    Expr.LTE e1 e2 -> cmp Expr.LTE e1 e2
    Expr.EQ  e1 e2 -> cmp Expr.EQ e1 e2
    Expr.GTE e1 e2 -> cmp Expr.GTE e1 e2
    Expr.GT  e1 e2 -> cmp Expr.GT e1 e2
    Expr.Not e1 -> (Expr.Not <$> expr e1) >>= exprT
    Expr.Var _     -> exprT e
    Expr.LitD _    -> exprT e
    Expr.LitB _ -> exprT e
    Expr.If test thn els ->
      (Expr.If <$> expr test <*> expr thn <*> expr els) >>= exprT
    Expr.Cond choices other ->
      do  ch <- condChoice `traverse` choices
          oth <- expr `traverse` other
          exprT $ Expr.Cond ch oth
  where
    cmp op e1 e2 = (op <$> expr e1 <*> expr e2) >>= exprT
    expr = transformExpr exprT
    bin op e1 e2 = (op <$> expr e1 <*> expr e2) >>= exprT
    condChoice (ae, le) = (,) <$> expr ae <*> expr le

transformModelExprs ::
  Monad m =>
  (Expr.Expr -> m Expr.Expr) ->
  Syntax.Model ->
  m Syntax.Model
transformModelExprs exprT mdl =
  do  decls' <- transformDecl `traverse` Syntax.modelDecls mdl
      events' <- transformEvent `traverse` Syntax.modelEvents mdl
      pure $ mdl { Syntax.modelDecls = decls'
                 , Syntax.modelEvents = events'
                 }
  where
    -- foo :: MetaAnn Syntax.Decl -> m (MetaAnn Syntax.Decl)
    transformDecl (MetaAnn m (Syntax.Let n v)) = MetaAnn m . Syntax.Let n <$> expr v
    transformDecl (MetaAnn m (Syntax.State n v)) = MetaAnn m . Syntax.State n <$> expr v
    transformDecl (MetaAnn m (Syntax.Assert e)) = MetaAnn m . Syntax.Assert <$> expr e
    transformDecl (MetaAnn m (Syntax.Parameter n Nothing)) =  pure $ MetaAnn m $ Syntax.Parameter n Nothing
    transformDecl (MetaAnn m (Syntax.Parameter n (Just e))) =  expr e >>= \e' -> pure $ MetaAnn m $ Syntax.Parameter n (Just e')

    transformStmt (n, v) = (,) n <$> expr v
    transformEvent evt =
      do  when' <- expr `traverse` Syntax.eventWhen evt
          rate' <- expr (Syntax.eventRate evt)
          effect' <- transformStmt `traverse` Syntax.eventEffect evt
          pure $ evt { Syntax.eventWhen = when'
                     , Syntax.eventRate = rate'
                     , Syntax.eventEffect = effect'
                     }

    expr = transformExpr exprT


inlineLets :: Syntax.Model -> [Text] -> Syntax.Model 
inlineLets m except = 
  let (lets, _) = canonicalLets m
      lets' = foldr Map.delete lets except
  in  inlineLets' lets' m

inlineLets' :: Map Text Expr.Expr -> Syntax.Model -> Syntax.Model
inlineLets' lets m = runIdentity (transformModelExprs go m)
  where
    go :: Expr.Expr -> Identity Expr.Expr
    go e =
      case e of
        Expr.Var v ->
          case lets Map.!? v of
            Just e' -> go e'
            Nothing -> pure e
        _ -> pure e

-- | Rename variables in an expression via the provided function
renameExprVarsWith :: (Text -> Text) -> Expr.Expr -> Expr.Expr
renameExprVarsWith r e = runIdentity $ transformExpr go e
  where
    go ex =
      case ex of
        Expr.Var v -> pure $ Expr.Var (r v)
        _          -> pure ex

-- | Rename variables in an event via the provided function
--
-- Note: this will also rename variables that appear on event effect LHSs
renameEventVarsWith :: (Text -> Text) -> Syntax.Event -> Syntax.Event
renameEventVarsWith r = runIdentity . modifyEventVars (pure . r)
  where
    modifyEventVars varT evt =
      do  when'   <- expr `traverse` Syntax.eventWhen evt
          rate'   <- expr (Syntax.eventRate evt)
          effect' <- transformStmt `traverse` Syntax.eventEffect evt
          name' <- varT (Syntax.eventName evt)
          pure $ evt  { Syntax.eventWhen = when'
                      , Syntax.eventRate = rate'
                      , Syntax.eventEffect = effect'
                      , Syntax.eventName = name'
                      }
      where
        exprT e =
          case e of
            Expr.Var v -> Expr.Var <$> varT v
            _     -> pure e

        transformStmt (n, v) = 
          do  n' <- varT n
              v' <- expr v
              pure (n', v')

        expr = transformExpr exprT

canonicalLets :: Syntax.Model -> (Map Text Expr.Expr, Set Text)
canonicalLets Syntax.Model{..} = (lets, intermediates)
  where
    lets = Map.fromList [ (v, fromJust e) | (v, (_, e))  <- refMap ]
    intermediates = Set.unions [ ts | (_, (ts, _)) <- refMap ]
    refMap = [ (v, dereference letMap v) | (v, _) <- letList ]
    letList = Syntax.letDecls modelDecls
    letMap = Map.fromList letList

-- Dereference chains of e.g. { let y = 3; let x = y; let z = x; }
dereference :: Map Text Expr.Expr -> Text -> (Set Text, Maybe Expr.Expr)
dereference letMap varName = go Set.empty
  where
    -- `curr` is an accumulation of intermediate `let`s in a reference chain
    -- that can be safely removed
    go :: Set Text -> (Set Text, Maybe Expr.Expr)
    go curr = 
      case letMap Map.!? varName of
        Just (Expr.Var v) | v `Map.member` letMap -> 
          let (new, result) = dereference letMap v
          in  (Set.insert v (Set.unions [curr, new]), result)
        Just e -> (curr, Just e)
        Nothing -> (curr, Nothing)