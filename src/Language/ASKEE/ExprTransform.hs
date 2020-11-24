{-# Language PatternSynonyms #-}
module Language.ASKEE.ExprTransform where

import qualified Language.ASKEE.Syntax as Syntax
import qualified Language.ASKEE.Expr as Expr


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
      (Expr.If <$> expr test <*> expr thn <*> expr els) >>= expr
    Expr.Cond choices other ->
      do  choices <- condChoice `traverse` choices
          oth <- expr `traverse` other
          exprT $ Expr.Cond choices oth
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
  do  decls' <- transformDecl `traverse` (Syntax.modelDecls mdl)
      events' <- transformEvent `traverse` (Syntax.modelEvents mdl)
      pure $ mdl { Syntax.modelDecls = decls'
                 , Syntax.modelEvents = events'
                 }
  where
    transformDecl (Syntax.Let n v) = Syntax.Let n <$> expr v
    transformDecl (Syntax.State n v) = Syntax.State n <$> expr v
    transformDecl (Syntax.Assert e) = Syntax.Assert <$> expr e

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




