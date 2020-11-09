{- LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE FlexibleInstances #-}
{- LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Compile where

import Language.ASKEE.Core   as Core
import Language.ASKEE.Expr   as Expr
import Language.ASKEE.Syntax as Syntax

import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Text         ( unpack, Text )
import Data.Foldable     ( foldrM )

type ModelGen a = Either String a

compileModel :: Syntax.Model -> ModelGen Core.Model
compileModel m@(Syntax.Model {..}) = 
  do  (identMap, modelInitState) <- initialValues m
      modelEvents <- mapM (compileEvent identMap) modelEvents
      pure $ Core.Model {..}


initialValues :: Syntax.Model -> ModelGen ([(Text, Core.Ident)], [(Core.Ident, Double)])
initialValues (Syntax.Model _ decls _) = 
  do  let names = "time"                        : map fst (varDecls decls)
          exps  = (Syntax.ArithExpr (ALit 0.0)) : map snd (varDecls decls)
          vars = Map.fromList (zip names exps)
      vals <- mapM (evalExp vars) exps
      pure $ (zip names names, zip names vals) --TODO: kinda sketchy


compileEvent :: [(Text, Core.Ident)] -> Syntax.Event -> ModelGen Core.Event
compileEvent identMap (Syntax.Event {..}) = do
  rate <- compileExp identMap eventRate
  when <- case eventWhen of
    Just w  -> compileLog identMap w
    Nothing -> pure $ ExprNumLit 1.0 -- True
  effect <- mapM (compileStatement identMap) eventEffect
  pure $ Core.Event eventName rate when effect


compileStatement :: [(Text, Core.Ident)] -> Syntax.Statement -> ModelGen (Core.Ident, Core.Expr)
compileStatement identMap (name, exp) =
  do  expr <- compileArith identMap exp
      ident <- case lookup name identMap of
        Just ident -> pure ident
        Nothing    -> fail $ "Compile: compileStatement: internal error: unbound identifier "<>unpack name
      pure (ident, expr)


compileExp :: [(Text, Core.Ident)] -> Syntax.ModelExpr -> ModelGen Core.Expr
compileExp identMap (ArithExpr e) = compileArith identMap e
compileExp identMap (IfExpr e1 e2 e3) = compileIf identMap e1 e2 e3
compileExp identMap (CondExpr (Condition choices other)) = compileCond identMap choices other

compileArith :: [(Text, Core.Ident)] -> Expr.ArithExpr -> ModelGen Core.Expr
compileArith identMap = go
  where
    go :: Expr.ArithExpr -> ModelGen Core.Expr
    go (Expr.Add e1 e2) = binop go Core.ExprAdd e1 e2
    go (Expr.Sub e1 e2) = binop go Core.ExprSub e1 e2
    go (Expr.Mul e1 e2) = binop go Core.ExprMul e1 e2
    go (Expr.Div e1 e2) = binop go Core.ExprDiv e1 e2
    go (Expr.Neg e1) = Core.ExprNeg <$> (go e1)
    go (ALit d) = pure $ Core.ExprNumLit d
    go (Var t) = 
      case lookup t identMap of
        Just ident -> pure $ Core.ExprVar ident
        Nothing    -> fail $ "Compile: compileArith: internal error: unbound identifier "<>unpack t

compileLog :: [(Text, Core.Ident)] -> Expr.LogExpr -> ModelGen Core.Expr
compileLog identMap = goL
  where
    goL :: Expr.LogExpr -> ModelGen Core.Expr
    goL (Expr.GT e1 e2)  = binop goA Core.ExprGT e1 e2
    goL (Expr.GTE e1 e2) = Core.ExprNot <$> (binop goA Core.ExprLT e1 e2)
    goL (Expr.EQ e1 e2)  = binop goA Core.ExprEQ e1 e2
    goL (Expr.LTE e1 e2) = Core.ExprNot <$> (binop goA Core.ExprGT e1 e2)
    goL (Expr.LT e1 e2)  = binop goA Core.ExprLT e1 e2
    goL (Expr.And e1 e2) = binop goL Core.ExprAnd e1 e2
    goL (Expr.Or e1 e2)  = binop goL Core.ExprOr e1 e2
    goL (Expr.Not e1)    = Core.ExprNot <$> (goL e1)

    goA :: Expr.ArithExpr -> ModelGen Core.Expr
    goA = compileArith identMap

binop :: 
  (e -> ModelGen Core.Expr) ->
  (Core.Expr -> Core.Expr -> Core.Expr) ->
  e ->
  e ->
  ModelGen Core.Expr
binop comp op e1 e2 = op <$> comp e1 <*> comp e2

compileIf :: 
  [(Text, Core.Ident)] ->
  Expr.LogExpr ->
  Syntax.ModelExpr ->
  Syntax.ModelExpr ->
  ModelGen Core.Expr
compileIf identMap e1 e2 e3 = 
  Core.ExprIf <$> 
  compileLog identMap e1 <*>
  compileExp identMap e2 <*> 
  compileExp identMap e3

compileCond :: [(Text, Core.Ident)] -> [(ArithExpr, LogExpr)] -> Maybe ArithExpr -> ModelGen Core.Expr
compileCond identMap choices otherM =
  case otherM of
    Nothing -> 
      let default' = Core.ExprDiv (Core.ExprNumLit 1) (Core.ExprNumLit 0)
      in  foldrM cond default' choices
    Just other ->
      do  default' <- compileArith identMap other
          foldrM cond default' choices

  where
    cond :: (ArithExpr, LogExpr) -> Core.Expr -> ModelGen Core.Expr
    cond (result, condition) otherwise = do
      resultExpr <- compileArith identMap result
      conditionExpr <- compileLog identMap condition
      pure $ Core.ExprIf conditionExpr resultExpr otherwise


evalExp :: Map Text Syntax.ModelExpr -> Syntax.ModelExpr -> Either String Double
evalExp vars = go
  where
    go (Syntax.ArithExpr e) = Expr.evalArith vars' e
    go (Syntax.IfExpr e1 e2 e3) = 
      do  cond <- Expr.evalLog vars' e1
          if cond
            then go e2
            else go e3
    go (Syntax.CondExpr (Syntax.Condition choices otherM)) = undefined

    vars' = Map.mapMaybe arith vars

    arith :: Syntax.ModelExpr -> Maybe Expr.ArithExpr
    arith (Syntax.ArithExpr ae) = Just ae
    arith _                     = Nothing