{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.Compile where

import Language.ASKEE.Core   as Core
import Language.ASKEE.Syntax as Syntax

import Data.List as List ( find )
import Data.Text         ( unpack, Text )
import Data.Foldable     ( foldrM )

import Prelude hiding (GT, EQ, LT)

type Err = Either String

instance MonadFail Err where
  fail = Left

compileModel :: Syntax.Model -> Err Core.Model
compileModel m@(Syntax.Model {..}) = 
  do  (identMap, modelInitState) <- initialValues m
      modelEvents <- mapM (compileEvent identMap) modelEvents
      pure $ Core.Model {..}

initialValues :: Syntax.Model -> Err ([(Text, Core.Ident)], [(Core.Ident, Double)])
initialValues (Syntax.Model _ decls _) = 
  do  let names = map name decls
          exps  = map val decls
      results <- mapM (evalExp decls) exps
      pure $  (zip names [0..], zip [0..] results)

compileEvent :: [(Text, Core.Ident)] -> Syntax.Event -> Err Core.Event
compileEvent identMap (Syntax.Event {..}) = do
  rate <- compileExp identMap eventRate
  when <- case eventWhen of
    Just w  -> compileExp identMap w
    Nothing -> pure $ ExprNumLit 1.0 -- True
  effect <- mapM (compileStatement identMap) eventEffect
  pure $ Core.Event rate when effect

compileStatement :: [(Text, Core.Ident)] -> Syntax.Statement -> Err (Core.Ident, Expr)
compileStatement identMap (name, exp) =
  do  expr <- compileExp identMap exp
      ident <- case lookup name identMap of
        Just ident -> pure ident
        Nothing    -> fail $ "internal error: identifier "++show (unpack name)++" not found"
      pure (ident, expr)

compileExp :: [(Text, Core.Ident)] -> Exp -> Err Expr
compileExp identMap = compile
  where
    compile :: Exp -> Err Expr
    compile (Add e1 e2)   = binop ExprAdd e1 e2
    compile (Sub e1 e2)   = binop ExprSub e1 e2
    compile (Mul e1 e2)   = binop ExprMul e1 e2
    compile (Div e1 e2)   = binop ExprDiv e1 e2
    compile (Neg e1)      = ExprNeg <$> (compile e1)
    compile (GT e1 e2)    = binop ExprGT e1 e2
    compile (GTE e1 e2)   = ExprNot <$> (binop ExprLT e1 e2)
    compile (EQ e1 e2)    = binop ExprEQ e1 e2
    compile (LTE e1 e2)   = ExprNot <$> (binop ExprGT e1 e2)
    compile (LT e1 e2)    = binop ExprLT e1 e2
    compile (And e1 e2)   = binop ExprAnd e1 e2
    compile (Or e1 e2)    = binop ExprOr e1 e2
    compile (Not e1)      = ExprNot <$> (compile e1)
    compile (If e1 e2 e3) = 
      do  e1' <- compile e1
          e2' <- compile e2
          e3' <- compile e3
          pure $ ExprIf e1' e2' e3'
    compile (Cond (Condition choices Nothing)) =
      let default' = ExprDiv (ExprNumLit 1) (ExprNumLit 0)
      in  foldrM cond default' choices
    compile (Cond (Condition choices (Just defaultExp))) =
      do  default' <- compile defaultExp
          foldrM cond default' choices
    compile (Real d) = pure $ ExprNumLit d
    compile (Var t) = 
      case lookup t identMap of
        Just ident -> pure $ ExprVar ident
        Nothing    -> fail $ "compile: unbound identifier "++show (unpack t)

    binop :: (Expr -> Expr -> Expr) -> Exp -> Exp -> Err Expr
    binop op e1 e2 = do
      e1' <- compile e1
      e2' <- compile e2
      pure $ op e1' e2'

    cond :: (Exp, Exp) -> Expr -> Err Expr
    cond (result, condition) otherwise = do
      result' <- compile result
      condition' <- compile condition
      pure $ ExprIf condition' result' otherwise

-- Largely a duplication of the effort in Language.ASKEE.Core, but that
-- evaluation runs in IO
evalExp :: [Decl] -> Exp -> Err Double
evalExp decls = ev
  where
    ev :: Exp -> Err Double
    ev (Add e1 e2)   = binop (+) e1 e2
    ev (Sub e1 e2)   = binop (-) e1 e2
    ev (Mul e1 e2)   = binop (*) e1 e2
    ev (Div e1 e2)   = binop (/) e1 e2
    ev (Neg e1)      = negate <$> (ev e1)
    ev (GT e1 e2)    = cmpop (>) e1 e2
    ev (GTE e1 e2)   = cmpop (>=) e1 e2
    ev (EQ e1 e2)    = cmpop (==) e1 e2
    ev (LTE e1 e2)   = cmpop (<=) e1 e2
    ev (LT e1 e2)    = cmpop (<) e1 e2
    ev (And e1 e2)   = logop (&&) e1 e2
    ev (Or e1 e2)    = logop (||) e1 e2
    ev (Not e1)      = logop (\x _ -> not x) e1 undefined
    ev (If e1 e2 e3) = 
      do  e1' <- bool <$> ev e1
          e2' <- ev e2
          e3' <- ev e3
          pure $ if e1' then e2' else e3'
    ev (Cond (Condition choices Nothing)) =       
      let default' = 1 / 0
      in  foldrM cond default' choices
    ev (Cond (Condition choices (Just defaultExp))) = 
      do  default' <- ev defaultExp
          foldrM cond default' choices
    ev (Real d) = pure d
    ev (Var t) = case List.find (\d -> name d == t) decls of
      Just decl -> ev (val decl)
      Nothing   -> fail $ "eval: unbound identifier "++show (unpack t)

    binop :: (Double -> Double -> Double) -> Exp -> Exp -> Err Double
    binop op e1 e2 =
      do  e1' <- ev e1
          e2' <- ev e2
          pure $ op e1' e2'

    cmpop :: (Double -> Double -> Bool) -> Exp -> Exp -> Err Double
    cmpop op e1 e2 = 
      do  e1' <- ev e1
          e2' <- ev e2
          pure $ double $ op e1' e2'

    logop:: (Bool -> Bool -> Bool) -> Exp -> Exp -> Err Double
    logop op e1 e2 = 
      do  e1' <- bool <$> ev e1
          e2' <- bool <$> ev e2
          pure $ double $ op e1' e2'

    cond :: (Exp, Exp) -> Double -> Err Double
    cond (result, condition) otherwise = do
      result' <- ev result
      condition' <- bool <$> ev condition
      pure $ if condition' then result' else otherwise


bool :: Double -> Bool
bool d
  | d == 0 = False
  | d > 0 = True
  | d < 0 = undefined -- ?

double :: Bool -> Double
double True = 1.0
double False = 0.0