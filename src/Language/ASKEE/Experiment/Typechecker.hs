{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Language.ASKEE.Experiment.Typechecker where

import Data.Text(Text)
import Data.Map(Map)
import Control.Monad(when)
import Data.Foldable(traverse_)
import qualified Data.Map as Map
import qualified Control.Monad.State as State
import qualified Control.Monad.Except as Except
import Control.Monad.Except(throwError)

import qualified Language.ASKEE.Experiment.Syntax as E

type TC a = State.StateT CheckEnv (Except.Except Text) a

data CheckEnv =
  CheckEnv { ceVars :: Map Text E.Type
           , ceDecls :: Map Text E.Decl
           , ceTVar :: Int
           , ceSubst :: Map Int E.Type
           , ceConstraints :: [E.TypeConstraint]
           }

-- runTC :: CheckEnv -> TC a -> Either Text a
-- runTC env tc =
--   let tcResult = State.evalState tc env
--   in Except.runExcept tcResult

requireUnboundName :: Text -> TC ()
requireUnboundName name =
  do  isBound <- State.gets isNameBound
      when isBound (throwError $ "Symbol '" <> name <> "' is already bound`")
  where
    isNameBound env =
      Map.member name (ceVars env) || Map.member name (ceDecls env)

bindVar :: Text -> E.Type -> TC ()
bindVar name ty =
  do  requireUnboundName name
      State.modify \env -> env { ceVars = Map.insert name ty (ceVars env)}

bindDecl :: Text -> E.Decl -> TC ()
bindDecl name decl =
  do  requireUnboundName (declName decl)
      State.modify \env -> env { ceDecls = Map.insert name decl (ceDecls env)}

getVarType :: Text -> TC E.Type
getVarType name =
  do  vars <- State.gets ceVars
      case Map.lookup name vars of
        Nothing -> throwError ("Variable '" <> name <> "' is not defined")
        Just ty -> pure ty

bindDecl' :: E.Decl -> TC ()
bindDecl' decl = bindDecl (declName decl) decl

newTVar :: TC E.Type
newTVar =
  do  i <- State.gets ceTVar
      State.modify \env -> env { ceTVar = i + 1}
      pure (E.TypeVar i)

addConstraint :: E.TypeConstraint -> TC ()
addConstraint c =
  State.modify \env -> env { ceConstraints = c:ceConstraints env }

zonk :: E.Type -> TC E.Type
zonk ty =
  do  subst <- State.gets ceSubst
      pure (applySubst subst ty)

unify :: E.Type -> E.Type -> TC ()
unify t1 t2 =
  do  t1' <- zonk t1
      t2' <- zonk t2
      case mgu t1' t2' of
        Nothing ->
          -- XXX: add a real error
          throwError "Type error"
        Just u ->
          State.modify
            \env -> env { ceSubst = ceSubst env `composeSubst` u }



-- scope :: TC a -> TC a
-- scope tc =
--   do  vars <- State.gets ce
--       a <- tc
--       State.modify (\env -> env { ce})


declName :: E.Decl -> Text
declName decl =
  case decl of
    E.DMeasure m -> E.tnName $ E.measureName m
    E.DExperiment e -> E.tnName $ E.experimentName e

--------------

inferLit :: E.Literal -> E.Type
inferLit l =
  case l of
    E.LitBool _ -> E.TypeBool
    E.LitNum _ -> E.TypeNumber


inferExpr :: E.Expr -> TC (E.Expr, E.Type)
inferExpr e0 =
  case e0 of
    E.Lit l -> pure (e0, inferLit l)
    E.Var v ->
      do  ty <- getVarType (E.tnName v)
          pure (E.Var v { E.tnType = Just ty }, ty)

    E.Dot e1 label ->
      do  (e1', e1ty) <- inferExpr e1
          ty <- newTVar
          addConstraint (E.HasField e1ty label ty)
          pure (E.Dot e1' label, ty)

    E.Call fname args ->
      do  argsInferred <- inferExpr `traverse` args
          let (args', argTys) = unzip argsInferred
          resultTy <- checkFunc fname argTys
          pure (E.Call fname args', resultTy)
  where
    checkFunc fname =
      case fname of
        E.Add -> arith
        E.Subtract -> arith
        E.Multiply -> arith
        E.Divide -> binary arith
        E.Negate -> unary arith
        E.Not -> unary logical
        E.LessThan -> binary comparison
        E.GreaterThan -> binary comparison
        E.LessThanEqual -> binary comparison
        E.GreaterThanEqual -> binary comparison
        E.Equal -> binary comparison    -- do we ever want to compare bools for equality?
        E.NotEqual -> binary comparison -- if so, we probably want to have a constraint instead
        E.Or -> logical
        E.And -> logical

    unary f tys =
      do  when (length tys /= 1) (throwError "[BUG] expecting only one argument to call")
          f tys

    binary f tys =
      do  when (length tys /= 2) (throwError "[BUG] expecting only two arguments to call")
          f tys

    arith tys = unifyAll E.TypeNumber tys >> pure E.TypeNumber
    logical tys = unifyAll E.TypeBool tys >> pure E.TypeBool
    comparison tys = unifyAll E.TypeNumber tys >> pure E.TypeBool

    unifyAll ty ts = unify ty `traverse_` ts


-- constraints




-- unificiation


mgu :: E.Type -> E.Type -> Maybe (Map Int E.Type)
mgu t1 t2  =
  case (t1, t2) of
    (E.TypeBool, E.TypeBool) -> ok
    (E.TypeNumber, E.TypeNumber) -> ok
    (E.TypeSequence t1', E.TypeSequence t2') ->
      mgu t1' t2'
    (E.TypeVar i, _) -> bindTVar i t2
    (_, E.TypeVar i) -> bindTVar i t1
    _ -> Nothing
  where
    ok = Just Map.empty

-- applySubst (composeSubst s1 s2) t == applySubst s2 (applySubst s1 t)
composeSubst :: Map Int E.Type -> Map Int E.Type -> Map Int E.Type
composeSubst s1 s2 =
  (applySubst s2 <$> s1) `Map.union` s2

bindTVar :: Int -> E.Type -> Maybe (Map Int E.Type)
bindTVar var ty =
  case ty of
    E.TypeVar i | var == i  -> Just Map.empty
    _ | occurs var ty       -> Nothing
      | otherwise           -> Just $ Map.singleton var ty

occurs :: Int -> E.Type -> Bool
occurs var ty =
  case ty of
    E.TypeBool -> False
    E.TypeNumber -> False
    E.TypeVar i -> var == i
    E.TypeSequence sty -> occurs var sty

applySubst :: Map Int E.Type -> E.Type -> E.Type
applySubst subst ty =
  case ty of
    E.TypeBool -> E.TypeBool
    E.TypeNumber -> E.TypeNumber
    E.TypeSequence ty' -> E.TypeSequence (applySubst subst ty')
    E.TypeVar i -> Map.findWithDefault ty i subst

