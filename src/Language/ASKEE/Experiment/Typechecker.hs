{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Language.ASKEE.Experiment.Typechecker where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad(when, unless, forM, zipWithM)
import Data.Foldable(traverse_)
import qualified Control.Monad.State as State
import qualified Control.Monad.Except as Except
import Control.Monad.Except(throwError)

import qualified Language.ASKEE.Experiment.Syntax as E
import Language.ASKEE.Experiment.TraverseType(TraverseType(traverseType), collect, mapType)

--------------

inferArgs :: [E.Binder] -> ([E.Binder] -> TC a) -> TC a
inferArgs args body =
    scope $
      (introduceArg `traverse` args) >>= body
  where
    introduceArg binder =
      do  ty <- case E.tnType binder of
                  Just t  -> pure t
                  Nothing -> newTVar
          bindVar (E.tnName binder) ty
          pure (E.setType binder ty)

inferMeasure :: E.MeasureDecl -> TC E.MeasureDecl
inferMeasure measure =
  inferArgs (E.measureArgs measure) \args ->
    do  -- vars
        let (varNames, varExprs) = unzip (E.measureVars measure)
        (varExps', varExpTys) <- unzip <$> inferExpr `traverse` varExprs
        varNames' <-
          forM (zip varNames varExpTys) \(name, ty) ->
            do  bindVar (E.tnName name) ty
                setModifiableSymbol (E.tnName name)
                pure (E.setType name ty)

        let outTy = E.TypePoint
                  $ Map.fromList [ (E.tnName x, E.getType x) | x <- varNames' ]
            newName = E.setType (E.measureName measure) outTy

        -- binder
        binderTy <- newTVar
        let dataBinder' = E.setType (E.measureDataBinder measure) binderTy
        bindVar (E.tnName dataBinder') binderTy

        -- impl
        block' <- inferBlock (E.measureImpl measure)

        m' <- zonk E.MeasureDecl { E.measureName = newName
                                 , E.measureTArgs = []
                                 , E.measureConstraints = []
                                 , E.measureArgs = args
                                 , E.measureVars = zip varNames' varExps'
                                 , E.measureDataBinder = dataBinder'
                                 , E.measureImpl = block'
                                 }

        let freeVars = Set.toList $ freeTVars m'
            bv = E.TVBound <$> freeVars
            subst = Map.fromList (zip (E.TVFree <$> freeVars) (E.TypeVar <$> bv))

        cns <- extractConstraints

        pure $ applySubst subst m' { E.measureTArgs       = freeVars
                                   , E.measureConstraints = cns
                                   }

inferMeasureExpr :: E.MeasureExpr -> TC (E.Type, E.MeasureExpr)
inferMeasureExpr mexpr =
  do mty <- lookupMeasure (E.tnName (E.meMeasureName mexpr))
     undefined

checkCall :: [E.Expr] -> [E.Type] -> TC [E.Expr]
checkCall es ts =
  case compare (length es) (length ts) of
    EQ -> zipWithM checkExpr es ts
    LT -> throwError "Not enough arguments"
    GT -> throwError "Too many arguments"


inferLit :: E.Literal -> E.Type
inferLit l =
  case l of
    E.LitBool _ -> E.TypeBool
    E.LitNum _ -> E.TypeNumber

inferBlock :: [E.Stmt] -> TC [E.Stmt]
inferBlock = scope . traverse inferStmt

inferStmt :: E.Stmt -> TC E.Stmt
inferStmt s0 =
  case s0 of
    E.Set ident e ->
      do  let name = E.tnName ident
          varTy <- getVarType name
          checkModifiableSymbol name
          (e', eTy) <- inferExpr e
          unify varTy eTy
          pure $ E.Set (E.setType ident varTy) e'
    E.Let binder e ->
      do  let name = E.tnName binder
          (e', eTy) <- inferExpr e
          bindVar name eTy
          pure $ E.Let (E.setType binder eTy) e'
    E.If thens els ->
      E.If <$> traverse checkThen thens <*> inferBlock els
  where
    checkThen (test, stmts) =
      do  (test', tty) <- inferExpr test
          unify E.TypeBool tty
          stmts' <- inferBlock stmts
          pure (test', stmts')


checkExpr :: E.Expr -> E.Type -> TC E.Expr
checkExpr e t =
  do (e',t') <- inferExpr e
     unify t t'
     pure e'

inferExpr :: E.Expr -> TC (E.Expr, E.Type)
inferExpr e0 =
  case e0 of
    E.Lit l -> pure (e0, inferLit l)
    E.Var v ->
      do  ty <- getVarType (E.tnName v)
          pure (E.Var (E.setType v ty), ty)

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






--------------------------------------------------------------------------------
-- The typechecking monad

type TC a = State.StateT CheckEnv (Except.Except Text) a

data CheckEnv =
  CheckEnv { ceVars :: Map Text E.Type
           , ceDecls :: Map Text E.Decl
           , ceMutable :: Set Text
           , ceTVar :: Int
           , ceSubst :: Subst
           , ceConstraints :: [E.TypeConstraint]
           }

emptyCheckEnv :: CheckEnv
emptyCheckEnv =
  CheckEnv
    { ceVars        = mempty
    , ceDecls       = mempty
    , ceMutable     = mempty
    , ceTVar        = 1
    , ceSubst       = mempty
    , ceConstraints = mempty
    }


runTC :: TraverseType a => TC a -> Either Text a
runTC tc = Except.runExcept $ State.evalStateT (tc >>= zonk) env
  where
  env = emptyCheckEnv

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

lookupMeasure :: Text -> TC E.MeasureType
lookupMeasure name =
  do mb <- State.gets (Map.lookup name . ceDecls)
     case mb of
       Nothing -> throwError $ "Undefined measure '" <> name <> "'"
       Just decl  ->
         case decl of
           E.DExperiment {} ->
              throwError $ "'" <> name <> "' is an experiment, not a measure."
           E.DMeasure m -> instantiate (E.measureType m)

declName :: E.Decl -> Text
declName decl =
  case decl of
    E.DMeasure m -> E.tnName $ E.measureName m
    E.DExperiment e -> E.tnName $ E.experimentName e

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
      pure (E.TypeVar (E.TVFree i))

addConstraint :: E.TypeConstraint -> TC ()
addConstraint c =
  State.modify \env -> env { ceConstraints = c:ceConstraints env }

extractConstraints :: TC [E.TypeConstraint]
extractConstraints =
  do  cs <- State.gets ceConstraints
      State.modify \env -> env { ceConstraints = [] }
      zonk cs

checkModifiableSymbol :: Text -> TC ()
checkModifiableSymbol name =
  do  isMod <- State.gets \env -> name `Set.member` ceMutable env
      unless isMod (throwError $ "'" <> name <> "' is immutable")

setModifiableSymbol :: Text -> TC ()
setModifiableSymbol name =
  State.modify \env ->
                env { ceMutable = Set.insert name (ceMutable env) }

zonk :: TraverseType t => t -> TC t
zonk = traverseType \ty -> do subst <- State.gets ceSubst
                              pure (applySubst subst ty)

unify :: E.Type -> E.Type -> TC ()
unify t1 t2 =
  do  t1' <- zonk t1
      t2' <- zonk t2
      case mgu t1' t2' of
        Left err -> throwError err
        Right u ->
          State.modify
            \env -> env { ceSubst = ceSubst env `composeSubst` u }


scope :: TC a -> TC a
scope tc =
  do  env0 <- State.get
      a <- tc
      State.modify \env -> env { ceVars = ceVars env0
                               , ceMutable = ceMutable env0
                               }
      pure a

instantiate :: TraverseType t => E.Qualiefied t -> TC t
instantiate (E.Forall xs cs t) =
  do subst <- Map.fromList <$> traverse freshVar xs
     traverse_ addConstraint (applySubst subst cs)
     pure (applySubst subst t)
  where
  freshVar x =
    do ty <- newTVar
       pure (E.TVBound x, ty)




--------------------------------------------------------------------------------
-- Substitution and Unifiers

type TypeError = Text

mgu :: E.Type -> E.Type -> Either TypeError Subst
mgu t1 t2  =
  case (t1, t2) of
    (E.TypeBool, E.TypeBool) -> ok
    (E.TypeNumber, E.TypeNumber) -> ok
    (E.TypeSequence t1', E.TypeSequence t2') -> mgu t1' t2'
    (E.TypePoint mp1, E.TypePoint mp2) -> mguMap mp1 mp2
    (E.TypeVar i@(E.TVFree _), _) -> bindTVar i t2
    (_, E.TypeVar i@(E.TVFree _)) -> bindTVar i t1
    _ -> Left "Types differ"
  where
    ok = pure Map.empty

mguMap :: Map Text E.Type -> Map Text E.Type -> Either TypeError Subst
mguMap mp1 mp2 = mguFields (Map.toList mp1) (Map.toList mp2)
  where
  mguFields xs ys =
    case (xs,ys) of
      ([],[]) -> pure mempty
      ((f1,t1) : xs', (f2,t2) : ys') ->
        case compare f1 f2 of
          EQ -> do su1 <- mgu t1 t2
                   let upd (f,t) = (f, applySubst su1 t)
                   su2 <- mguFields (map upd xs') (map upd ys')
                   pure (composeSubst su1 su2)
          LT -> missingField f1
          GT -> missingField f2

      ((f,_):_, []     ) -> missingField f
      ([],      (f,_):_) -> missingField f

  missingField f = Left ("Missing field '" <> f <> "'")

type Subst = Map E.TypeVar E.Type

-- applySubst (composeSubst s1 s2) t == applySubst s2 (applySubst s1 t)
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 =
  (applySubst s2 <$> s1) `Map.union` s2

bindTVar :: E.TypeVar -> E.Type -> Either TypeError Subst
bindTVar var ty =
  case ty of
    E.TypeVar i | var == i  -> pure Map.empty
    _ | occurs var ty       -> Left "Recursive type"
      | otherwise           -> pure (Map.singleton var ty)

occurs :: E.TypeVar -> E.Type -> Bool
occurs var ty =
  case ty of
    E.TypeBool -> False
    E.TypeNumber -> False
    E.TypeVar i -> var == i
    E.TypeSequence sty -> occurs var sty
    E.TypePoint mp -> any (occurs var) mp

applySubst :: TraverseType t => Subst -> t -> t
applySubst subst = mapType doSubst
  where
  doSubst ty =
    case ty of
      E.TypeBool          -> E.TypeBool
      E.TypeNumber        -> E.TypeNumber
      E.TypeSequence ty'  -> E.TypeSequence (doSubst ty')
      E.TypePoint mp      -> E.TypePoint (doSubst <$> mp)
      E.TypeVar i         -> Map.findWithDefault ty i subst


freeTVars :: TraverseType t => t -> Set Int
freeTVars = collect freeVarsInType
  where
    freeVarsInType t0 =
      case t0 of
        E.TypeBool -> Set.empty
        E.TypeNumber -> Set.empty
        E.TypeSequence t -> freeVarsInType t
        E.TypePoint mp -> foldMap freeVarsInType mp
        E.TypeVar (E.TVFree i) -> Set.singleton i
        E.TypeVar (E.TVBound _) -> Set.empty
