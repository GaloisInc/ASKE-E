{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
module Language.ASKEE.Expo.Typechecker where

import Data.Text(Text, pack)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Monad(when, unless, forM, zipWithM, void)
import Data.Foldable(traverse_)
import qualified Control.Monad.State as State
import qualified Control.Monad.Except as Except
import Control.Monad.Except(throwError)

import qualified Language.ASKEE.Expo.Syntax as E
import Language.ASKEE.Expo.TypeOf(typeOf)
import Language.ASKEE.Expo.TraverseType(TraverseType(traverseType), collect, mapType)
import Language.ASKEE.Panic (panic)
import Debug.Trace (traceShowId)

--------------

inferDecls :: [E.Decl] -> TC [E.Decl]
inferDecls = traverse inferDecl

inferDecl :: E.Decl -> TC E.Decl
inferDecl decl =
  do  decl' <- case decl of
        E.DMeasure m -> E.DMeasure <$> inferMeasure m
        -- E.DExperiment e -> E.DExperiment <$> inferExperiment e
        E.DModel m -> E.DModel <$> inferModel m
        E.DMain m -> E.DMain <$> inferMain m

      bindDecl' decl'
      pure decl'

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

inferMain :: E.MainDecl -> TC E.MainDecl
inferMain mn = 
  scope $
    do  mainStmts' <- inferStmt `traverse` E.mainStmts mn
        void $ extractConstraints >>= solveConstraints
        mainOutput' <- inferExpr `traverse` E.mainOutput mn
        pure E.MainDecl { E.mainStmts  = mainStmts'
                        , E.mainOutput = fst <$> mainOutput' }

  -- where
  --   inferMainStmt :: E.Stmt -> TC E.Stmt
  --   inferMainStmt ms =
  --     case ms of
  --       _ -> undefined
        -- E.MSSample bind num ident es -> 
        --   do  ty <- E.TypeVector <$>
        --               case E.tnType ident of
        --                 Nothing -> E.etResult <$> lookupSampleSource (E.tnName ident)
        --                 Just t -> pure t
        --       let bind' = E.setType bind ty
        --       bindVar (E.tnName bind) ty
        --       -- (es', tys) <- unzip <$> inferExpr `traverse` es
        --       unless (null es) (panic "inferMainStmt" ["you've called an experiment with arguments, you fool!"])
        --       pure $ E.MSSample bind' num ident es

inferModel :: E.ModelDecl -> TC E.ModelDecl
inferModel mdl =
  do  let con = 
            E.TCon { E.tconName   = E.tnName (E.mdName mdl)
                   , E.tconArgs   = []
                   , E.tconFields = E.mdFields mdl
                   }
          ty = E.TypeStream (E.TypeRandomVar (E.TypeCon con))
      
      pure mdl { E.mdName = E.setType (E.mdName mdl) ty }

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

        case E.measureFinal measure of
          Nothing -> pure ()
          Just _  -> throwError "'final' not supported at present"

{-
        (fexpr, outTy) <- case E.measureFinal measure of
          Nothing ->
            let ty = E.TypePoint $ Map.fromList [ (E.tnName x, E.getType x) | x <- varNames' ]
            in pure (Nothing, ty)

          Just finalizer ->
            do  (expr', fty) <- inferExpr finalizer
                pure (Just expr', fty)
-}



        -- binder
        binderTy <- newTVar
        let dataBinder' = E.setType (E.measureDataBinder measure) binderTy
        bindVar (E.tnName dataBinder') binderTy

        -- impl
        block' <- inferBlock (E.measureImpl measure)

        m' <- zonk E.MeasureDecl { E.measureName = E.measureName measure
                                 , E.measureTArgs = []
                                 , E.measureConstraints = []
                                 , E.measureArgs = args
                                 , E.measureVars = zip varNames' varExps'
                                 , E.measureDataBinder = dataBinder'
                                 , E.measureImpl = block'
                                 , E.measureFinal = Nothing -- XXX fexpr
                                 }

        let freeVars = Set.toList $ freeTVars m'
            bv = E.TVBound <$> freeVars
            subst = Map.fromList (zip (E.TVFree <$> freeVars) (E.TypeVar <$> bv))

        outTy <- zonk $ 
          E.TypeRandomVar $
            E.TypeCon
              E.TCon  
                { E.tconName = E.tnName (E.measureName measure)
                , E.tconArgs = E.TypeVar <$> bv
                , E.tconFields = Map.fromList
                  [ (E.tnName x, E.getType x) | x <- varNames' ]
                }

        let pname = E.setType (E.measureName measure) outTy

        cns <- extractConstraints

        pure $ applySubst subst m' { E.measureName        = pname
                                   , E.measureTArgs       = freeVars
                                   , E.measureConstraints = cns
                                   }



-- inferMeasureExpr :: E.MeasureExpr -> TC (E.MeasureExpr, E.Type)
-- inferMeasureExpr mexpr =
--   do let name = E.tnName (E.meMeasureName mexpr)
--      (mty, tyArgs) <- lookupMeasure name
--      es  <- checkCall name (E.meArgs mexpr) (E.mtArgs mty)
--      dsTy <- getVarType (E.tnName (E.meDataset mexpr))
--      unify (E.TypeVector (E.mtData mty)) dsTy

--      let r = E.mtResult mty

--      pure ( E.MeasureExpr { E.meMeasureName = E.setType (E.meMeasureName mexpr) r
--                           , E.meDataset = E.setType (E.meDataset mexpr) dsTy
--                           , E.meArgs = es
--                           , E.meTypeArgs = tyArgs
--                           }
--           , r
--           )

-- inferSampleExpr :: E.SampleExpr -> TC (E.SampleExpr, E.Type)
-- inferSampleExpr sexpr =
--   do  let name = E.tnName (E.seName sexpr)
--       ety <- lookupSampleSource name
--       seRange' <- checkExpr (E.seRange sexpr) (E.TypeStream E.TypeNumber)
--           -- XXX: perhaps Stream should be a constraint instead of a type

--       seArgs' <- checkCall name (E.seArgs sexpr) (E.etArgs ety)

--       pure ( E.SampleExpr { E.seName = E.seName sexpr
--                           , E.seArgs = seArgs'
--                           , E.seRange = seRange'
--                           }
--            , E.TypeStream (E.etResult ety)
--            )

-- inferExperimentStmt :: E.ExperimentStmt -> TC E.ExperimentStmt
-- inferExperimentStmt stmt =
--   case stmt of
--     E.ESLet name e ->
--       do  (e', ty) <- inferExpr e
--           bindVar (E.tnName name) ty
--           pure $ E.ESLet (E.setType name ty) e'

--     E.ESMeasure name m ->
--       do  (m', ty) <- inferMeasureExpr m
--           bindVar (E.tnName name) ty
--           pure $ E.ESMeasure (E.setType name ty) m'

--     E.ESSample name se ->
--       do  (se', ty) <- inferSampleExpr se
--           bindVar (E.tnName name) ty
--           pure $ E.ESSample (E.setType name ty) se'

--     E.ESTrace name e ->
--       do  (e', ty) <- inferExpr e
--           var <- newTVar
--           unify ty (E.TypeStream var)
--           let vty = E.TypeVector var
--           pure $ E.ESTrace (E.setType name vty) e'


-- inferExperiment :: E.ExperimentDecl -> TC E.ExperimentDecl
-- inferExperiment ex =
--   inferArgs (E.experimentArgs ex) \args' ->
--     do  stmts' <- inferExperimentStmt `traverse` E.experimentStmts ex

-- {-
--         -- XXX: maybe add constraint to rty to ensure the type is 'simple'
--         (returnExp', rty) <- inferExpr (E.experimentReturn ex)
-- -}

--         cns <- extractConstraints >>= solveConstraints

--         let rty = E.TypeCon
--                   E.TCon { E.tconName = E.tnName (E.experimentName ex)
--                          , E.tconArgs = []
--                          , E.tconFields = Map.fromList
--                                                   (concatMap stmtToField stmts')
--                          }

--         let exName = E.experimentName ex
--         let ex' = E.ExperimentDecl { E.experimentName = E.setType exName rty
--                                    , E.experimentArgs = args'
--                                    , E.experimentStmts = stmts'
--                                    -- , E.experimentReturn = returnExp'
--                                    }

--         ex'' <- zonk ex'
--         let tvars = freeTVars ex''

--         -- XXX: errors are bad
--         unless (Set.null tvars) (throwError "experiment is polymorphic")
--         unless (null cns) (throwError "could not solve all constraints in experiment")

--         pure ex''

--   where
--   stmtToField s =
--     case s of
--       E.ESLet x e     -> [ (E.tnName x, typeOf e) ]
--       E.ESSample {}   -> []
--       E.ESMeasure x m -> [ (E.tnName x, E.getType (E.meMeasureName m)) ]
--       E.ESTrace x _   -> [ (E.tnName x, E.getType x) ]

checkCall :: Text -> [E.Expr] -> [E.Type] -> TC [E.Expr]
checkCall thing es ts =
  case compare have need of
    EQ -> zipWithM checkExpr es ts
    LT -> throwError ("Not enough arguments in call to " <> thing)
    GT -> throwError ("Too many arguments in call to " <> thing)
  where
  have = length es
  need = length ts


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

    E.Dot e1 label _ ->
      do  (e1', e1ty) <- inferExpr e1
          ty <- newTVar
          addConstraint (E.HasField e1ty label ty)
          pure (E.Dot e1' label (Just ty), ty)

    E.Call fname args ->
      do  argsInferred <- inferExpr `traverse` args
          let (args', argTys) = unzip argsInferred
          resultTy <- checkFunc fname argTys
          pure (E.Call fname args', resultTy)

    E.Point pm ->
      do  let fields = fst <$> pm
          case [f | f:_:_ <- List.group (List.sort fields)] of
            [] -> pure ()
            duplicateField:_ -> throwError $ "field '" <> duplicateField
                                             <> "' is duplicated in point constructor"

          (fValues, fTys) <- unzip <$> (checkPointField `traverse` pm)
          pure (E.Point fValues, E.TypePoint (Map.fromList fTys))
-- inferMeasureExpr :: E.MeasureExpr -> TC (E.MeasureExpr, E.Type)
-- inferMeasureExpr mexpr =
--   do let name = E.tnName (E.meMeasureName mexpr)
--      (mty, tyArgs) <- lookupMeasure name
--      es  <- checkCall name (E.meArgs mexpr) (E.mtArgs mty)
--      dsTy <- getVarType (E.tnName (E.meDataset mexpr))
--      unify (E.TypeVector (E.mtData mty)) dsTy

--      let r = E.mtResult mty

--      pure ( E.MeasureExpr { E.meMeasureName = E.setType (E.meMeasureName mexpr) r
--                           , E.meDataset = E.setType (E.meDataset mexpr) dsTy
--                           , E.meArgs = es
--                           , E.meTypeArgs = tyArgs
--                           }
--           , r
--           )
    E.Measure measuredThing measuringTool measuringArgs _tyArgs ->
      do  (measuredThing', measuredThingTy) <- inferExpr measuredThing

          let name = E.tnName measuringTool

          (measuringTy, measuringTyArgs) <- lookupMeasure name
          unify (E.TypeStream (E.TypeRandomVar (E.mtData measuringTy))) measuredThingTy

          measuringArgs' <- checkCall name measuringArgs (E.mtArgs measuringTy)

          zonk
            ( E.Measure
                { E.measuredThing = measuredThing'
                , E.measuringTool = E.setType measuringTool (E.mtResult measuringTy)
                , E.measuringArgs = measuringArgs'
                , E.measuringArgTys = measuringTyArgs
                 }
            , E.mtResult measuringTy
            )

    E.At slicedThing slicer ->
      do  (slicedThing', slicedThingTy) <- inferExpr slicedThing
          pointTy <- newTVar
          unify (E.TypeStream (E.TypeRandomVar pointTy)) slicedThingTy

          (slicer', slicerTy) <- inferExpr slicer
          addConstraint (E.IsTimeLike slicerTy)
          pure (E.At slicedThing' slicer', slicedThingTy)

    E.Sample sampleNum sampledThing ->
      do  (sampledThing', sampledThingTy) <- inferExpr sampledThing
          pointTy <- newTVar
          unify (E.TypeRandomVar pointTy) sampledThingTy
          pure (E.Sample sampleNum sampledThing', E.TypeVector pointTy)


  where
    checkPointField (n, v) =
      do  (v', ty) <- inferExpr v
          pure ((n, v'), (n, ty))

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
        E.Range ->
          \argTys -> do  when (length argTys /= 3) (throwError "[BUG] expecting exactly 3 args for range")
                         unifyAll E.TypeNumber argTys
                         pure $ E.TypeVector E.TypeNumber

    unary f tys =
      do  when (length tys /= 1) (throwError "[BUG] expecting only one argument to call")
          f tys

    binary f tys =
      do  when (length tys /= 2) (throwError "[BUG] expecting only two arguments to call")
          f tys

    arith tys = unifyAll E.TypeNumber tys >> pure (head tys) --E.TypeNumber
    logical tys = unifyAll E.TypeBool tys >> pure (head tys) --E.TypeBool
    comparison tys = unifyAll E.TypeNumber tys >> pure E.TypeBool -- XXX ???

    unifyAll ty ts = permissiveUnify ty `traverse_` ts

-- stripRandom :: E.Type -> TC E.Type
-- stripRandom = traverseType go
--   where
--     go :: E.Type -> TC E.Type
--     go ty =
--       case ty of
--         E.TypeRandomVar t -> pure t
--         E.TypeBool -> pure ty
--         E.TypeNumber -> pure ty
--         (E.TypeVector t) -> E.TypeVector <$> go t
--         (E.TypePoint mp) -> undefined
--         (E.TypeCon tc) -> undefined
--         (E.TypeVar tv) -> undefined
--         (E.TypeMeasurable t) -> E.TypeMeasurable <$> go t


-- isMeasurable :: E.Type -> Bool
-- isMeasurable ty =
--   case ty of
--     E.TypeMeasurable _ -> True
--     E.TypeRandomVar (E.TypeMeasurable _) -> True
--     _ -> False

-- isRandom :: E.Type -> Bool
-- isRandom ty =
--   case ty of
--     E.TypeRandomVar _ -> True
--     E.TypeMeasurable (E.TypeRandomVar _) -> True
--     _ -> False



--------------------------------------------------------------------------------
-- The typechecking monad

type TC a = State.StateT CheckEnv (Except.Except Text) a

data DeclType =
    DTMeasure (E.Qualified E.MeasureType)
  -- | DTExperiment E.ExperimentType
  | DTModel E.ModelType
  deriving Show

data CheckEnv =
  CheckEnv { ceVars :: Map Text E.Type
           , ceDecls :: Map Text DeclType
           , ceMutable :: Set Text
           , ceTVar :: Int
           , ceSubst :: Subst
           , ceConstraints :: [E.TypeConstraint]
           }
           deriving Show

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


runTC :: TraverseType a => TC a -> Either Text (a, CheckEnv)
runTC tc = Except.runExcept $ State.runStateT (tc >>= zonk) env
  where
  env = emptyCheckEnv

requireUnboundName :: Text -> TC ()
requireUnboundName name =
  do  isBound <- State.gets isNameBound
      when isBound (throwError $ "Symbol '" <> name <> "' is already bound")
  where
    isNameBound env =
      Map.member name (ceVars env) -- || Map.member name (ceDecls env)

bindVar :: Text -> E.Type -> TC ()
bindVar name ty =
  do  requireUnboundName name
      State.modify \env -> env { ceVars = Map.insert name ty (ceVars env)}

bindDecl :: Text -> DeclType -> TC ()
bindDecl name declTy =
  do  requireUnboundName name
      State.modify \env -> env { ceDecls = Map.insert name declTy (ceDecls env)}


lookupMeasure :: Text -> TC (E.MeasureType, [E.Type])
lookupMeasure name =
  do mb <- State.gets (Map.lookup name . ceDecls)
     case mb of
       Nothing -> throwError $ "Undefined measure '" <> name <> "'"
       Just decl  ->
         case decl of
           DTMeasure m -> instantiate m
           _ -> throwError $ "'" <> name <> "' is not a measure."


-- lookupSampleSource :: Text -> TC E.ExperimentType
-- lookupSampleSource name =
--   do mb <- State.gets (Map.lookup name . ceDecls)
--      case mb of
--        Nothing -> throwError $ "Undefined experiment '" <> name <> "'"
--        Just decl  ->
--          case decl of
--            DTExperiment e -> pure e
--            DTModel m -> pure E.ExperimentType { E.etArgs = []
--                                               , E.etResult = m
--                                               }
--            _ -> throwError $ "'" <> name <> "' is not a experiment."

declName :: E.Decl -> Text
declName decl =
  case decl of
    E.DMeasure m -> E.tnName $ E.measureName m
    -- E.DExperiment e -> E.tnName $ E.experimentName e
    E.DModel m -> E.tnName $ E.mdName m
    E.DMain _ -> "main"

getVarType :: Text -> TC E.Type
getVarType name =
  do  vars <- State.gets ceVars
      case Map.lookup name vars of
        Nothing -> throwError ("Variable '" <> name <> "' is not defined")
        Just ty -> pure ty

bindDecl' :: E.Decl -> TC ()
bindDecl' decl = 
  case decl of
    E.DMeasure m -> 
      do  bD (DTMeasure $ E.measureType m)
    -- E.DExperiment e -> b $ DTExperiment $ E.experimentType e
    E.DModel m -> 
      do  bD (DTModel $ E.modelType m) 
          bV (E.plainModelType $ E.modelType m)
          -- addConstraint (E.IsMeasurable (E.plainModelType $ E.modelType m))
    E.DMain _ -> pure ()
  where
    bD = bindDecl (declName decl)
    bV = bindVar (declName decl)

newTVar :: TC E.Type
newTVar =
  do  i <- State.gets ceTVar
      State.modify \env -> env { ceTVar = i + 1}
      pure (E.TypeVar (E.TVFree i))

addConstraint :: E.TypeConstraint -> TC ()
addConstraint c =
  do  solveResult <- zonk c >>= solveConstraint
      case solveResult of
        Right _ -> pure ()
        Left c' ->
          State.modify \env -> env { ceConstraints = c':ceConstraints env }

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
unify t1 t2 = void $ unify' t1 t2

permissiveUnify :: E.Type -> E.Type -> TC ()
permissiveUnify t1 t2 = 
  do  t1' <- zonk t1
      t2' <- zonk t2
      case smgu t1' t2' of
        Left err -> throwError err
        Right u | Map.null u -> pure ()
                | otherwise ->
                  State.modify \env -> env { ceSubst = ceSubst env `composeSubst` u }
                       

unify' :: E.Type -> E.Type -> TC Bool
unify' t1 t2 =
  do  t1' <- zonk t1
      t2' <- zonk t2
      case mgu t1' t2' of
        Left err -> throwError err
        Right u | Map.null u -> pure False
                | otherwise ->
                    do State.modify \env -> env { ceSubst = ceSubst env `composeSubst` u }
                       pure True

scope :: TC a -> TC a
scope tc =
  do  env0 <- State.get
      a <- tc
      State.modify \env -> env { ceVars = ceVars env0
                               , ceMutable = ceMutable env0
                               }
      pure a

instantiate :: TraverseType t => E.Qualified t -> TC (t, [E.Type])
instantiate (E.Forall xs cs t) =
  do vars <- traverse freshVar xs
     let subst = Map.fromList vars
     traverse_ addConstraint (applySubst subst cs)
     pure (applySubst subst t, snd <$> vars)
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
    (E.TypeVector t1', E.TypeVector t2') -> mgu t1' t2'
    (E.TypePoint mp1, E.TypePoint mp2) -> mguMap mp1 mp2
    (E.TypeCon tc1, E.TypeCon tc2) -> mguTCon tc1 tc2
    (E.TypeVar i@(E.TVFree _), _) -> bindTVar i t2
    (_, E.TypeVar i@(E.TVFree _)) -> bindTVar i t1
    (E.TypeRandomVar t1', E.TypeRandomVar t2') -> mgu t1' t2'
    (E.TypeStream t1', E.TypeStream t2') -> mgu t1' t2'
    _ -> Left $ "Types differ: "<>pack (show (t1, t2))
    -- _ -> smgu t1 t2
  where
    ok = pure Map.empty

smgu :: E.Type -> E.Type -> Either TypeError Subst
smgu t1 t2 =
  case (t1, t2) of
    (E.TypeStream t1', t2') -> smgu t1' t2'
    (t1', E.TypeStream t2') -> smgu t1' t2'
    (E.TypeRandomVar t1', t2') -> smgu t1' t2'
    (t1', E.TypeRandomVar t2') -> smgu t1' t2'
    _ -> mgu t1 t2

mguTCon :: E.TCon -> E.TCon -> Either TypeError Subst
mguTCon tc1 tc2
  | E.tconName tc1 == E.tconName tc2 = mguMany (E.tconArgs tc1) (E.tconArgs tc2)
  | otherwise = Left "Type con mismatch"

mguMany :: [E.Type] -> [E.Type] -> Either TypeError Subst
mguMany xs ys =
  case (xs,ys) of
    ([],[]) -> pure mempty
    (x:xs',y:ys') ->
      do su1 <- mgu x y
         su2 <- mguMany (applySubst su1 <$> xs') (applySubst su1 <$> ys')
         pure (composeSubst su1 su2)
    _ -> Left "Not enough arugments to type"


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
  case var of
    E.TVFree v -> v `Set.member` freeTVars ty
    E.TVBound {} -> False

applySubst :: TraverseType t => Subst -> t -> t
applySubst subst = mapType doSubst
  where
  doSubst ty =
    case ty of
      E.TypeBool          -> E.TypeBool
      E.TypeNumber        -> E.TypeNumber
      E.TypeVector ty'    -> E.TypeVector (doSubst ty')
      E.TypePoint mp      -> E.TypePoint (doSubst <$> mp)
      E.TypeVar i         -> Map.findWithDefault ty i subst
      E.TypeCon tc        -> E.TypeCon (applySubst subst tc)
      -- E.TypeVector ty'     -> E.TypeVector (applySubst subst ty')
      E.TypeStream ty' -> E.TypeStream (doSubst ty')
      E.TypeRandomVar ty'  -> E.TypeRandomVar (doSubst ty')


freeTVars :: TraverseType t => t -> Set Int
freeTVars = collect freeVarsInType
  where
    freeVarsInType t0 =
      case t0 of
        E.TypeBool -> Set.empty
        E.TypeNumber -> Set.empty
        E.TypeVector t -> freeVarsInType t
        E.TypePoint mp -> foldMap freeVarsInType mp
        E.TypeCon tc -> freeTVars tc
        E.TypeVar (E.TVFree i) -> Set.singleton i
        E.TypeVar (E.TVBound _) -> Set.empty
        E.TypeStream t -> freeVarsInType t
        E.TypeRandomVar t -> freeVarsInType t

-------------------------------------------------------------------------------
-- constraints

solveConstraints :: [E.TypeConstraint] -> TC [E.TypeConstraint]
solveConstraints cns = go [] cns False
  where
    go unsolved todo !hasChanges =
      case todo of
        [] | hasChanges -> go [] unsolved False
           | otherwise  -> pure unsolved

        c:cs ->
          do  c' <- zonk c
              solveResult <- solveConstraint c'
              case solveResult of
                Left us -> go (us:unsolved) cs hasChanges
                Right changes -> go unsolved cs (hasChanges || changes)

solveConstraint :: E.TypeConstraint -> TC (Either E.TypeConstraint Bool)
solveConstraint = go id
  where
    go :: (E.Type -> E.Type) -> E.TypeConstraint -> TC (Either E.TypeConstraint Bool)
    go f constraint =
      case constraint of
        E.HasField recordTy label fieldTy ->
          case recordTy of
            E.TypeCon tc ->
              case Map.lookup label (E.tconFields tc) of
                -- XXX this `f` application may be unsound
                Just fieldTy' -> Right <$> unify' fieldTy (f fieldTy')
                Nothing ->
                  throwError ("'" <> E.tconName tc <>
                                              "' does not have field " <> label)

            E.TypePoint pm ->
              case Map.lookup label pm of
                Just fieldTy' -> Right <$> unify' fieldTy (f fieldTy')
                Nothing ->
                  throwError ("Record does not have field '" <> label <> "'")
            E.TypeVar _ -> pure $ Left constraint
            -- E.TypeVector ty -> go f $ E.HasField ty label fieldTy
            -- XXX: support arbitrarily many layers of vectors for this operation?
            E.TypeStream ty -> go (f . E.TypeStream) (E.HasField ty label fieldTy)
            E.TypeRandomVar ty -> go (f . E.TypeRandomVar) (E.HasField ty label fieldTy)
            _ -> throwError $ pack $ "type "<>show recordTy<>" is not data point"
            --XXX: better error
        E.IsTimeLike ty ->
          case ty of
            E.TypeNumber -> triv
            E.TypeVector E.TypeNumber -> triv
            E.TypeVar _ -> pure $ Left constraint
            _ -> throwError $ pack $ "type "<>show ty<>" is not time-like"

              
    triv = pure $ Right True