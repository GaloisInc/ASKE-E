module Language.ASKEE.ExprTransform where

import qualified Language.ASKEE.Syntax as Syntax
import qualified Language.ASKEE.Expr as Expr
import qualified Language.ASKEE.Core as Core
import qualified Data.Map as Map
import           Data.Map(Map)
import           Data.Text(Text)
import           Control.Monad.Identity(Identity(..), runIdentity)

type Arith = Expr.ArithExpr
type Log = Expr.LogExpr
type ModelE = Syntax.ModelExpr

transformArith ::
  Monad m =>
  (Arith -> m Arith) ->
  Arith ->
  m Arith
transformArith arithT ae =
  case ae of
    Expr.Add e1 e2 -> bin Expr.Add e1 e2
    Expr.Sub e1 e2 -> bin Expr.Sub e1 e2
    Expr.Mul e1 e2 -> bin Expr.Mul e1 e2
    Expr.Div e1 e2 -> bin Expr.Div e1 e2
    Expr.Neg e1    -> (Expr.Neg <$> aExpr e1) >>= arithT
    Expr.Var _     -> arithT ae
    Expr.ALit _    -> arithT ae
  where
    aExpr = transformArith arithT
    bin op e1 e2 = (op <$> aExpr e1 <*> aExpr e2) >>= arithT

transformLog ::
  Monad m =>
  (Arith -> m Arith) ->
  (Log -> m Log) ->
  Log ->
  m Log
transformLog arithT logT le =
  case le of
    Expr.And e1 e2 -> bin Expr.And e1 e2
    Expr.Or  e1 e2 -> bin Expr.Or e1 e2
    Expr.LT  e1 e2 -> cmp Expr.LT e1 e2
    Expr.LTE e1 e2 -> cmp Expr.LTE e1 e2
    Expr.EQ  e1 e2 -> cmp Expr.EQ e1 e2
    Expr.GTE e1 e2 -> cmp Expr.GTE e1 e2
    Expr.GT  e1 e2 -> cmp Expr.GT e1 e2
    Expr.Not e1 -> (Expr.Not <$> lExpr e1) >>= logT
    Expr.LLit _ -> logT le
  where
    lExpr = transformLog arithT logT
    aExpr = transformArith arithT
    bin op e1 e2 = (op <$> lExpr e1 <*> lExpr e2) >>= logT
    cmp op e1 e2 = (op <$> aExpr e1 <*> aExpr e2) >>= logT

transformMExpr ::
  Monad m =>
  (Arith -> m Arith) ->
  (Log -> m Log) ->
  (ModelE -> m ModelE) ->
  ModelE ->
  m ModelE
transformMExpr arithT logT modelT me =
  case me of
    Syntax.ArithExpr e -> (Syntax.ArithExpr <$> transformArith arithT e) >>= modelT
    Syntax.IfExpr test thn els ->
      (Syntax.IfExpr <$> lExpr test <*> mExpr thn <*> mExpr els) >>= modelT
    Syntax.CondExpr cond ->
      do  choices <- condChoice `traverse` (Syntax.condChoices cond)
          oth <- aExpr `traverse` (Syntax.condOtherwise cond)
          modelT $ Syntax.CondExpr (Syntax.Condition choices oth)
  where
    condChoice (ae, le) = (,) <$> aExpr ae <*> lExpr le
    mExpr = transformMExpr arithT logT modelT
    lExpr = transformLog arithT logT
    aExpr = transformArith arithT

transformModelExprs ::
  Monad m =>
  (Arith -> m Arith) ->
  (Log -> m Log) ->
  (ModelE -> m ModelE) ->
  Syntax.Model ->
  m Syntax.Model
transformModelExprs arithT logT modelT mdl =
  do  decls' <- transformDecl `traverse` (Syntax.modelDecls mdl)
      events' <- transformEvent `traverse` (Syntax.modelEvents mdl)
      pure $ mdl { Syntax.modelDecls = decls'
                 , Syntax.modelEvents = events'
                 }
  where
    transformDecl (Syntax.Let n v) = Syntax.Let n <$> mExpr v
    transformDecl (Syntax.State n v) = Syntax.State n <$> mExpr v
    transformDecl (Syntax.Assert e) = Syntax.Assert <$> lExpr e

    transformStmt (n, v) = (,) n <$> aExpr v
    transformEvent evt =
      do  when' <- lExpr `traverse` Syntax.eventWhen evt
          rate' <- mExpr (Syntax.eventRate evt)
          effect' <- transformStmt `traverse` Syntax.eventEffect evt
          pure $ evt { Syntax.eventWhen = when'
                     , Syntax.eventRate = rate'
                     , Syntax.eventEffect = effect'
                     }

    mExpr = transformMExpr arithT logT modelT
    lExpr = transformLog arithT logT
    aExpr = transformArith arithT

arithAsCore :: Arith -> Core.Expr
arithAsCore ae =
  case ae of
   Expr.Add e1 e2 -> binop Core.Add e1 e2
   Expr.Sub e1 e2 -> binop Core.Sub e1 e2
   Expr.Mul e1 e2 -> binop Core.Mul e1 e2
   Expr.Div e1 e2 -> binop Core.Div e1 e2
   Expr.Neg e1    -> Core.Expr1 Neg (arithAsCore e1)
   Expr.ALit d    -> Core.Literal (NumLit d)
   Expr.Var t     -> Core.ExprVar t
  where
    binop op e1 e2 = Core.Expr2 op (arithAsCore e1) (arithAsCore e2)

logAsCore :: Log -> Core.Expr
logAsCore le =
  case le of
    Expr.GT e1 e2  -> cmp Core.Lt  e2 e1
    Expr.GTE e1 e2 -> cmp Core.Lte e2 e1
    Expr.EQ e1 e2  -> cmp Core.Eq  e1 e2
    Expr.LTE e1 e2 -> cmp Core.Lte e1 e2
    Expr.LT e1 e2  -> cmp Core.Lt  e1 e2
    Expr.And e1 e2 -> binop Core.And e1 e2
    Expr.Or e1 e2  -> binop Core.Or  e1 e2
    Expr.Not e1    -> unop  Core.Not (logAsCore e1)
    Expr.LLit b    -> Core.Literal (Core.BoolLit b)
  where
    cmp op e1 e2   = Core.Expr2 op (arithAsCore e1) (arithAsCore e2)
    binop op e1 e2 = Core.Expr2 op (logAsCore e2) (logAsCore e2)
    unop op e1     = Core.Expr1 op (logAsCore e2)

modelEasCore :: ModelE -> Core.Expr
modelEasCore me =
  case me of
    Syntax.ArithExpr e1 -> arithAsCore e1
    Syntax.IfExpr tst e1 e2 -> Core.ExprIf (logAsCore tst) (modelEasCore e1) (modelEasCore e2)
    Syntax.CondExpr cs ->
      condAsCore (Syntax.condChoices cs) (Syntax.condOtherwise cs)
  where
    condAsCore [] (Just e) = arithAsCore e
    condAsCore [] Nothing = Core.ExprFail "Incomplete match"
    condAsCore ((ae, le):t) oth =
      Core.ExprIf (logAsCore le) (arithAsCore ae) (condAsCore t oth)


transformCoreExprM :: Monad m => (Core.Expr -> m Core.Expr) -> Core.Expr -> m Core.Expr
transformCoreExprM tx e0 =
  case e0 of
    Core.ExprAdd e1 e2 -> binop Core.ExprAdd e1 e2
    Core.ExprMul e1 e2 -> binop Core.ExprMul e1 e2
    Core.ExprSub e1 e2 -> binop Core.ExprSub e1 e2
    Core.ExprDiv e1 e2 -> binop Core.ExprDiv e1 e2
    Core.ExprLT  e1 e2 -> binop Core.ExprLT e1 e2
    Core.ExprEQ  e1 e2 -> binop Core.ExprEQ e1 e2
    Core.ExprGT  e1 e2 -> binop Core.ExprGT e1 e2
    Core.ExprAnd e1 e2 -> binop Core.ExprAnd e1 e2
    Core.ExprOr  e1 e2 -> binop Core.ExprOr e1 e2
    Core.ExprNot e1    -> unop Core.ExprNot e1
    Core.ExprNeg e1    -> unop Core.ExprNot e1
    Core.ExprIf  e1 e2 e3 ->
      (Core.ExprIf <$> subExpr e1 <*> subExpr e2 <*> subExpr e3) >>= tx
    Core.ExprNumLit _ -> tx e0
    Core.ExprVar    _ -> tx e0
    Core.ExprFail   _ -> tx e0
    Core.ExprBoolLit _ -> tx e0
  where
    subExpr = transformCoreExprM tx
    unop op e1 = (op <$> subExpr e1) >>= tx
    binop op e1 e2 = (op <$> subExpr e1 <*> subExpr e2) >>= tx

transformCoreExpr :: (Core.Expr -> Core.Expr) -> Core.Expr -> Core.Expr
transformCoreExpr f = runIdentity . transformCoreExprM (Identity . f)

simplifyLiterals :: Core.Expr -> Core.Expr
simplifyLiterals = transformCoreExpr simpl
  where
    simpl e0 =
      case e0 of
        -- ops on literals
        Core.ExprNeg (Core.ExprNumLit d) -> Core.ExprNumLit (-d)
        Core.ExprAdd (Core.ExprNumLit d1) (Core.ExprNumLit d2) ->
          Core.ExprNumLit (d1 + d2)
        Core.ExprSub (Core.ExprNumLit d1) (Core.ExprNumLit d2) ->
          Core.ExprNumLit (d1 - d2)
        Core.ExprDiv (Core.ExprNumLit d1) (Core.ExprNumLit d2)
          | d2 /= 0.0 -> Core.ExprNumLit (d1 / d2)
          | d2 == 0.0 -> Core.ExprFail "Division by 0"
        Core.ExprMul (Core.ExprNumLit d1) (Core.ExprNumLit d2) ->
          Core.ExprNumLit (d1 * d2)

        Core.ExprAnd (Core.ExprBoolLit b1) (Core.ExprBoolLit b2) ->
          Core.ExprBoolLit (b1 && b2)
        Core.ExprOr (Core.ExprBoolLit b1) (Core.ExprBoolLit b2) ->
          Core.ExprBoolLit (b1 || b2)
        Core.ExprNot (Core.ExprBoolLit b1) ->
          Core.ExprBoolLit (not b1)

        Core.ExprLT (Core.ExprNumLit d1) (Core.ExprNumLit d2) ->
          Core.ExprBoolLit (d1 < d2)
        Core.ExprGT (Core.ExprNumLit d1) (Core.ExprNumLit d2) ->
          Core.ExprBoolLit (d1 > d2)
        Core.ExprEQ (Core.ExprNumLit d1) (Core.ExprNumLit d2) ->
          Core.ExprBoolLit (d1 == d2)

        Core.ExprIf (Core.ExprBoolLit True) e1 _ -> e1
        Core.ExprIf (Core.ExprBoolLit False) _ e2 -> e2

        e -> e

-- TODO: check for unbound vars?
modelAsCore :: Syntax.Model -> Either String Core.Model
modelAsCore mdl =
  do  let events' = mkEvent `map` (Syntax.modelEvents mdl)
      inits <- initState `traverse` Syntax.stateDecls (Syntax.modelDecls mdl)
      pure $ Core.Model  { Core.modelName = Syntax.modelName mdl
                         , Core.modelEvents =  events'
                         , Core.modelInitState = inits
                         }

  where
    mkEvent evt =
      let effs' = mkEffect <$> Syntax.eventEffect evt
          rate = letSubst (modelEasCore $ Syntax.eventRate evt)
          mbWhen = letSubst . logAsCore <$> Syntax.eventWhen evt
          when =
            case mbWhen of
              Nothing -> Core.ExprBoolLit True
              Just a  -> a

      in Core.Event { Core.eventName = Syntax.eventName evt
                    , Core.eventRate = rate
                    , Core.eventWhen = when
                    , Core.eventEffect = effs'
                    }

    mkEffect (n, v) = (n, letSubst (arithAsCore v))

    lets = Syntax.letDecls (Syntax.modelDecls mdl)
    letMap = Map.fromList (fmap modelEasCore <$> lets)
    letSubstVar (Core.ExprVar v) =
      case Map.lookup v letMap of
        Nothing -> Core.ExprVar v
        Just e  -> letSubst e

    letSubstVar e = e
    letSubst = transformCoreExpr letSubstVar

    initState (n, v) = (,) n <$> evalConstDouble (modelEasCore v)

    -- TODO: better errors
    evalConstDouble e =
      case simplifyLiterals $ letSubst e of
        (Core.ExprNumLit d) -> pure d
        _ -> Left ("Expression is not a constant double: " ++ show e)


