{-# Language PatternSynonyms #-}
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
   Expr.Neg e1    -> Core.Op1 Core.Neg (arithAsCore e1)
   Expr.ALit d    -> NumLit d
   Expr.Var t     -> Core.Var t
  where
    binop op e1 e2 = Core.Op2 op (arithAsCore e1) (arithAsCore e2)

logAsCore :: Log -> Core.Expr
logAsCore le =
  case le of
    Expr.GT e1 e2  -> cmp Core.Lt  e2 e1
    Expr.GTE e1 e2 -> cmp Core.Leq e2 e1
    Expr.EQ e1 e2  -> cmp Core.Eq  e1 e2
    Expr.LTE e1 e2 -> cmp Core.Leq e1 e2
    Expr.LT e1 e2  -> cmp Core.Lt  e1 e2
    Expr.And e1 e2 -> binop Core.And e1 e2
    Expr.Or e1 e2  -> binop Core.Or  e1 e2
    Expr.Not e1    -> unop  Core.Not e1
    Expr.LLit b    -> Core.Literal (Core.Bool b)
  where
    cmp op e1 e2   = Core.Op2 op (arithAsCore e1) (arithAsCore e2)
    binop op e1 e2 = Core.Op2 op (logAsCore e2) (logAsCore e2)
    unop op e1     = Core.Op1 op (logAsCore e1)

modelEasCore :: ModelE -> Core.Expr
modelEasCore me =
  case me of
    Syntax.ArithExpr e1 -> arithAsCore e1
    Syntax.IfExpr tst e1 e2 -> Core.If (logAsCore tst) (modelEasCore e1) (modelEasCore e2)
    Syntax.CondExpr cs ->
      condAsCore (Syntax.condChoices cs) (Syntax.condOtherwise cs)
  where
    condAsCore [] (Just e) = arithAsCore e
    condAsCore [] Nothing = Core.Fail "Incomplete match"
    condAsCore ((ae, le):t) oth =
      Core.If (logAsCore le) (arithAsCore ae) (condAsCore t oth)


transformCoreExprM :: Monad m => (Core.Expr -> m Core.Expr) -> Core.Expr -> m Core.Expr
transformCoreExprM tx e0 =
  case e0 of
    Core.Op2 op e1 e2 -> binop op e1 e2
    Core.Op1 op e1    -> unop op e1
    Core.If  e1 e2 e3 ->
      (Core.If <$> subExpr e1 <*> subExpr e2 <*> subExpr e3) >>= tx
    Core.Literal {} -> tx e0
    Core.Var    _ -> tx e0
    Core.Fail   _ -> tx e0
  where
    subExpr = transformCoreExprM tx
    unop op e1 = (Core.Op1 op <$> subExpr e1) >>= tx
    binop op e1 e2 = (Core.Op2 op <$> subExpr e1 <*> subExpr e2) >>= tx

transformCoreExpr :: (Core.Expr -> Core.Expr) -> Core.Expr -> Core.Expr
transformCoreExpr f = runIdentity . transformCoreExprM (Identity . f)


pattern NumLit :: Double -> Core.Expr
pattern NumLit d = Core.Literal (Core.Num d)
pattern BoolLit :: Bool -> Core.Expr
pattern BoolLit b = Core.Literal (Core.Bool b)

simplifyLiterals :: Core.Expr -> Core.Expr
simplifyLiterals = transformCoreExpr simpl
  where

    simpl e0 =
      case e0 of
        -- ops on literals
        Core.Op1 Core.Neg (NumLit d) -> NumLit (-d)
        Core.Op2 Core.Add (NumLit d1) (NumLit d2) ->
          NumLit (d1 + d2)
        Core.Op2 Core.Sub (NumLit d1) (NumLit d2) ->
          NumLit (d1 - d2)
        Core.Op2 Core.Div (NumLit d1) (NumLit d2)
          | d2 /= 0.0 -> NumLit (d1 / d2)
          | d2 == 0.0 -> Core.Fail "Division by 0"
        Core.Op2 Core.Mul (NumLit d1) (NumLit d2) ->
          NumLit (d1 * d2)

        Core.Op2 Core.And (BoolLit b1) (BoolLit b2) ->
          BoolLit (b1 && b2)
        Core.Op2 Core.Or (BoolLit b1) (BoolLit b2) ->
          BoolLit (b1 || b2)
        Core.Op1 Core.Not (BoolLit b1) ->
          BoolLit (not b1)

        Core.Op2 Core.Lt (NumLit d1) (NumLit d2) ->
          BoolLit (d1 < d2)
        Core.Op2 Core.Leq (NumLit d1) (NumLit d2) ->
          BoolLit (d1 > d2)
        Core.Op2 Core.Eq (NumLit d1) (NumLit d2) ->
          BoolLit (d1 == d2)

        Core.If (BoolLit True) e1 _ -> e1
        Core.If (BoolLit False) _ e2 -> e2

        e -> e

-- TODO: check for unbound vars?
modelAsCore :: Syntax.Model -> Either String Core.Model
modelAsCore mdl =
  do  let events' = mkEvent `map` (Syntax.modelEvents mdl)
      inits <- initState `traverse` Syntax.stateDecls (Syntax.modelDecls mdl)
      pure $ Core.Model  { Core.modelName = Syntax.modelName mdl
                         , Core.modelEvents =  events'
                         , Core.modelInitState = Map.fromList inits
                         }

  where
    mkEvent evt =
      let effs' = mkEffect <$> Syntax.eventEffect evt
          rate = letSubst (modelEasCore $ Syntax.eventRate evt)
          mbWhen = letSubst . logAsCore <$> Syntax.eventWhen evt
          when =
            case mbWhen of
              Nothing -> BoolLit True
              Just a  -> a

      in Core.Event { Core.eventName = Syntax.eventName evt
                    , Core.eventRate = rate
                    , Core.eventWhen = when
                    , Core.eventEffect = Map.fromList effs'
                    }

    mkEffect (n, v) = (n, letSubst (arithAsCore v))

    lets = Syntax.letDecls (Syntax.modelDecls mdl)
    letMap = Map.fromList (fmap modelEasCore <$> lets)
    letSubstVar (Core.Var v) =
      case Map.lookup v letMap of
        Nothing -> Core.Var v
        Just e  -> letSubst e

    letSubstVar e = e
    letSubst = transformCoreExpr letSubstVar

    initState (n, v) = (,) n <$> evalConstDouble (modelEasCore v)

    -- TODO: better errors
    evalConstDouble e =
      case simplifyLiterals $ letSubst e of
        (NumLit d) -> pure d
        _ -> Left ("Expression is not a constant double: " ++ show e)


