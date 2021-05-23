module Language.ASKEE.Core.ImportASKEE where

import qualified Data.Map as Map

import qualified Language.ASKEE.Expr   as Src
import qualified Language.ASKEE.Syntax as Src
import Language.ASKEE.Core
import Language.ASKEE.Core.Simplify(simplifyExpr)

modelAsCore :: Src.Model -> Either String Model
modelAsCore mdl =
  do let inits = initState <$> Src.stateDecls decls
     pure modelNoInit { modelInitState = Map.fromList inits }

  where
  decls = Src.modelDecls mdl

  modelNoInit =
    mapExprs simplifyExpr $
    inlineLets
    Model { modelName      = Src.modelName mdl
          , modelParams    = [n | (n, _) <- Src.parameterDecls decls ]
          , modelEvents    = map eventAsCore (Src.modelEvents mdl)
          , modelLets      = Map.fromList
                              [ (x, expAsCore e) | (x,e) <- Src.letDecls decls ]
          , modelInitState = Map.empty
          }

  initState (n,e) =
    (n, simplifyExpr (substExpr (modelLets modelNoInit) (expAsCore e)))


eventAsCore :: Src.Event -> Event
eventAsCore evt =
  Event
    { eventName   = Src.eventName evt
    , eventRate   = expAsCore (Src.eventRate evt)
    , eventWhen   = maybe (BoolLit True) expAsCore (Src.eventWhen evt)
    , eventEffect = Map.fromList (mkEffect <$> Src.eventEffect evt)
    }
  where mkEffect (n, v) = (n, expAsCore v)


expAsCore :: Src.Expr -> Expr
expAsCore e =
  case e of
    Src.Add e1 e2     -> binop Add e1 e2
    Src.Sub e1 e2     -> binop Sub e1 e2
    Src.Mul e1 e2     -> binop Mul e1 e2
    Src.Div e1 e2     -> binop Div e1 e2
    Src.Exp e1        -> Op1 Exp (expAsCore e1)
    Src.Log e1        -> Op1 Log (expAsCore e1)
    Src.Neg e1        -> Op1 Neg (expAsCore e1)
    Src.LitD d        -> NumLit d
    Src.Var t         -> Var t
    Src.GT e1 e2      -> cmp Lt  e2 e1
    Src.GTE e1 e2     -> cmp Leq e2 e1
    Src.EQ e1 e2      -> cmp Eq  e1 e2
    Src.LTE e1 e2     -> cmp Leq e1 e2
    Src.LT e1 e2      -> cmp Lt  e1 e2
    Src.And e1 e2     -> binop And e1 e2
    Src.Or e1 e2      -> binop Or e1 e2
    Src.Not e1        -> unop Not e1
    Src.LitB b        -> BoolLit b
    Src.If tst e1 e2  -> If (expAsCore tst) (expAsCore e1) (expAsCore e2)
    Src.Cond bs oth   -> condAsCore bs oth
  where
  cmp op e1 e2   = Op2 op (expAsCore e1) (expAsCore e2)
  binop op e1 e2 = Op2 op (expAsCore e1) (expAsCore e2)
  unop op e1     = Op1 op (expAsCore e1)

  condAsCore [] (Just d) = expAsCore d
  condAsCore [] Nothing  = Fail "Incomplete match"
  condAsCore ((ae, le):t) oth =
    If (expAsCore le) (expAsCore ae) (condAsCore t oth)



