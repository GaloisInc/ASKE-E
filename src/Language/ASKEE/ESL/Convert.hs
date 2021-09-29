{-# Language OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ESL.Convert where

import Data.Text(Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe(catMaybes)

import           Language.ASKEE.Metadata
import           Language.ASKEE.Core.Expr
import           Language.ASKEE.Core.Syntax ( Event(..), Model(..) )
import qualified Language.ASKEE.ESL.Syntax  as Src
import qualified Language.ASKEE.Expr        as Src
import qualified Data.Set as Set

modelAsCore :: Src.Model -> Model
modelAsCore mdl =
  mapExprs simplifyExpr
  Model { modelName      = Src.modelName mdl
        , modelParams    = mParams
        , modelEvents    = map eventAsCore (Src.modelEvents mdl)
        , modelLets      = Map.fromList mLets
        , modelInitState =
            Map.fromList [(v, expAsCore e) | (v, e) <- Src.stateDecls decls]
        , modelMeta = extractMeta mdl
        }

  where
  decls = Src.modelDecls mdl

  mParams = Map.fromList
          $ [ (x, expAsCore <$> e) | (x,e) <- Src.parameterDecls decls ] ++
            [ (x, Just e)          | (x,e) <- letParams ]

  (letParams,mLets) =
    partOrderDecls
      (Set.fromList ("time" : map fst (Src.stateDecls decls)))
      [ (x, expAsCore e) | (x,e) <- Src.letDecls decls ]


extractMeta :: Src.Model -> Map Ident (Map Text [Text])
extractMeta model =
  Map.fromList
    $ ("", meta (Src.modelMeta model))
    : catMaybes ( map declMeta  (Src.modelDecls model) ++
                  map eventMeta (Src.modelEvents model) )
  where
  meta xs = Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ]

  addName x = ("name",x)

  declMeta d =
    case metaValue d of
      Src.Let   x _     -> Just (x, meta (addName x : metaData d))
      Src.State x _     -> Just (x, meta (addName x : metaData d))
      Src.Parameter x _ -> Just (x, meta (addName x : metaData d))
      Src.Assert _      -> Nothing

  -- XXX
  eventMeta ev =
    do v <- Src.eventMetadata ev
       pure (Src.eventName ev, meta [ addName (Src.eventName ev)
                                    , ("description",v)])




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
    Src.Sin e1        -> Op1 Sin (expAsCore e1)
    Src.Pow e1 e2     -> binop Pow e1 e2
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
