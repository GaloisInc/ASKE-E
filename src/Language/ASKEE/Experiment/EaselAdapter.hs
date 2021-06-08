{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Language.ASKEE.Experiment.EaselAdapter where

import qualified Language.ASKEE.Core.Expr as Core
import qualified Language.ASKEE.Core.Syntax as Core
import qualified Language.ASKEE.Experiment.Syntax as E
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Text(pack)

modelType :: Core.Model -> E.Type
modelType m =
  E.TypePoint . Map.fromList $
    ("time", E.TypeNumber):[(n, E.TypeNumber) | n <- Core.modelStateVars m ]

replicateModel :: Int -> Core.Model -> Core.Model
replicateModel n m =
    m { Core.modelParams = Core.modelParams m
      , Core.modelInitState = Map.unions (Core.modelInitState <$> variants)
      , Core.modelEvents = concat (Core.modelEvents <$> variants)
      , Core.modelLets = Map.unions (Core.modelLets <$> variants)
      }
  where
    variants = variant <$> [1..n]
    varNames = Map.keys (Core.modelInitState m) ++
               Map.keys (Core.modelLets m)
    nameVariant i x = (x, x <> "_" <> pack (show i))
    nameMap i = Core.Var <$> Map.fromList (nameVariant i <$> varNames)
    renameEvent i e = e { Core.eventName = snd (nameVariant i (Core.eventName e))
                        , Core.eventEffect = Map.mapKeys (snd . nameVariant i)
                                                         (Core.eventEffect e)
                        }
    variant i =
      Core.mapExprs (Core.substExpr (nameMap i)) m
        { Core.modelInitState = Map.mapKeys (snd . nameVariant i) (Core.modelInitState m)
        , Core.modelLets = Map.mapKeys (snd . nameVariant i) (Core.modelLets m)
        , Core.modelEvents = renameEvent i <$> Core.modelEvents m
        }

withMeasure :: E.MeasureDecl -> Core.Model -> Core.Model
withMeasure decl m =
    m { Core.modelInitState = Core.modelInitState m `Map.union` newVars
      , Core.modelEvents = evtWithSetters <$> Core.modelEvents m
      }
  where
    newVars = Map.fromList (convertVar <$> E.measureVars decl)
    convertVar (n, v) = (E.tnName n, convertExpression v)
    measureSetters = convertBlock $ E.measureImpl decl
    evtWithSetters evt =
      evt { Core.eventEffect = Core.eventEffect evt `Map.union` measureSetters }


convertBlock :: [E.Stmt] -> Map Core.Ident Core.Expr
convertBlock stmts = Map.unions (asSetters <$> stmts)

asSetters :: E.Stmt -> Map Core.Ident Core.Expr
asSetters stmt =
  case stmt of
    E.Set n e -> Map.singleton (E.tnName n) (convertExpression e)
    E.If thens blockElse ->
      foldr addThen (convertBlock blockElse) thens
    _ -> Map.empty
  where
    merge p _ thenE elseE = Just $ Core.If p thenE elseE
    mapThen p = Map.mapWithKey \n ex -> Core.If p ex (Core.Var n)
    mapElse p = mapThen (Core.Op1 Core.Not p)
    addThen (test, stmts) =
      let stmts' = convertBlock stmts
          test' = convertExpression test
      in Map.mergeWithKey (merge test') (mapThen test') (mapElse test') stmts'



convertExpression :: E.Expr -> Core.Expr
convertExpression e =
  case e of
    E.Lit (E.LitNum n) -> Core.NumLit n
    E.Var t -> Core.Var (E.tnName t)
    E.Call E.Add [e1, e2] -> Core.Op2 Core.Add (convertExpression e1) (convertExpression e2)
    E.Dot _ l _ -> Core.Var l  -- this might need another look
    E.Call E.LessThan [e1, e2] -> Core.Op2 Core.Lt (convertExpression e1)
                                                   (convertExpression e2)
    E.Call E.GreaterThanEqual [e1, e2] -> convertExpression e2 Core.:<=: convertExpression e1

    _ -> error ("nooooo: " ++ show e)
