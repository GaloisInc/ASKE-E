{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Experiment.EaselAdapter where

import qualified Language.ASKEE.Core as Core
import qualified Language.ASKEE.Experiment.Syntax as E
import qualified Data.Map as Map
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
    renameEvent i e = e { Core.eventName = snd (nameVariant i (Core.eventName e)) }
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
    measureSetters = Map.fromList (asSetters `concatMap` E.measureImpl decl)
    evtWithSetters evt =
      evt { Core.eventEffect = Core.eventEffect evt `Map.union` measureSetters }


asSetters :: E.Stmt -> [(Core.Ident, Core.Expr)]
asSetters stmt =
  case stmt of
    E.Set n e -> [(E.tnName n, convertExpression e )]
    _ -> []

convertExpression :: E.Expr -> Core.Expr
convertExpression e =
  case e of
    E.Lit (E.LitNum n) -> Core.NumLit n
    E.Var t -> Core.Var (E.tnName t)
    E.Call E.Add [e1, e2] -> Core.Op2 Core.Add (convertExpression e1) (convertExpression e2)
    E.Dot _ l -> Core.Var l  -- this might need another look
    _ -> error "nooooo"
