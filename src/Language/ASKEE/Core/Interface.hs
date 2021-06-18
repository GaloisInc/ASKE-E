module Language.ASKEE.Core.Interface where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Graph (stronglyConnComp)
import Data.List(partition,foldl')
import Data.Foldable(toList)

import Language.ASKEE.Model.Basics
import Language.ASKEE.Model.Interface
import Language.ASKEE.Core.Syntax
import Language.ASKEE.Core.Expr
import Language.ASKEE.Core.Eval(evalDouble)


modelInterface :: Model -> ModelInterface
modelInterface model = ModelInterface
  { modelInputs = map toParam paramDef
  , modelOutputs = map toState (Map.keys (modelInitState model) ++
                                map fst stateLets)
  }
  where
  (params, stateLets) = orderLets model
  paramDef = computeDefaults (modelParams model) params

  toParam (x,mb) = Port
    { portName      = x
    , portValueType = Real
    , portDefault   = VReal <$> mb
    , portMeta      = Map.empty
    }

  toState x = Port
    { portName      = x
    , portValueType = Real
    , portDefault   = Nothing
    , portMeta      = Map.empty
    }



-- Order lets by dependency and split into lets that don't depend on
-- state variables, and ones that do.
orderLets :: Model -> ( [(Ident,Expr)], [(Ident,Expr)] )
orderLets model = (map fst without, map fst with)
  where
  stateVars = Map.keysSet (modelInitState model)
  lets      = modelLets model

  (with,without) =
         partition snd
       $ concatMap toList
       $ stronglyConnComp
       $ [ ((n,usesState), x, Set.toList vs)
         | n@(x,e) <- Map.toList lets
         , let vs = collectExprVars e
               usesState = not $ Set.null $ Set.intersection vs stateVars
         ]

computeDefaults :: [Ident] -> [(Ident,Expr)] -> [(Ident, Maybe Double)]
computeDefaults ps0 defs =
  case foldl' addDef (Set.fromList ps0, Map.empty) defs of
    (no,yes) -> [ (x, Nothing) | x <- Set.toList no ] ++
                [ (x, Just d)  | (x,d) <- Map.toList yes ]
  where
  addDef (ps,env) (x,e)
    | not (Set.null (collectExprVars e `Set.intersection` ps))
      = (Set.insert x ps, env)
    | otherwise = (ps, Map.insert x (evalDouble e env) env)

  

