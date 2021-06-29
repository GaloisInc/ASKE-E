{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Language.ASKEE.Core.Interface where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List(foldl')

import Language.ASKEE.Model.Basics
import Language.ASKEE.Model.Interface
import Language.ASKEE.Core.Syntax
import Language.ASKEE.Core.Expr
import Language.ASKEE.Core.Eval(evalDouble)

-- NOTE: the parameters defaults in our representations are expressions,
-- white the ones in the interface are constants.    To make the two fit
modelInterface :: Model -> ModelInterface
modelInterface Model{..} = ModelInterface
  { modelInputs = map toParam params
  , modelOutputs = map toState (Map.keys modelInitState ++ Map.keys modelLets)
  }
  where
    noDefault   = [ x | (x,Nothing) <- Map.toList modelParams ]
    params      = computeDefaults noDefault
                $ orderDecls [ (x,e) | (x,Just e) <- Map.toList modelParams ]

    toParam (x,mb) = Port
      { portName      = x
      , portValueType = Real
      , portDefault   = VReal <$> mb
      , portMeta      = meta x
      }

    toState x = Port
      { portName      = x
      , portValueType = Real
      , portDefault   = Nothing
      , portMeta      = meta x
      }

    meta x = Map.findWithDefault Map.empty x modelMeta

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
