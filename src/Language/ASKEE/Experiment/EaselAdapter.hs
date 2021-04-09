{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Experiment.EaselAdapter where

import qualified Language.ASKEE.Core as Core
import qualified Language.ASKEE.Experiment.Syntax as E
import qualified Data.Map as Map

modelType :: Core.Model -> E.Type
modelType m =
  E.TypePoint . Map.fromList $
    ("time", E.TypeNumber):[(n, E.TypeNumber) | n <- Core.modelStateVars m ]