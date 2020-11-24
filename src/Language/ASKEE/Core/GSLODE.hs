{-# Language OverloadedStrings #-}
module Language.ASKEE.Core.GSLODE where

import Data.Map(Map)
import qualified Data.Map as Map

import qualified Numeric.LinearAlgebra.Data as LinAlg
import qualified Numeric.GSL.ODE as ODE

import Language.ASKEE.Core
import Language.ASKEE.Core.Eval(evalDouble)
import Language.ASKEE.Core.DiffEq


evalDiffEqs :: [Ident] -> [Expr] -> Double -> [Double] -> [Double]
evalDiffEqs xs es t s = [ evalDouble e env | e <- es ]
  where
  env = Map.insert "time" t (Map.fromList (zip xs s))

simulate :: DiffEqs -> [Double] -> Map Ident [Double]
simulate eqs times = Map.fromList (zip xs rows)
  -- XXX: evalute lets also?

  where
  (xs,es) = unzip (Map.toList (deqState eqs))
  resMatrix = ODE.odeSolve (evalDiffEqs xs es)
                           (Map.elems (deqInitial eqs))
                           (LinAlg.fromList times)

  rows = LinAlg.toList <$> LinAlg.toColumns resMatrix
