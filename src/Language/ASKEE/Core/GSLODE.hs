{-# Language OverloadedStrings #-}
module Language.ASKEE.Core.GSLODE where

import qualified Data.Map as Map

import qualified Numeric.LinearAlgebra.Data as LinAlg
import qualified Numeric.GSL.ODE as ODE

import Language.ASKEE.Core
import Language.ASKEE.Core.Eval(evalDouble)
import Language.ASKEE.Core.DiffEq
import Language.ASKEE.DataSeries


evalDiffEqs :: [Ident] -> [Expr] -> Double -> [Double] -> [Double]
evalDiffEqs xs es t s = [ evalDouble e env | e <- es ]
  where
  env = Map.insert "time" t (Map.fromList (zip xs s))

simulate :: DiffEqs -> [Double] -> DataSeries
simulate eqs ts =
  DataSeries { times = ts
             , values = Map.fromList (zip xs rows)
             }
  -- XXX: evalute lets also?

  where
  (xs,es) = unzip (Map.toList (deqState eqs))
  resMatrix = ODE.odeSolve (evalDiffEqs xs es)
                           (Map.elems (deqInitial eqs))
                           (LinAlg.fromList ts)

  rows = LinAlg.toList <$> LinAlg.toColumns resMatrix
