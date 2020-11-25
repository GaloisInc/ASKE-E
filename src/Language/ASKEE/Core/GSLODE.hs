{-# Language OverloadedStrings #-}
module Language.ASKEE.Core.GSLODE where

import Data.Map(Map)
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

simulate :: DiffEqs -> [Double] -> DataSeries Double
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

-- | Compute the square of the difference between the model and the data.
computeModelError :: DiffEqs -> DataSeries Double -> DataSeries Double
computeModelError eqs expected =
  zipAligned err (simulate eqs (times expected)) expected
  where
  err x y = let diff = x - y in diff * diff

-- | Add up all the errors for each veriable
computeErrorPerVar :: DataSeries Double -> Map Ident Double
computeErrorPerVar errs = foldDataSeries (+) startErr errs
  where
  startErr = const 0 <$> values errs


