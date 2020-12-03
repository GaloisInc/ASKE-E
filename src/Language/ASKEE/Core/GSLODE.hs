{-# Language BlockArguments, OverloadedStrings #-}
module Language.ASKEE.Core.GSLODE where

import Data.Map(Map)
import qualified Data.Map as Map

import qualified Numeric.LinearAlgebra.Data as LinAlg
import qualified Numeric.GSL.ODE as ODE
import qualified Numeric.GSL.Fitting as FIT

import Language.ASKEE.Core
import Language.ASKEE.Core.Eval(evalDouble)
import Language.ASKEE.Core.DiffEq
import Language.ASKEE.DataSeries

import Data.IORef(newIORef,atomicModifyIORef)
import System.IO.Unsafe(unsafeDupablePerformIO)

import Debug.Trace


evalDiffEqs ::
  [Ident] -> [Expr] -> Map Ident Double -> Double -> [Double] -> [Double]
evalDiffEqs xs es ps t s = [ evalDouble e env | e <- es ]
  where
  env = Map.insert "time" t (Map.fromList (zip xs s)) `Map.union` ps

simulate :: DiffEqs -> Map Ident Double -> [Double] -> DataSeries Double
simulate eqs paramVs ts =
  DataSeries { times = ts
             , values = Map.fromList (zip xs rows)
             }
  -- XXX: evalute lets also?

  where
  (xs,es) = unzip (Map.toList (deqState eqs))
  initS  = [ evalDouble e paramVs | e <- Map.elems (deqInitial eqs) ]
  resMatrix = ODE.odeSolve (evalDiffEqs xs es paramVs)
                           initS
                           (LinAlg.fromList ts)

  rows = LinAlg.toList <$> LinAlg.toColumns resMatrix

-- | Compute the square of the difference between the model and the data.
computeModelError ::
  DiffEqs -> Map Ident Double -> DataSeries Double -> DataSeries Double
computeModelError eqs ps expected =
  zipAligned err (simulate eqs ps (times expected)) expected
  where
  err x y = let diff = x - y in diff * diff

-- | Add up all the errors for each veriable
computeErrorPerVar :: DataSeries Double -> Map Ident Double
computeErrorPerVar errs = foldDataSeries (+) startErr errs
  where
  startErr = const 0 <$> values errs

-- test

model :: [Double] -> Double -> [Double]
model = memo \[a,b] -> trace ("model: " ++ show [a,b])
              (\t -> trace ("mode-t " ++ show t) $ [a * t + b])

model' = memo \[a,b] -> trace ("deriv: " ++ show [a,b])
                      (\t -> trace ("deriv-t " ++ show t)[ [t,1] ])


test = fst
     $ FIT.fitModel 1e-4 1e-4 20 (model,model')
        [ (x, [y]) | x <- [ 1..100], let y = 2 * x + 3 ]
        [0,0]

memo :: ([Double] -> a) -> [Double] -> a
memo f = unsafeDupablePerformIO
  do ref <- newIORef ([], f [])
     pure \xs -> unsafeDupablePerformIO
               $ atomicModifyIORef ref \v@(key,val) ->
                  if xs == key then (v,val)
                               else let b = f xs
                                    in ((xs,b),b)


