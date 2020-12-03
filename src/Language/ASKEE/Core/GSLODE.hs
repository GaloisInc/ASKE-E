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


-- | Create an computation environment from a vector of parameters.
paramsFromVector :: [Ident] -> LinAlg.Vector Double -> Map Ident Double
paramsFromVector ps vs = Map.fromList (ps `zip` LinAlg.toList vs)

-- | Turn the errors into a vector of residuals.
paramsToVector :: [Ident] -> Map Ident Double -> LinAlg.Vector Double
paramsToVector ps errs = LinAlg.fromList [ errs Map.! p | p <- ps ]

-- | The function to be minimized
minFunction ::
  DiffEqs ->
  DataSeries Double ->
  LinAlg.Vector Double -> LinAlg.Vector Double
minFunction eqs ds = minFunction' eqs ds . paramsFromVector (deqParams eqs)

-- | The function to be minimized with parameters specified as a map
minFunction' ::
  DiffEqs ->
  DataSeries Double ->
  Map Ident Double -> LinAlg.Vector Double
minFunction' eqs ds = getErr
  where
  ps = deqParams eqs
  getErr vs = paramsToVector ps
            $ computeErrorPerVar
            $ computeModelError eqs vs ds


-- XXX: formRows or fromCols?
residualChange ::
  DiffEqs ->
  DataSeries Double ->
  LinAlg.Vector Double -> LinAlg.Matrix Double
residualChange eqs ds ps = LinAlg.fromRows (map change (deqParams eqs))
  where
  paramMap = paramsFromVector (deqParams eqs) ps
  here     = minFunction' eqs ds paramMap
  change p = minFunction' eqs ds (Map.adjust (+1) p paramMap) - here


fitModel ::
  DiffEqs           {- ^ Model to optimize -} ->
  DataSeries Double {- ^ Data to fit -} ->
  FIT.FittingMethod {- ^ Method to use for fitting: XXX scaled or not? -} ->
  Double            {- ^ Absolute tolerance -} ->
  Double            {- ^ Relative tolerance -} ->
  Int               {- ^ Maximum number of iterations -} ->
  Map Ident Double  {- ^ Initial parameter values -} ->
  Map Ident Double  {- ^ Optimized parameters -}
fitModel eqs ds method absTol relTol limit start =
    paramsFromVector (deqParams eqs)
  $ fst
  $ FIT.nlFitting method absTol relTol limit
      (minFunction eqs ds)
      (residualChange eqs ds)
      (paramsToVector (deqParams eqs) start)




