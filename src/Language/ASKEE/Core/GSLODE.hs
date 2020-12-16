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
             , values = Map.fromList (zip xs (map upd rows))
             }
  -- XXX: evalute lets also?

  where
  (xs,es) = unzip (Map.toList (deqState eqs))
  initS  = [ evalDouble e paramVs | e <- Map.elems (deqInitial eqs) ]

  resMatrix = ODE.odeSolve (evalDiffEqs xs es paramVs)
                           initS
                           (LinAlg.fromList ts')

  -- we add time 0, because our initial state is for time 0
  -- also we assume no -ve times
  (upd,ts') = case ts of
                t:_ | t > 0 -> (tail,0 : ts)
                _           -> (id,ts)

  rows = LinAlg.toList <$> LinAlg.toColumns resMatrix


-- | Compute the difference between the model and the data, using
-- the provided functions.
modelErrorWith ::
  (Ident -> Double -> Double -> Double) {- ^ how to compute difference -} ->
  DiffEqs -> DataSeries Double -> Map Ident Double -> DataSeries Double
modelErrorWith err eqs expected ps =
  zipAlignedWithTimeAndLabel err' (simulate eqs ps (times expected)) expected
  where err' l _ x y = err l x y


-- | Compute the square of the difference between the model and the data.
modelSquareError ::
  DiffEqs -> DataSeries Double -> Map Ident Double -> DataSeries Double
modelSquareError =
  modelErrorWith \_ x y -> let diff = x - y in diff * diff

-- | Add up all the errors for each veriable
computeErrorPerVar :: DataSeries Double -> Map Ident Double
computeErrorPerVar errs = foldDataSeries (+) startErr errs
  where
  startErr = const 0 <$> values errs



residualChange ::
  (Ident -> Double -> Double -> Double) ->
  DiffEqs ->
  DataSeries Double ->
  LinAlg.Vector Double -> LinAlg.Matrix Double
residualChange err eqs ds ps = LinAlg.fromColumns (map change (deqParams eqs))
  where
  paramMap = paramsFromVector (deqParams eqs) ps
  mkErr    = dsToVector . modelErrorWith err eqs ds
  here     = mkErr paramMap
  change p = (mkErr (Map.adjust (+1) p paramMap) - here)

-- | Create a computation environment from a vector of parameters.
paramsFromVector :: [Ident] -> LinAlg.Vector Double -> Map Ident Double
paramsFromVector ps vs = Map.fromList (ps `zip` LinAlg.toList vs)

paramsToVector :: [Ident] -> Map Ident Double -> LinAlg.Vector Double
paramsToVector ps vs = LinAlg.fromList [ vs Map.! p | p <- ps ]

-- | Joins together all values for all variables.
-- For example { x := [1,2,3], y := [5,6,7] } results in [1,2,3,5,6,7]
-- This is used to estimate residuals
dsToVector :: DataSeries Double -> LinAlg.Vector Double
dsToVector = LinAlg.fromList . concat . Map.elems . values


fitModel ::
  DiffEqs           {- ^ Model to optimize -} ->
  DataSeries Double {- ^ Data to fit -} ->
  Map Ident Double  {- ^ Scaling for resudals, assumed to be 1 if missing -} ->
  Map Ident Double  {- ^ Initial parameter values -} ->
  (Map Ident Double, [ Map Ident Double ])
  {- ^ Optimized parameters, and how we got there -}
fitModel eqs ds scaled start =
    extract
  $ FIT.nlFitting method 1e-4 1e-4 20
      residual
      (residualChange err eqs ds)
      (paramsToVector ps start)

  where
  (method,err)
      | Map.null scaled = (FIT.LevenbergMarquardt, errNoScale)
      | otherwise       = (FIT.LevenbergMarquardtScaled, errScale)

  residual = dsToVector . modelErrorWith err eqs ds . paramsFromVector ps
  ps       = deqParams eqs
  extract (ans,work) = ( paramsFromVector ps ans
                       , map (paramsFromVector ps) (LinAlg.toColumns work)
                       )
  errNoScale = \_ a b -> a - b
  errScale   = \v a b -> (a - b) / Map.findWithDefault 1 v scaled



test :: DiffEqs
test = DiffEqs
  { deqParams  = ["c","d"]
  , deqInitial = Map.singleton "x" (Var "c")
  , deqState   = Map.singleton "x" (Var "d" :*: Var "time")
  , deqLet     = Map.empty
  }

example :: Map Ident Double
example =
  fst $
  fitModel
    test
    (dataSeries ["x"] [ (t,[7+t*t]) | t <- [ 2 .. 100 ] ])
    Map.empty
    (Map.fromList [ ("c",0),("d",0) ])




