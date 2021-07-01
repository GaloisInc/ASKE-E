{-# Language BlockArguments, OverloadedStrings, PatternSynonyms #-}
module Language.ASKEE.DEQ.Simulate where


import           Data.Map  ( Map )
import qualified Data.Map  as Map
import           Data.List ( transpose )

import Language.ASKEE.Core.Expr   ( mapExprs
                                  , substExpr
                                  , Expr(..), pattern (:-:), pattern NumLit
                                  , Ident, asText )
import Language.ASKEE.Core.Eval   ( evalDouble )
import Language.ASKEE.DataSeries  ( foldDataSeries
                                  , zipAligned
                                  , zipAlignedWithTimeAndLabel
                                  , DataSeries(..) )
import Language.ASKEE.DEQ.Syntax  ( DiffEqs(..), addParams )

import qualified Numeric.LinearAlgebra.Data as LinAlg
import qualified Numeric.GSL.ODE            as ODE
import qualified Numeric.GSL.Fitting        as Fit
import Data.Text (Text)

evalDiffEqs ::
  [Ident] -> [Expr] -> Map Ident Double -> Double -> [Double] -> [Double]
evalDiffEqs xs es ps t s = [ evalDouble e env | e <- es ]
  where
  env = Map.insert "time" t (Map.fromList (zip xs s)) `Map.union` ps

simulate :: DiffEqs -> Map Ident Double -> [Double] -> DataSeries Double
simulate eqs paramVs ts =
  DataSeries { times = ts
             , values = Map.fromList (zip observableNames (map upd rows))
             }
  where
  (stateNames,stateInitExprs) = unzip (Map.toList (deqRates eqs'))
  letList                     = Map.toList (deqLets eqs')
  (letNames,letInitExprs)     = unzip letList

  observableNames = stateNames ++ letNames
  observableInitExprs = stateInitExprs
                     ++ map (\(letName, letExpr) -> letExpr :-: Var letName)
                            letList

  initMap = Map.map (\e -> evalDouble e paramVs) (deqInitial eqs')
  initS   = Map.elems initMap
         ++ [ evalDouble l (paramVs `Map.union` initMap)
            | l <- letInitExprs ]

  resMatrix = ODE.odeSolve
                (evalDiffEqs observableNames observableInitExprs paramVs)
                initS
                (LinAlg.fromList ts')

  -- we add time 0, because our initial state is for time 0
  -- also we assume no -ve times
  (upd,ts') = case ts of
                t:_ | t > 0 -> (tail,0 : ts)
                _           -> (id,ts)

  rows   = LinAlg.toList <$> LinAlg.toColumns resMatrix
  eqs'   = mapExprs inlineEqParams eqs
  params = foldr Map.delete (Map.mapMaybe id (deqParams eqs')) (Map.keys paramVs)
  inlineEqParams = substExpr params



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
computeErrorPerVar errs = postProc <$> foldDataSeries (+) startErr errs
  where
  startErr = const 0 <$> values errs
  numPts   = fromIntegral (length (times errs))
  postProc = (/numPts) . sqrt


fitModel ::
  DiffEqs           {- ^ Model to optimize -} ->
  DataSeries Double {- ^ Data to fit -} ->
  Map Ident Double  {- ^ Scaling for resudals, assumed to be 1 if missing -} ->
  Map Ident Double  {- ^ Initial parameter values -} ->
  (Map Text (Double,Double), [ Map Text Double ])
  -- ^ Returns (result, search path)
  -- where result is the computed value for each parameter,
  -- and the corresponding errror
fitModel eqs ds scaled start =
  ( psFromVec slns
  , map (psFromVec . LinAlg.toList) (LinAlg.toColumns path)
  )
  where
  (slns,path)   = Fit.fitModelScaled 1e-6 1e-6 20 (model,deriv) dt initParams
  ps            = map asText $ Map.keys $ deqParams eqs'
  initParams    = psToVec start
  psToVec vs    = [ vs Map.! p | p <- ps ]
  psFromVec vs  = Map.fromList (ps `zip` vs)

  dt            = [ (x,(vs,s))
                  | (x,vs) <- Map.toList (values ds)
                  , let s = case Map.lookup x scaled of
                              Nothing -> 1
                              Just m  -> 1 / m
                  ]

  simWith vs = simulate eqs' vs (times ds)

  model pvec = let ans = simWith (psFromVec pvec)
               in \x -> values ans Map.! x

  deriv vs =
    let pmap     = psFromVec vs
        here     = simWith pmap
        delta    = 1e-4
        change p = fmap (/ delta)
                 $ zipAligned (-) (simWith (Map.adjust (+delta) p pmap)) here
        changes  = map change ps
    in \x -> transpose [ values pch Map.! x | pch <- changes ]

  eqs' = addParams (Map.map (Just . NumLit) start) eqs
