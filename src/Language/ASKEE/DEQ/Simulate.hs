{-# Language BlockArguments, OverloadedStrings, PatternSynonyms #-}
module Language.ASKEE.DEQ.Simulate where

import           Data.Map  ( Map )
import qualified Data.Map  as Map
import           Data.Set   ( Set )
import qualified Data.Set  as Set
import           Data.List ( transpose )

import Language.ASKEE.Core.Expr   ( substExpr, simplifyExpr
                                  , Expr(..), pattern (:-:), pattern NumLit
                                  , collectExprVars
                                  , orderDecls
                                  , Ident, asText )
import Language.ASKEE.Core.Eval   ( evalDouble )
import Language.ASKEE.DataSeries  ( foldDataSeries
                                  , zipAligned
                                  , zipAlignedWithTimeAndLabel
                                  , DataSeries(..) )
import Language.ASKEE.DEQ.Syntax  ( DiffEqs(..), addParams )
import Language.ASKEE.Panic (panic)
-- import Language.ASKEE.DEQ.Print

import qualified Numeric.LinearAlgebra.Data as LinAlg
import qualified Numeric.GSL.ODE            as ODE
import qualified Numeric.GSL.Fitting        as Fit
import Data.Text (Text)
import qualified Data.Text as Text


evalDiffEqs ::
  [Ident] -> [Expr] -> Double -> [Double] -> [Double]
evalDiffEqs xs es t s = [ evalDouble e env | e <- es ]
  where
  env = Map.insert "time" t (Map.fromList (zip xs s))


getParams ::
  DiffEqs ->
  Map Ident Double {- ^ Overwrites for parameters -} ->
  Map Ident Double
getParams eqs val = fin
  where
  evalP :: Ident -> Maybe Expr -> Double
  evalP x mb = case Map.lookup x val of
                 Just d -> d
                 Nothing ->
                   case mb of
                     Just e  -> evalDouble e fin
                     Nothing -> 0   -- XXX: unspecified
  fin :: Map Ident Double
  fin = Map.mapWithKey evalP (deqParams eqs)

specializeDiffEqs :: Map Ident Double -> DiffEqs -> DiffEqs
specializeDiffEqs paramVs eqs =
  DiffEqs { deqParams  = mempty
          , deqInitial = spec <$> deqInitial eqs
          , deqRates   = spec <$> deqRates  eqs
          , deqLets    = spec <$> deqLets eqs
          }
  where
  su   = NumLit <$> paramVs
  spec = simplifyExpr . substExpr su


pruneEqns :: Set Ident -> DiffEqs -> DiffEqs
pruneEqns xs eqs
  | Set.null xs = eqs
  | otherwise =    eqs { deqInitial = restrict deqInitial
                       , deqLets    = restrict deqLets
                       , deqRates   = restrict deqRates
                       }
  where

  restrict f = Map.restrictKeys (f eqs) vars
  vars = complete xs

  deps = Map.fromList
           [ (x, collectExprVars e)
           | (x,e) <- Map.toList (deqRates eqs) ++ Map.toList (deqLets eqs)
           ]

  depsofSet ys = Set.unions
                  [ Map.findWithDefault mempty y deps | y <- Set.toList ys ]

  complete vs =
    let ds = depsofSet vs
    in if ds `Set.isSubsetOf` vs then vs else complete (Set.union ds vs)


{- | This just makes up fresh variables for all states.
It was mostly used to investigate automatically synthesized
models with rather unreasonable variable names. -}
renameHack :: DiffEqs -> DiffEqs
renameHack eqs =
  eqs { deqInitial = Map.fromList [ (names Map.! x, e) | (x,e) <- Map.toList (deqInitial eqs) ]
      , deqRates = Map.fromList [ (names Map.! x, change e) | (x,e) <- Map.toList (deqRates eqs) ]
      , deqLets = change <$> deqLets eqs
      }
  where
  change = substExpr su
  su = Var <$> names
  names = Map.fromList
          [ (x, Text.pack ("S" <> show n))
          | (n,x) <- zip [1 :: Int ..] (Map.keys (deqRates eqs)) ]


simulate ::
  DiffEqs ->
  Map Ident Double {- ^ Parameters -} ->
  Set Ident {- ^ Variables to simuate; [] means all -} ->
  [Double] {- ^ Times of interest -} ->
  DataSeries Double
simulate eqs paramVs vars ts =
  DataSeries { times = ts
             , values = Map.fromList (zip observableNames (map upd rows))
             }
  where
  (stateNames,stateInitExprs) = unzip (Map.toList (deqRates eqs'))
  letList                     = orderDecls (Map.toList (deqLets eqs'))
  (letNames,letExprs)         = unzip letList

  observableNames = stateNames ++ letNames
  observableInitExprs = stateInitExprs
                     ++ zipWith (:-:) letExprs (map Var letNames)

  initMap           = (`evalDouble` mempty) <$> deqInitial eqs'
  initEnv           = foldl evalLet initMap letList
  evalLet env (x,e) = Map.insert x (evalDouble e env) env
  initS             = mapLookup initEnv <$> observableNames
  mapLookup x       = lookupPanic "simulate" ["Unknown observable: " ++ show x] x

  resMatrix = ODE.odeSolve
                (evalDiffEqs observableNames observableInitExprs)
                initS
                (LinAlg.fromList ts')

  -- we add time 0, because our initial state is for time 0
  -- also we assume no -ve times
  (upd,ts') = case ts of
                t:_ | t > 0 -> (tail,0 : ts)
                _           -> (id,ts)

  rows   = LinAlg.toList <$> LinAlg.toColumns resMatrix

  eqs'   = let eqs1 = pruneEqns vars eqs
           in specializeDiffEqs (getParams eqs1 paramVs) eqs1


-- | Compute the difference between the model and the data, using
-- the provided functions.
modelErrorWith ::
  (Ident -> Double -> Double -> Double) {- ^ how to compute difference -} ->
  DiffEqs -> DataSeries Double -> Map Ident Double -> DataSeries Double
modelErrorWith err eqs expected ps =
  zipAlignedWithTimeAndLabel err' (simulate eqs ps mempty (times expected)) expected
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
  psToVec vs    = mapLookup "psToVec" vs <$> ps
  psFromVec vs  = Map.fromList (ps `zip` vs)

  dt            = [ (x,(vs,s))
                  | (x,vs) <- Map.toList (values ds)
                  , let s = case Map.lookup x scaled of
                              Nothing -> 1
                              Just m  -> 1 / m
                  ]

  simWith vs = simulate eqs' vs mempty (times ds)

  model pvec = let ans = simWith (psFromVec pvec)
               in mapLookup "model answer" (values ans)

  deriv vs =
    let pmap     = psFromVec vs
        here     = simWith pmap
        delta    = 1e-4
        change p = fmap (/ delta)
                 $ zipAligned (-) (simWith (Map.adjust (+delta) p pmap)) here
        changes  = map change ps
    in \x -> transpose [ mapLookup "deriv" (values pch) x | pch <- changes ]


  -- Transform the given eqs by specializing to all the parameters
  -- that we're _not_ trying to fit, then add as parameters the params in
  -- @start@
  eqs' = addParams (Map.map (Just . NumLit) start) specialized
  specialized  = specializeDiffEqs toSpecialize eqs
  toSpecialize = Map.filterWithKey (\p _ -> Map.notMember p start) (getParams eqs mempty)

  mapLookup who m x = lookupPanic "fitModel" [who, "Unknown parameter: " ++ show x] m x

lookupPanic :: Ord k => String -> [String] -> Map k a -> k -> a
lookupPanic who msg m x =
  case Map.lookup x m of
    Just v  -> v
    Nothing -> panic who msg
