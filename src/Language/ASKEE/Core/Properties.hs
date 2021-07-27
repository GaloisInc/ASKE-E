module Language.ASKEE.Core.Properties where

import qualified Data.Set as Set
import qualified Language.ASKEE.Core.Syntax as Core
import qualified Language.ASKEE.Core.Expr as CoreExpr
import qualified Data.Map as Map

-- | Determine if a model has a fixed total population across all state variables
populationLimit :: Core.Model -> Maybe Double
populationLimit m =
    if all isEventConservative (Core.modelEvents m)
      then mbPopulation
      else Nothing
  where
    inlinedModel = Core.inlineLets . Core.inlineParams $ m

    mbPopulation =
      case Map.elems (Core.modelInitState inlinedModel) of
        [] -> Nothing
        e:l ->
          case nonemptySum e l of
            CoreExpr.NumLit v -> Just v
            _ -> Nothing

    nonemptySum e l = CoreExpr.simplifyExpr (foldr (CoreExpr.:+:) e l)
    mkEventExp (v, e) = e CoreExpr.:-: CoreExpr.Var v

    isEventConservative evt =
      case mkEventExp <$> Map.toList (Core.eventEffect evt) of
        [] -> True
        e:l' -> nonemptySum e l' ==  CoreExpr.NumLit 0

-- | Make all population limits explicit in the model
withExplicitPopulationLimits :: Core.Model -> Maybe Core.Model
withExplicitPopulationLimits m =
  do  pop <- populationLimit m
      pure $ m { Core.modelEvents = mkLimitedEvent pop <$> Core.modelEvents m }
  where
    popCmp pop var = CoreExpr.Var var CoreExpr.:<: CoreExpr.NumLit pop
    popLimits pop evt =
      popCmp pop <$> Map.keys (Core.eventEffect evt)
    mkLimitedEvent pop evt =
      evt { Core.eventWhen = foldr (CoreExpr.:&&:) (Core.eventWhen evt)  (popLimits pop evt) }

-- | Try to simplify any expression involving sums of state variables if a population constraint can be derived
simplifyPopulations :: Core.Model -> Maybe Core.Model
simplifyPopulations m =
  do  pop <- populationLimit m
      pure $ CoreExpr.mapExprs (simpl pop) m
  where
    varTerms = CoreExpr.Op1 CoreExpr.Neg . CoreExpr.Var <$> Core.modelStateVars m
    popSum pop = foldr (CoreExpr.:+:) (CoreExpr.NumLit pop) varTerms
    simplified pop e = CoreExpr.simplifyExpr $ CoreExpr.toFrom (e CoreExpr.:+: popSum pop)
    simpl pop e | simplified pop e `isSimpler` e = simplified pop e
                | otherwise = e
    isSimpler a b = Set.size (CoreExpr.collectExprVars a) < Set.size (CoreExpr.collectExprVars b)

