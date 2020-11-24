
{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-# Language ParallelListComp #-}

module Language.ASKEE.DiffEq.DiffEqC where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax(unsafeTExpCoerce,unTypeQ)
import qualified Numeric.LinearAlgebra.Data as LinAlg
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)

import qualified Numeric.GSL.ODE as ODE

import Language.ASKEE.Expr (Expr(..))
import Language.ASKEE.DiffEq.DiffEq
import qualified Language.ASKEE.Syntax as Syntax



compileExpr :: Expr -> Map Text (Q (TExp Double)) -> Q (TExp Double)
compileExpr expr nameMap =
  case expr of
    Var v -> nameMap Map.! v

    LitD l -> [||  l ||]

    Add e1 e2 ->
      [|| $$(doSub e1) + $$(doSub e2) ||]

    Sub e1 e2 ->
      [|| $$(doSub e1) - $$(doSub e2) ||]

    Mul e1 e2 ->
      [|| $$(doSub e1) * $$(doSub e2) ||]

    Div e1 e2 ->
      [|| $$(doSub e1) / $$(doSub e2) ||]

    Neg e1 ->
      [|| negate ($$(doSub e1)) ||]

  where
  doSub e0 = compileExpr e0 nameMap

compileDiffEq :: [DiffEq] ->
          Q (TExp (Double -> [Double] -> [Double]))
compileDiffEq eqs =
  [|| \t xs ->
      let env = LinAlg.fromList xs
      -- XXX: Time is not part of the state?
      in $$(do let nameMap = Map.fromList $ [ (n, [|| env LinAlg.! i ||])
                                          | StateEq n _ <- eqs'
                                          | i <- [0 ..]
                                          ]
               mkList [ compileExpr (deExpr e) nameMap | e <- eqs' ]
            )
  ||]
  where
  eqs' = inlineNonStateVars eqs

mkList :: [Q (TExp Double)] -> Q (TExp [Double])
mkList = unsafeTExpCoerce . listE . map unTypeQ

compileModelSim :: Syntax.Model -> Q (TExp ([Double] -> Map Text [Double] ))
compileModelSim mdl =
  case asEquationSystem mdl of
    Left err -> fail err
    Right (eqns,icE) ->
      do let stateVars = fst <$> stateEqs eqns
             gslode    = compileDiffEq eqns
             ic = mkList [ compileExpr (icE Map.! x) Map.empty | x <- stateVars]

         [|| \time ->
                let res  = ODE.odeSolve $$gslode $$ic (LinAlg.fromList time)
                    rows = LinAlg.toList <$> LinAlg.toColumns res
                in Map.delete "time" $ Map.fromList (stateVars `zip` rows) ||]



