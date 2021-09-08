{-# Language OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.DEQ.Convert where

import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as Map

import           Language.ASKEE.Core.Expr
import           Language.ASKEE.Core.Syntax ( Event(..), Model(..) )
import qualified Language.ASKEE.DEQ.Syntax  as Src
import           Data.List ( sort, foldl' )
import           Data.Tuple.Extra ( uncurry3 )

asCore :: Src.DiffEqs -> Model
asCore mdl =
  Model { modelName = ""
        , modelParams = Src.deqParams mdl
        , modelInitState = Src.deqInitial mdl
        , modelLets = Src.deqLets mdl
        , modelMeta = Map.empty
        , modelEvents = coreEvents (Src.deqRates mdl)
        }

coreEvents :: Map Ident Expr -> [Event]
coreEvents dRates = map event indexedRateMap
  where
    differentials = concatMap (uncurry $ splitDifferential FlowPlus) $ Map.toList dRates
    rateExprs = map (uncurry3 splitRateExpr) differentials
    indexedRateMap = zip ([1..] :: [Int]) (Map.toList rateMap)
    rateMap = foldl' rateFoldFn Map.empty rateExprs
    rateFoldFn mp (fd, sv, m, e) = Map.insertWith (++) e [(fd, sv, m)] mp
    event (i, (e, effects)) =
      let eventName = T.append "E" (T.pack $ show i)
          eventRate = e
          eventWhen = BoolLit True -- TODO: What should we do about guards?
          eventEffect = Map.fromList $ map eventExpr effects
      in Event{..}
    eventExpr (fd, sv, m) = (sv, if fd == FlowPlus then Var sv :+: NumLit m else Var sv :-: NumLit m)

data FlowDirection = FlowPlus | FlowMinus
  deriving (Eq, Ord, Show)

splitDifferential :: FlowDirection -> Ident -> Expr -> [(FlowDirection, Ident, Expr)]
splitDifferential fd sv e =
  case e of
    e1 :+: e2 -> splitDifferential fd sv e1 ++ splitDifferential fd sv e2
    e1 :-: e2 -> splitDifferential fd sv e1 ++ splitDifferential (otherDir fd) sv e2
    _         -> [(fd, sv, e)]
  where
      otherDir FlowPlus = FlowMinus
      otherDir FlowMinus = FlowPlus

splitRateExpr :: FlowDirection -> Ident -> Expr -> (FlowDirection, Ident, Double, Expr)
splitRateExpr fd sv e = (fd, sv, multiplier, canonicalExpr)
  where
    (multiplier, mExprs, dExprs) = splitExpr e
    mult es = simplifyExpr $ foldr (:*:) (NumLit 1) es
    canonicalExpr =
        let numerator = mult (sort mExprs)
            denominator = mult (sort dExprs)
        in if not (null dExprs) then numerator :/: denominator else numerator

splitExpr :: Expr -> (Double, [Expr], [Expr])
splitExpr e =
  case e of
    Literal (Num v) :*: e' ->
      let (m, mExprs, dExprs) = splitExpr e'
      in (v * m, mExprs, dExprs)
    e' :*: Literal (Num v)  ->
      let (m, mExprs, dExprs) = splitExpr e'
      in (v * m, mExprs, dExprs)
    e' :*: e'' ->
      let (m, mExprs, dExprs) = splitExpr e'
          (m', mExprs', dExprs') = splitExpr e''
      in (m * m', mExprs ++ mExprs', dExprs ++ dExprs')
    e' :/: e'' ->
      let (m, mExprs, dExprs) = splitExpr e'
          (m', mExprs', dExprs') = splitExpr e''
      in (m / m', mExprs ++ dExprs', dExprs ++ mExprs')
    Op1 Neg e' ->
      let (m, mExprs, dExprs) = splitExpr e'
      in (-1 * m, mExprs, dExprs)
    _ -> (1, [e], [])
