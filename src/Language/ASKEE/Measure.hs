{-# Language OverloadedStrings #-}
module Language.ASKEE.Measure where

import Data.Text(Text)
import qualified Language.ASKEE.Core as Core


type Name = Text

data Measure =
    EventBased Statement
    -- ^ Measurements that happen at a sepcific event

  | TimeBased TimePoints Statement
    -- ^ Measurements that happen at specific points in type

  | Measure :+: Measure
    -- ^ Multiple measures.
    deriving Show


-- | Desribes a guarded sets of observations.
data Statement =
    When Selector Statement
  | Statement :> Statement
  | Do Observation
    deriving Show

-- | Desribes a concrete thing that we are computing.
data Observation =
    TraceExpr Name Core.Expr
    -- ^ Save information about a particular run
    -- The result is a time series of the values of the expression within
    -- an individual run.

  | Accumulate Name Double Core.Expr
    -- ^ Aggreagate information within a single run
    -- The result is a scalar containing the aggregated value at the
    -- end of a run.

  | TraceGlobal Name Double Core.Expr
    -- ^ Save information across multiple runs.
    -- Must be synchronized on time.
    -- The result is a time series containing an aggregate of the
    -- values of the expression at a sepcific time point, across all runs.
    deriving Show


-- | Desrcibes a collection of points in time.
data TimePoints =
    AtTime Double
  | AtTimes Double Double Double -- ^ from, step, to
  | TimePoints :&: TimePoints    -- ^ multiple time points
    deriving Show

-- | This describes some particular set of states of the model
-- simulation we are particularly interested in - like a filter
data Selector =
    Every
  | Or Selector Selector
  | And Selector Selector
  | TimeGT Double            -- time >
  | TimeLT Double            -- time <
  | OnEvent Text
  | If Core.Expr
    deriving Show


measureObservations :: Measure -> [Observation] -> [Observation]
measureObservations m =
  case m of
    m1 :+: m2     -> measureObservations m1 . measureObservations m2
    TimeBased _ s -> statementObservations s
    EventBased s  -> statementObservations s

statementObservations :: Statement -> [Observation] -> [Observation]
statementObservations s =
  case s of
    When _ s1 -> statementObservations s1
    s1 :> s2  -> statementObservations s1 . statementObservations s2
    Do o      -> (o :)


