module Language.ASKEE.Measure where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Language.ASKEE.Syntax as Syntax
import qualified Language.ASKEE.Core as Core

-- observe exprs and not vars

-- measure single simulation
-- aggregate data from multiple simulation runs
-- summarize aggregate data

type Name = Text
type Time = Double

-- this describes some particular set of states of the model
-- simulation we are particularly interested in - like a filter
data Selector =
    Every
  | Or Selector Selector
  | And Selector Selector
  | TimeGT Double            -- time >
  | TimeLT Double            -- time <
  | OnEvent Text
  | If Core.Expr

-- something we want to track/record about when an event occurs
-- this allows us to summarize the state of the model over th
-- course of the simulation
data Observation =
    TraceExpr Name Core.Expr
  | Accumulate Name Core.Expr Core.Expr

data Measure = Measure Observation Selector

data MeasureResult =
    MeasureResult
      { mrAccumulators :: Map Name Double
      , mrTraces :: [(Time, Map Name Double)]
      }

measureModel :: Syntax.Model -> Double -> [Measure] -> MeasureResult
measureModel model maxTime measures = undefined

data Simulation =
  Simulation
    { simSelector :: Core.Expr -- observations from this run (possibly aggregate stuff from simSingleRun)
    , simSingleRun :: [Observation]
    , simAggregate :: [(Name, Core.Expr)]
    }

data SimulationResult

measureModelManyTimes ::
  Syntax.Model ->
  [Measure] ->
  Int ->
  ([MeasureResult] -> SimulationResult) ->
  SimulationResult
measureModelManyTimes model measures termination summarize = undefined


-- Accumulate "a" (a + 1.0) (0.0)
-- Accumulate "a" (a || something) (False)
