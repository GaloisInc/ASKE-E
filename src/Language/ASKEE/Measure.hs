{-# Language OverloadedStrings #-}
module Language.ASKEE.Measure where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Language.ASKEE.Syntax as Syntax
import qualified Language.ASKEE.Core as Core
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint((<+>), ($$))
import qualified Language.ASKEE.SimulatorGen as SG
import qualified Language.ASKEE.SimulatorGen as C
import qualified Data.Text as Text
import qualified Data.Set as Set


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

data Observation =
    TraceExpr Name Core.Expr
  | Accumulate Name Double Core.Expr
    deriving Show

-- | Desribes a collection of points in time.
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


