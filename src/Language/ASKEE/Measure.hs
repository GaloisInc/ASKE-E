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

-- observe exprs and not vars

-- measure single simulation
-- aggregate data from multiple simulation runs
-- summarize aggregate data

ppText :: Text.Text -> PP.Doc
ppText = PP.text . Text.unpack

---------


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
  | Accumulate Name Core.Expr Double

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


runnerClassName :: Core.Model -> PP.Doc
runnerClassName model = SG.modelClassName model <> "_Runner"

-- TO EXPLORE: runUntil as an expression
genSimulationRunnerCpp :: Core.Model -> Double -> [Measure] -> PP.Doc
genSimulationRunnerCpp model runUntil measures =
    PP.vcat [ PP.vcat (mkInclude <$> includes)
            , simclass
            , runnerClass
            ]
  where
    simclass = SG.genModel model
    -- TODO: refactor into CPP generating class
    mkInclude n = "#include" <> "<" <> n <> ">"
    includes = ["vector", "utility"]

    runnerClass =
      PP.vcat [ "struct" <+> runnerClassName model <+> "{"
              , PP.nest 2 (attributes $$ methods)
              , "};"
              ]

    declVar ty name = (ty <+> name) <> ";"

    attributes =
      PP.vcat $ declVar (SG.modelClassName model) modelVarName
              : (attributeFor <$> measures)

    methods = PP.vcat [ stepFunc model measures
                      , runFunc model runUntil measures ]


    stateVars = Map.keysSet $ Core.modelInitState model

    attributeFor (Measure obs _) =
      case obs of
        Accumulate name _ initVal -> SG.declareInitStmt "double" (ppText name) (PP.double initVal)
        TraceExpr name _ -> SG.declareStmt "std::vector<std::pair<double, double>>" (ppText name)


{-
void run(uint32_t seed) {
  _model = SIR{};
  _model.set_seed(seed);
  // iniitalize accumulators
  m = 0;
  // reset traces
  while (!endCondion) {
    step();
  }
}

-}

-- XXX: We could generalize the time limit to be a function of
-- the state instead.
runFunc :: Core.Model -> Double -> [Measure] -> PP.Doc
runFunc model end measures = C.functionTemplate' "run" ["uint32_t seed"] "void"$
  [ C.assignStmt modelVarName (SG.modelClassName model <> "{}")
  , C.callFunc (C.member modelVarName SG.setSeed1Name) [ "seed" ] <> ";"
  ] ++
  [ case obs of
      Accumulate name _ initVal -> C.assignStmt (ppText name) (PP.double initVal)
      TraceExpr name _ -> C.callProc (C.member (ppText name) "clear") <> ";"
  | Measure obs _ <- measures
  ] ++
  [ "while" <+> PP.parens (C.member modelVarName SG.timeName <+> "<" <+> PP.double end)
  , PP.nest 2 (C.callProc "step" <> ";")
  ]

stepFunc :: Core.Model -> [Measure] -> PP.Doc
stepFunc model measures = C.functionTemplate' "step" [] "void" $
  [ C.declareStmt "double" "nextTime"
  , C.declareStmt "int" "event"
  , C.callFunc (C.member modelVarName SG.nextEventFunctionName)
            [ "event", "nextTime" ] <> ";"
  , transtionStatements
  , C.callFunc (C.member modelVarName SG.runEventFunctionName)
      [ "event", "nextTime" ] <> ";"
  ] ++
  [ prevValDecl name | Measure (Accumulate name _ _) _ <- measures ] ++
  [ accumulatorUpdate sel name e | Measure (Accumulate name e _) sel <- measures ]

  where
  transtionStatements = PP.empty

  prevValName name = "_prev_" <> ppText name
  prevValDecl name = C.declareInitStmt "double" (prevValName name) (ppText name)

  accumulatorUpdate s name e =
    PP.vcat [ "if" <> PP.parens (genSelector model env s)
            , PP.nest 2 (C.assignStmt (ppText name) (SG.genExpr' env e))
            ]

  stateVars = Map.keysSet (Core.modelInitState model)
  env n | n `Set.member` stateVars = modelVarName <> "." <> SG.stateVarName n
        | otherwise                = prevValName n




modelVarName :: PP.Doc
modelVarName = "_model"

type Env = Core.Ident -> PP.Doc

genSelector :: Core.Model -> Env -> Selector -> PP.Doc
genSelector model env = go
  where
  go s =
    case s of
      If e -> SG.genExpr' env e
      Every -> "true"
      And e1 e2 -> go e1 <+> "&&" <+> go e2
      Or e1 e2 ->  go e1 <+> "||" <+> go e2
      TimeGT d -> (modelVarName <> "." <> SG.timeName) <+> ">" <+> PP.double d
      TimeLT d -> (modelVarName <> "." <> SG.timeName) <+> "<" <+> PP.double d
      OnEvent e -> "_event" <+> "==" <+> PP.int (SG.eventNum (Core.modelEvents model) e)












-- Accumulate "a" (a + 1.0) (0.0)
-- Accumulate "a" (a || something) (False)
