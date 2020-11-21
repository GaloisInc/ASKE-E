{-# Language OverloadedStrings, BlockArguments #-}
module Language.ASKEE.MeasureToCPP where

import qualified Data.Map as Map

import qualified Language.ASKEE.Core as Core
import           Language.ASKEE.Measure
import qualified Language.ASKEE.C as C
import qualified Language.ASKEE.SimulatorGen as SG


--------------------------------------------------------------------------------
-- API names

runnerClassName :: Core.Model -> C.Doc
runnerClassName model = "Runner" -- SG.modelClassName model <> "_Runner"
-- We just use a fixed name for the moment, which makes it easier to
-- write generic wrapper code

modelVarName :: C.Doc
modelVarName = C.ident "_model"




--------------------------------------------------------------------------------

genSimulationRunnerCpp :: Core.Model -> Double -> Measure -> C.Doc
genSimulationRunnerCpp model runUntil measure =
  C.stmts [ C.stmts (C.include <$> includes)
          , SG.genModel model
          , C.struct (runnerClassName model) (attributes ++ methods)
          ]
  where
  includes = ["vector", "utility", "math.h"]

  observations = measureObservations measure []

  attributes =
     C.declare (SG.modelClassName model) modelVarName
   : (attributeFor <$> observations)

  methods =
    [ stepFunc model measure
    , runFunc model runUntil observations
    ]

  attributeFor obs =
    case obs of
      Accumulate name initVal _ ->
          C.declareInit C.double (C.ident name) (C.doubleLit initVal)
      TraceExpr name _ ->
        C.declare "std::vector<std::pair<double, double>>" (C.ident name)
      TraceGlobal name initVal _ ->
        C.declare "std::vector<std::pair<double, double>>" (C.ident name)


runFunc :: Core.Model -> Double -> [Observation] -> C.Doc
runFunc model end obses =
  C.function C.void "run" [ C.arg "uint32_t" "seed"]
  $  [ C.assign modelVarName (C.callCon (SG.modelClassName model) [])
     , C.callStmt (C.member modelVarName SG.setSeed1Name) [ "seed" ]
     ]
  ++ (resest <$> obses)
  ++ [ C.while (C.member modelVarName SG.timeName C.< C.doubleLit end)
        [ C.callStmt "step" [] ] ]

  where
  resest obs =
    case obs of
      Accumulate name initVal _ -> C.assign (C.ident name) (C.doubleLit initVal)
      TraceExpr name _ -> C.callStmt (C.member (C.ident name) "clear") []
      TraceGlobal {} -> C.nop

stepFunc :: Core.Model -> Measure -> C.Doc
stepFunc model measure =
  C.function C.void "step" []
    $  [ C.declare C.double "nextTime"
       , C.declare C.int    "event"
       , C.callStmt (C.member modelVarName SG.nextEventFunctionName)
                                                [ "event", "nextTime" ]
       ]
    ++ genTimeBasedMeasure model measure []
    ++ [ C.callStmt (C.member modelVarName SG.runEventFunctionName)
                                                    [ "event", "nextTime" ]
       ]
    ++ genEventMeasure model measure


genTimeBasedMeasure :: Core.Model -> Measure -> [C.Doc] -> [C.Doc]
genTimeBasedMeasure model measure =
  case measure of
    m1 :+: m2 -> genTimeBasedMeasure model m1 . genTimeBasedMeasure model m2
    EventBased {} -> id
    TimeBased pts s -> (C.scope code :)
      where
      env n
        | n == "time"             = C.ident "time"
        | Core.isStateVar n model = C.member modelVarName (SG.stateVarName n)
        | otherwise               = C.ident n

      code =
        [ C.declareInit C.double "time" (C.member modelVarName SG.timeName)
        , C.lineComment "XXX: This is to deal with edge cases, maybe change"
        , C.ifThen ("time" C.== C.doubleLit 0)
                                        [ C.assign "time" (C.doubleLit (-1)) ]
        ] ++
        [ C.doWhile
            (genNextTime pts ++ genStatement model env s [])
            ("time" C.<= "nextTime")
        ]




-- bool nextTime(double time, double &nextTime) 
genNextTime :: [TimePoints] -> [C.Doc]
genNextTime tps =
  case tps of
    [] -> [ C.break ]
    t : more ->
      case t of
        AtTime d ->
          [ C.ifThenElse ("time" C.< C.doubleLit d)
             [ C.assign "time" (C.doubleLit d)
             ]
             (genNextTime more)
          ]
        AtTimes start' step' end' ->
          let start = C.doubleLit start'
              step  = C.doubleLit step'
              end   = C.doubleLit end'
          in
          [ C.ifThenElse ("time" C.< start)
              [ C.assign "time" start
              ]
              [ C.ifThenElse ("time" C.< end)
                  -- start + step * (1 + floor ((time - start) / step))
                  [ C.assign "time"
                    let approx = C.parens ("time" C.- start) C./ step
                        steps = C.doubleLit 1 C.+ C.call "floor" [approx]
                    in start C.+ step C.* C.parens steps
                  ]
                  (genNextTime more)
             ]
          ]




genEventMeasure :: Core.Model -> Measure -> [C.Doc]
genEventMeasure model measure =
  -- we should only need to save the vars in the event based part
  -- but for simplicity we store everything
  [ C.declareInit C.double (prevValName x) (C.ident x)
                  | Accumulate x _ _ <- measureObservations measure []
  ] ++ upd measure []
  where
  upd :: Measure -> [C.Doc] -> [C.Doc]
  upd m =
    case m of
      m1 :+: m2    -> upd m1 . upd m2
      TimeBased {} -> id
      EventBased s -> genStatement model env s

  prevValName name = C.ident ("_prev_" <> name)

  env n
    | n `Map.member` Core.modelInitState model =
                      C.member modelVarName (SG.stateVarName n)
    | n == "time"   = C.member modelVarName SG.timeName
    | otherwise     = prevValName n


genStatement :: Core.Model -> SG.Env -> Statement -> [C.Doc] -> [C.Doc]
genStatement model env s =
  case s of
    When sel s1 ->
     (C.ifThen (genSelector model env sel) (genStatement model env s1 []) :)
    s1 :> s2 -> genStatement model env s1 . genStatement model env s2
    Do obs -> (genObservation model env obs :)


genObservation :: Core.Model -> SG.Env -> Observation -> C.Doc
genObservation model env obs =
  case obs of
    TraceExpr x e    -> C.callStmt (C.member (C.ident x) "push_back")
                                 [ C.braces [ C.member modelVarName SG.timeName
                                            , SG.genExpr' env e
                                            ]
                                 ]
    Accumulate x _ e -> C.assign (C.ident x) (SG.genExpr' env e)


genSelector :: Core.Model -> SG.Env -> Selector -> C.Doc
genSelector model env = go
  where
  go s =
    case s of
      If e -> SG.genExpr' env e
      Every -> C.boolLit True
      And e1 e2 -> go e1 C.&& go e2
      Or e1 e2 ->  go e1 C.|| go e2
      TimeGT d -> C.member modelVarName SG.timeName C.> C.doubleLit d
      TimeLT d -> C.member modelVarName SG.timeName C.< C.doubleLit d
      OnEvent e ->
        "_event" C.== C.intLit (SG.eventNum (Core.modelEvents model) e)





