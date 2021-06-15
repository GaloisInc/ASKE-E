{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.CPP.SimulatorGen where

import qualified Language.ASKEE.Core.Expr as Core
import qualified Language.ASKEE.Core.Syntax as Core
import           Data.Text(Text)
import qualified Data.Map as Map

import Language.ASKEE.Panic(panic)

import qualified Language.ASKEE.CPP.Pretty as C
import Data.List (intercalate)

--------------------------------------------------------------------------------
-- API Names

eventEffectFuncName :: Core.Event -> C.Doc
eventEffectFuncName evt = C.ident ("event_effect_" <> Core.eventName evt)

eventRateName :: Core.Event -> C.Doc
eventRateName evt = C.ident ("event_rate_" <> Core.eventName evt)

eventWhenName :: Core.Event -> C.Doc
eventWhenName evt = C.ident ("event_when_" <> Core.eventName evt)

stateVarName :: Text -> C.Doc
stateVarName nm =
  if nm == "time"
    then timeName
    else C.ident nm

nextEventFunctionName :: C.Doc
nextEventFunctionName = C.ident "next_event"

runEventFunctionName :: C.Doc
runEventFunctionName = C.ident "run_event"

modelClassName :: Core.Model -> C.Doc
modelClassName mdl = C.ident (Core.modelName mdl)

rngName :: C.Doc
rngName = C.ident "prng"

timeName :: C.Doc
timeName = C.ident "time"

setSeed1Name :: C.Doc
setSeed1Name = C.ident "set_seed"

randEngineType :: C.Doc
randEngineType = C.ident "std::mt19937_64"
--------------------------------------------------------------------------------




type Env = Core.Ident -> C.Doc


genIncludes :: C.Doc
genIncludes = C.stmts [ C.include i | i <- includes ]
  where
  includes =
    [ "random"
    , "stdint.h"
    , "math.h"
    , "iostream"
    ]

eventNum :: [Core.Event]-> Core.Ident -> Int
eventNum events name =
  case lookup name indexedEventList of
    Just i  -> i
    Nothing -> error ("[BUG] did not find event named " ++
                                                show name ++ " in model")
  where
    indexedEventList = (Core.eventName <$> events) `zip` [1..]

genExecStep :: Core.Model-> C.Doc
genExecStep mdl =
  C.function C.void runEventFunctionName [ C.arg C.int    "nextEvent"
                                         , C.arg C.double "nextTime" ]
  $ [ C.assign timeName "nextTime" ] ++
    [ C.declareInit C.auto (stateVarCurName v) (stateVarName v)
                                           | v <- Core.modelStateVars mdl ] ++
    [ C.switch "nextEvent" (mkCase <$> evts)
    ]


  where
  evts = Core.modelEvents mdl
  stateVarCurName sv =
    if sv == "time"
      then timeName
      else "cur_" <> stateVarName sv
  mkCase evt =
    C.stmts
     [ C.case' (C.intLit (eventNum evts (Core.eventName evt)))
     , C.nested $
       [ C.assign (stateVarName n) (genExpr' stateVarCurName expr)
                                | (n,expr) <- Map.toList (Core.eventEffect evt)
       ] ++
       [ C.break ]
     ]


genNextStep :: [Core.Event] -> C.Doc
genNextStep evts =
  C.function C.void nextEventFunctionName [ C.refArg C.int    "nextEvent"
                                          , C.refArg C.double "nextTime" ]
  $ [ C.declareInit C.double (effRateName evt) (effRateExpr evt)
                                                          | evt <- evts ] ++

    [ C.declareInit C.double "total_rate" (foldr1 (C.+) (effRateName <$> evts))
    , C.declareInit C.auto "rate_dist"
        (C.callCon "std::uniform_real_distribution<double>"
                                                      [ "0.0", "total_rate" ])

    , C.declareInit C.auto "dt_dist"
        (C.callCon "std::exponential_distribution<double>" ["total_rate"])

    , C.declareInit C.double "random" (C.call "rate_dist" [rngName])

    , C.assign "nextTime" (timeName C.+ C.call "dt_dist" [rngName])
    ] ++
    concatMap runEffectCondStmt evts

  where
  runEffectCondStmt evt =
    [ "random" C.-= effRateName evt
    , C.ifThen ("random" C.<= "0.0")
        [ C.assign "nextEvent" (C.intLit (eventNum evts (Core.eventName evt)))
        , C.return
        ]
    ]

  effRateName evt = C.ident ("_eff_rate_" <> Core.eventName evt)
  effRateExpr evt = C.cond (C.call (eventWhenName evt) [])
                           (C.call (eventRateName evt) [])
                           "0.0"



genModel :: Core.Model -> C.Doc
genModel mdl
  | not $ null $ Core.modelParams mdl =
    panic "genModel" [ "Model parameters not yet supported." ]
  | otherwise =
  C.stmts
    [ genIncludes
    , C.struct (modelClassName mdl')
         $ (mkEventWhenDecl   <$> Core.modelEvents mdl')
        ++ (mkEventRateDecl   <$> Core.modelEvents mdl')
        ++ (mkEventEffectDecl <$> Core.modelEvents mdl')

        ++ [ genNextStep (Core.modelEvents mdl')
           , genExecStep mdl'
           , setSeedFunc
           ]

        ++ [ C.declareInit C.double (stateVarName v) (mkInit val)
           | (v,val) <- Map.toList (Core.modelInitState mdl')
           ]
        
        ++ [ C.declareInit C.double (stateVarName v) (C.doubleLit l)
           | (v, Core.NumLit l) <- Map.toList (Core.modelLets mdl')]

        ++ [ C.declareInit C.double timeName (C.doubleLit 0.0)
           , C.declare randEngineType rngName
           ]

        ++ [ mkPrintFn [ v | v <- Core.modelStateVars mdl' ] ]
    ]
  where
  mkInit = genExpr' \x -> panic "genModel"
                            [ "Unexpected vairable in initial condition:"
                            , show x ]

  mkEventEffectDecl evt =
    C.function C.void (eventEffectFuncName evt) []
      [ C.assign (stateVarName nm) (genExpr e)
      | (nm,e) <- Map.toList (Core.eventEffect evt)
      ]

  mkEventWhenDecl evt =
    C.function C.bool (eventWhenName evt) []
      [ C.returnWith (genExpr (Core.eventWhen evt)) ]

  mkEventRateDecl evt =
    C.function C.double (eventRateName evt) []
      [ C.returnWith (genExpr (Core.eventRate evt)) ]

  setSeedFunc =
    C.function C.void setSeed1Name  [ C.arg "uint32_t" "seed" ]
      [ C.stmt (C.call (C.member rngName "seed") ["seed"]) ]

  mkPrintFn vs = C.function C.void (C.ident "print") [] $
    cout (C.stringLit "{") :
    intercalate [cout (C.stringLit ",")] (map printV ("time":vs)) ++
    [cout (C.stringLit "}")]
    where
      printV :: Core.Ident -> [C.Doc]
      printV v = 
        [ cout (quotedStrLit v)
        , cout (C.stringLit ":")
        , cout (C.ident v)
        ]
      quotedStrLit s = C.stringLit ("\\\"" <> s <> "\\\"")
  
  cout s = C.stmt (C.ident "std::cout" C.<< s)

  mdl' = Core.inlineLets mdl

genExpr' :: Env -> Core.Expr -> C.Doc
genExpr' vf e0 =
  case e0 of
    Core.Op1 op e        -> unop (op1 op) e
    Core.Op2 op e1 e2    -> binop (op2 op) e1 e2
    Core.If test thn els -> C.cond (subExpr test) (subExpr thn) (subExpr els)
    Core.Literal l       -> lit l
    Core.Var n           -> vf n
    Core.Fail _          -> undefined

  where
  op1 op = case op of
             Core.Not -> "!"
             Core.Neg -> "-"
             Core.Exp -> "exp"
             Core.Log -> "log"

  op2 op = case op of
             Core.Add -> "+"
             Core.Sub -> "-"
             Core.Mul -> "*"
             Core.Div -> "/"
             Core.Lt  -> "<"
             Core.Leq -> "<="
             Core.Eq  -> "=="
             Core.And -> "&&"
             Core.Or  -> "||"

  lit l = case l of
            Core.Num d  -> C.doubleLit d
            Core.Bool d -> C.boolLit d

  binop op e1 e2 = C.callInfix op (subExpr e1) (subExpr e2)
  unop op e1     = C.callPrefix op (subExpr e1)
  subExpr        = genExprSub vf

genExpr :: Core.Expr -> C.Doc
genExpr = genExpr' stateVarName


-- todo: real precedence
genExprSub :: (Core.Ident -> C.Doc) -> Core.Expr -> C.Doc
genExprSub f e =
  case e of
    Core.Op2 {}     -> paren
    Core.Op1 {}     -> noparen
    Core.If {}      -> paren
    Core.Literal {} -> noparen
    Core.Var {}     -> noparen
    Core.Fail _     -> undefined
  where
  paren   = C.parens (genExpr' f e)
  noparen = genExpr' f e


genDriver :: 
  Core.Model -> 
  Double {- ^ start time -} ->
  Double {- ^ end time -} -> 
  Double {- ^ time step -} -> 
  C.Doc
genDriver model start stop step = C.main
  [ C.declare (modelClassName model) "model"
  , C.declareInit C.double "start" (C.doubleLit start)
  , C.declareInit C.double "stop" (C.doubleLit stop)
  , C.declareInit C.double "step" (C.doubleLit step)
  , C.declare (C.ident "std::vector" <> C.typeArgList [C.double]) "times"
  , C.for 
    (C.declareInit' C.double "time" "start") 
    ("time" C.< "stop") 
    "time += step" 
    [ C.stmt $ C.call (C.member "times" "push_back") ["time"] ]
  , C.declare C.int next_event
  , C.declareInit C.double next_time (C.doubleLit 0)
  , cout "\"[\""
  , C.for 
    (C.declareInit' C.int "i" (C.doubleLit 0)) 
    ("i" C.< C.call (C.member "times" "size") []) 
    "i++"
    [ C.while (next_time C.< C.subscript "times" "i") 
      [ C.stmt $ C.call (C.member "model" "next_event") [ next_event, next_time ]
      , C.stmt $ C.call (C.member "model" "run_event") [ next_event, next_time ]
      ]
    , C.stmt $ C.call (C.member "model" "print") []
    , C.ifThen ("i" C.< (C.call (C.member "times" "size") [] C.- C.doubleLit 1)) [cout "\",\""]
    ]
  , cout "\"]\""
  ]
  where
    cout s = C.stmt ("std::cout" C.<< s)
    next_event = C.ident "next_event"
    next_time = C.ident "next_time"