{-# Language LambdaCase, OverloadedStrings #-}
module Language.ASKEE.SimulatorGen where

import qualified Language.ASKEE.Core as Core
import           Data.Text(Text,unpack)
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint((<+>), ($$))
import           Data.Foldable(foldl')
import qualified Data.Map as Map

import qualified Language.ASKEE.C as C

-- TODO: fresh naming?

-- foreign import ccall "ASKEE_run_simulate" frunSimulation :: 
--   Int -> Ptr Int -> Int -> IO ()


-- simulate :: Syntax.Model -> Query -> IO QueryResult
-- simulate :: Syntax.Model -> IO (Array  (Int, Int) Double)

-- simulate :: C.CTranslUnit -> IO [Text]


-- FFI
-- produce a list/array of events
-- void simulate(int seed, event* events, int steps) { }  -- could be okay
-- event* simulate(int seed)
--
-- void simulate(int seed, event* events, int steps, initial_conditions ...) { }

functionTemplate :: PP.Doc -> [PP.Doc] -> PP.Doc -> PP.Doc -> PP.Doc
functionTemplate functionName args returnType body =
    PP.vcat [ prototype <+> PP.lbrace
            , PP.nest 4 body
            , PP.rbrace
            ]
  where
    argList = PP.parens (PP.hsep $ PP.punctuate (PP.text ",") args)
    prototype = PP.hsep [returnType, functionName, argList]

functionTemplate' :: PP.Doc -> [PP.Doc] -> PP.Doc -> [PP.Doc] -> PP.Doc
functionTemplate' functionName args returnType stmts =
  functionTemplate functionName args returnType (PP.vcat stmts)



genIncludes :: PP.Doc
genIncludes = PP.vcat (mkInc <$> ist)
  where
    mkInc i = PP.hcat [PP.text "#include<", PP.text i, PP.text ">"]
    ist = ["random", "stdint.h"]

ppText :: Text -> PP.Doc
ppText = PP.text . unpack

eventEffectFuncName :: Core.Event -> PP.Doc
eventEffectFuncName evt = PP.text "event_effect_" <> ppText (Core.eventName evt)

eventRateName :: Core.Event -> PP.Doc
eventRateName evt = PP.text "event_rate_" <> ppText (Core.eventName evt)

eventWhenName :: Core.Event -> PP.Doc
eventWhenName evt = PP.text "event_when_"  <> ppText (Core.eventName evt)

stateVarName :: Text -> PP.Doc
stateVarName nm = PP.text "state_" <> ppText nm

stepFunctionName :: PP.Doc
stepFunctionName = PP.text "step"


nextEventFunctionName :: PP.Doc
nextEventFunctionName = "next_event"

runEventFunctionName :: PP.Doc
runEventFunctionName = "run_event"

boolTypeName :: PP.Doc
boolTypeName = PP.text "bool"
dblTypeName :: PP.Doc
dblTypeName = PP.text "double"
voidTypeName :: PP.Doc
voidTypeName = PP.text "void"
rngName :: PP.Doc
rngName = PP.text "prng"
timeName :: PP.Doc
timeName = PP.text "time"
setSeed1Name :: PP.Doc
setSeed1Name = PP.text "set_seed"
randEngineType :: PP.Doc
randEngineType = PP.text "std::mt19937_64"

callProc :: PP.Doc -> PP.Doc
callProc p = p <> PP.parens PP.empty

callFunc :: PP.Doc -> [PP.Doc] -> PP.Doc
callFunc f args = f <> PP.parens (PP.hsep $ PP.punctuate PP.comma args)

member obj x = obj <> "." <> x

assignStmt :: PP.Doc -> PP.Doc -> PP.Doc
assignStmt n v = 
  PP.hsep [n, PP.text "=", v <> PP.semi]

declareStmt :: PP.Doc -> PP.Doc -> PP.Doc
declareStmt t v = PP.hsep [t, v <> PP.semi]

declareInitStmt :: PP.Doc -> PP.Doc -> PP.Doc -> PP.Doc
declareInitStmt ty n v =
  PP.hsep [ty, n, PP.text "=", v <> PP.text ";"]

returnStmt :: PP.Doc -> PP.Doc
returnStmt val =
  (PP.text "return" <+> val) <> PP.text ";"

modelClassName :: Core.Model -> PP.Doc
modelClassName mdl = ppText (Core.modelName mdl)

eventNum :: [Core.Event]-> Core.Ident -> Int
eventNum events name =
  case lookup name indexedEventList of
    Nothing -> error ("[BUG] did not find event named " ++ unpack name ++ " in model")
    Just i -> i
  where
    indexedEventList = (Core.eventName <$> events) `zip` [1..]

genExecStep :: Core.Model-> PP.Doc
genExecStep mdl =
  C.function C.void runEventFunctionName [ C.int    <+> "nextEvent"
                                         , C.double <+> "nextTime" ]
  [ assignStmt timeName "nextTime"
  , PP.vcat (copyVar <$> Core.modelStateVars mdl)
  , C.switch "nextEvent" (mkCase <$> evts)
  ]

  where
  evts = Core.modelEvents mdl
  stateVarCurName sv = "cur_" <> stateVarName sv
  copyVar n = declareInitStmt "auto" (stateVarCurName n) (stateVarName n)
  mkEffect (n, expr) = assignStmt (stateVarName n) (genExpr' stateVarCurName expr)
  mkCase evt =
    C.stmts [ C.case' (PP.int (eventNum evts (Core.eventName evt)))
            , C.nested $ fmap mkEffect (Map.toList (Core.eventEffect evt))
                      ++ [ C.break ]
            ]

genNextStep :: [Core.Event] -> PP.Doc
genNextStep evts =
  C.function C.void nextEventFunctionName [ C.int <+> "&nextEvent"
                                          , C.double <+> "&nextTime" ]
    [ PP.vcat $ rateDecl <$> evts
    , declareInitStmt dblTypeName totalRateName (totalRateExpr evts)
    , PP.text "auto rate_dist = std::uniform_real_distribution<double> { 0.0, "
                <> totalRateName
                <> PP.text "};"

    , PP.text "auto dt_dist = std::exponential_distribution<double> {"
              <+> totalRateName
              <+> PP.text "};"
    , declareInitStmt dblTypeName randomValueName (callFunc (PP.text "rate_dist") [rngName])
    , assignStmt "nextTime" (PP.hsep [timeName, PP.text "+", callFunc (PP.text "dt_dist") [rngName] ])
    , PP.vcat $ runEffectCondStmt <$> evts
    ]

  where
    subtractRateStmt evt =
      assignStmt randomValueName (randomValueName <+> PP.text "-" <+> effRateName evt)

    runEffectCondStmt evt =
      PP.vcat
        [ PP.hsep [randomValueName, PP.text "-=", effRateName evt <> PP.semi]
        , C.ifThen (randomValueName <+> PP.text "<= 0.0")
            [ assignStmt "nextEvent" (PP.int (eventNum evts (Core.eventName evt)))
            , C.return
            ]
        ]

    randomValueName = PP.text "random"
    totalRateName = PP.text "total_rate"
    totalRateExpr evts = 
      PP.hsep $ 
        PP.punctuate (PP.text " +") (effRateName <$> evts)

    effRateName evt = PP.text "_eff_rate_" <> ppText (Core.eventName evt)
    effRateExpr evt = 
      PP.hsep [ callProc (eventWhenName evt)
              , PP.text "?"
              , callProc (eventRateName evt)
              , PP.text ":"
              , PP.double 0.0
              ]

    rateDecl evt =
      declareInitStmt dblTypeName (effRateName evt) (effRateExpr evt)

genModel :: Core.Model -> PP.Doc
genModel mdl = 
  PP.vcat [ genIncludes
          , modelClass
          ]
  where
    decls = PP.vcat [ PP.vcat (mkEventWhenDecl <$> Core.modelEvents mdl)
                    , PP.vcat (mkEventRateDecl <$> Core.modelEvents mdl)
                    , PP.vcat (mkEventEffectDecl <$> Core.modelEvents mdl)
                    , genNextStep (Core.modelEvents mdl)
                    , genExecStep mdl
                     --, setSeedFunc1
                    , setSeedFunc2
                    , stateVars
                    , declareInitStmt dblTypeName timeName (PP.double 0.0)
                    , declareStmt randEngineType rngName
                    ]

    setSeedFunc1 =
      functionTemplate setSeed1Name 
                       [PP.text "std::seed_seq const& seed"]
                       voidTypeName
                       (PP.hcat [ rngName, PP.text ".seed", PP.parens (PP.text "seed"), PP.semi])

    setSeedFunc2 =
      functionTemplate setSeed1Name
                       [PP.text "uint32_t seed"]
                       voidTypeName
                       (PP.hcat [rngName, PP.text ".seed", PP.parens (PP.text "seed"), PP.semi])

    mkEventEffectStmt (nm, e) = 
      PP.hsep [stateVarName nm, PP.text "=", genExpr e] <> PP.semi  
    mkEventEffectDecl evt =
      let stmts = PP.vcat (mkEventEffectStmt <$> Map.toList (Core.eventEffect evt))
      in functionTemplate (eventEffectFuncName evt) [] voidTypeName stmts;

    mkEventWhenDecl evt =
      let result = PP.text "return" <+> (genExpr (Core.eventWhen evt) <> PP.semi)
      in functionTemplate (eventWhenName evt) [] boolTypeName result

    mkEventRateDecl evt =
      let result = PP.text "return" <+> (genExpr (Core.eventRate evt) <> PP.semi)
      in functionTemplate (eventRateName evt) [] dblTypeName result

    stepFunction evts = genNextStep evts

    -- TODO: make sure this is const
    ppState (varName, val) = declareInitStmt dblTypeName (stateVarName varName) (PP.double val)
              
    stateVars =
      PP.vcat $ ppState <$> Map.toList (Core.modelInitState mdl)
    
    modelClassDecl = PP.hsep [PP.text "struct", modelClassName mdl ]
    modelClass =
      PP.vcat [ modelClassDecl <+> PP.text "{" 
              , PP.nest 4 decls 
              , PP.text "};"
              ]


genExpr' :: (Core.Ident -> PP.Doc) -> Core.Expr -> PP.Doc
genExpr' vf e0 =
  case e0 of
    Core.Op1 op e     -> unop (op1 op) e
    Core.Op2 op e1 e2 -> binop (op2 op) e1 e2
    Core.If test thn els ->
      PP.hsep [ subExpr test
              , PP.text "?"
              , subExpr thn
              , PP.text ":"
              , subExpr els
              ]
    Core.Literal l -> lit l
    Core.Var n -> vf n

  where
    op1 op = case op of
               Core.Not -> "!"
               Core.Neg -> "-"

    op2 op = case op of
               Core.Add -> "+"
               Core.Sub -> "-"
               Core.Mul -> "*"
               Core.Div -> "/"
               Core.Lt  -> "<"
               Core.Eq  -> "=="
               Core.And -> "&&"
               Core.Or  -> "||"

    lit l = case l of
              Core.Num d  -> PP.double d
              Core.Bool d -> if d then "true" else "false"


    binop op e1 e2 = PP.hsep [subExpr e1, PP.text op, subExpr e2]
    unop op e1 = PP.text op <> subExpr e1
    subExpr = genExprSub vf

genExpr :: Core.Expr -> PP.Doc
genExpr = genExpr' stateVarName


-- todo: real precedence
genExprSub :: (Core.Ident -> PP.Doc) -> Core.Expr -> PP.Doc
genExprSub f e =
  case e of
    Core.Op2 {}       -> paren
    Core.Op1 {}       -> noparen
    Core.If {}      -> paren
    Core.Literal {}     -> noparen
    Core.Var {}     -> noparen
  where
    paren = PP.parens $ genExpr' f e
    noparen = genExpr' f e

test :: String
test =
  show $ functionTemplate (PP.text "ASKEE_run_simulate") [argDecl "int" "steps", argDecl "int*" "events"] (PP.text "void") (PP.text "hiiiii")
  where
    argDecl n v = PP.text n <+> PP.text v
