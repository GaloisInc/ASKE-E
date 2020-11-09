{-# Language LambdaCase #-}
module Language.ASKEE.SimulatorGen where

import qualified Language.ASKEE.Core as Core
import           Data.Text(Text,unpack)
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint((<+>))
import           Data.Foldable(foldl')

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


-- TODO: advance time
genExecuteStep :: [Core.Event] -> PP.Doc
genExecuteStep evts =
  PP.vcat [ PP.vcat $ rateDecl <$> evts
          , declareInitStmt dblTypeName totalRateName (totalRateExpr evts)
          , PP.text "auto rate_dist = std::uniform_real_distribution<double> { 0.0, "
                 <> totalRateName
                 <> PP.text "};"
          , declareInitStmt dblTypeName randomValueName (callFunc (PP.text "rate_dist") [rngName])
          , PP.vcat $ runEffectCondStmt <$> (evts `zip` [1..])
          , returnStmt (PP.int (negate 1))
          ]
  where
    subtractRateStmt evt =
      assignStmt randomValueName (randomValueName <+> PP.text "-" <+> effRateName evt)

    runEffectCondStmt (evt, i) =
      PP.vcat
        [ PP.hsep [randomValueName, PP.text "-=", effRateName evt <> PP.semi]
        , PP.text "if" <> PP.parens (randomValueName <+> PP.text "<= 0.0") <> PP.text "{"
        , PP.nest 4 $
            PP.vcat [ callProc (eventEffectFuncName evt) <> PP.semi
                    , returnStmt (PP.int i)
                    ]
        , PP.text "}"
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

runNSteps :: PP.Doc
runNSteps = undefined

genModel :: Core.Model -> PP.Doc
genModel mdl = 
  PP.vcat [ genIncludes
          , modelClass
          ]
  where
    decls = PP.vcat [ PP.vcat (mkEventWhenDecl <$> Core.modelEvents mdl)
                    , PP.vcat (mkEventRateDecl <$> Core.modelEvents mdl)
                    , PP.vcat (mkEventEffectDecl <$> Core.modelEvents mdl)
                    , stepFunction (Core.modelEvents mdl)
                    , setSeedFunc1
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
      let stmts = PP.vcat (mkEventEffectStmt <$> Core.eventEffect evt)
      in functionTemplate (eventEffectFuncName evt) [] voidTypeName stmts;

    mkEventWhenDecl evt =
      let result = PP.text "return" <+> (genExpr (Core.eventWhen evt) <> PP.semi)
      in functionTemplate (eventWhenName evt) [] boolTypeName result

    mkEventRateDecl evt =
      let result = PP.text "return" <+> (genExpr (Core.eventRate evt) <> PP.semi)
      in functionTemplate (eventRateName evt) [] dblTypeName result

    stepFunction evts =
      functionTemplate stepFunctionName [] dblTypeName (genExecuteStep evts)

    -- TODO: make sure this is const
    ppState (varName, val) = declareInitStmt dblTypeName (stateVarName varName) (PP.double val)
              
    stateVars =
      PP.vcat $ ppState <$> Core.modelInitState mdl
    
    modelClassDecl = PP.hsep [PP.text "struct", ppText (Core.modelName mdl) ]
    modelClass =
      PP.vcat [ modelClassDecl <+> PP.text "{" 
              , PP.nest 4 decls 
              , PP.text "};"
              ]


genExpr :: Core.Expr -> PP.Doc
genExpr e0 =
  case e0 of
    Core.ExprAdd    e1 e2 -> binop "+" e1 e2
    Core.ExprMul    e1 e2 -> binop "*" e1 e2
    Core.ExprSub    e1 e2 -> binop "-" e1 e2
    Core.ExprDiv    e1 e2 -> binop "/" e1 e2
    Core.ExprLT     e1 e2 -> binop "<" e1 e2
    Core.ExprEQ     e1 e2 -> binop "==" e1 e2
    Core.ExprGT     e1 e2 -> binop ">" e1 e2
    Core.ExprAnd    e1 e2 -> binop "&&" e1 e2
    Core.ExprOr     e1 e2 -> binop "||" e1 e2
    Core.ExprNot    e1 -> unop "!" e1
    Core.ExprNeg    e1 -> unop "-" e1
    Core.ExprIf     test thn els -> 
      PP.hsep [ genExprSub test
              , PP.text "?"
              , genExprSub thn
              , PP.text ":"
              , genExprSub els
              ]
    Core.ExprNumLit d -> PP.text $ show d
    Core.ExprVar    n -> stateVarName n

  where
    binop op e1 e2 = PP.hsep [genExprSub e1, PP.text op, genExprSub e2]
    unop op e1 = PP.text op <> genExprSub e1

-- todo: real precedence
genExprSub :: Core.Expr -> PP.Doc
genExprSub e =
  case e of
    Core.ExprAdd    _ _ -> paren
    Core.ExprMul    _ _ -> paren
    Core.ExprSub    _ _ -> paren
    Core.ExprDiv    _ _ -> paren
    Core.ExprLT     _ _ -> paren
    Core.ExprEQ     _ _ -> paren
    Core.ExprGT     _ _ -> paren
    Core.ExprAnd    _ _ -> paren
    Core.ExprOr     _ _ -> paren
    Core.ExprNot    _   -> noparen
    Core.ExprNeg    _  -> noparen
    Core.ExprIf     _ _ _ -> paren
    Core.ExprNumLit _ -> noparen
    Core.ExprVar    _ -> noparen
  where
    paren = PP.parens $ genExpr e
    noparen = genExpr e

test :: String
test =
  show $ functionTemplate (PP.text "ASKEE_run_simulate") [argDecl "int" "steps", argDecl "int*" "events"] (PP.text "void") (PP.text "hiiiii")
  where
    argDecl n v = PP.text n <+> PP.text v