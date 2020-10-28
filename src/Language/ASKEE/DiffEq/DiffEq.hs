-- TODO: support enabling predicates
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

module Language.ASKEE.DiffEq.DiffEq where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Numeric.LinearAlgebra.Data as LinAlg
import qualified Numeric.GSL.ODE as ODE

import           Language.ASKEE.Expr (ArithExpr(..))
import qualified Language.ASKEE.Syntax as Syntax

import qualified Text.PrettyPrint as Pretty
import           Text.Printf (printf)

type Identifier = Text
type EqGen a = Either String a

data DiffEq =
    StateEq {deVarName :: Text, deExpr :: ArithExpr } -- dx/dt = exp
  | VarEq   {deVarName :: Text, deExpr :: ArithExpr } -- x = exp 
  deriving(Show, Eq, Ord)

eqVarName :: DiffEq -> Text
eqVarName eq =
  case eq of
    StateEq n _ -> n
    VarEq n _ -> n

-- algebraic constructors -----------------------------------------------------

negExp :: ArithExpr -> ArithExpr
negExp (Neg e0) = e0
negExp (ALit e1) = ALit (negate e1)
negExp e0 = Neg e0

addExp :: ArithExpr -> ArithExpr -> ArithExpr
addExp (ALit 0.0) e = e
addExp e (ALit 0.0) = e
addExp (ALit d1) (ALit d2) = ALit (d1 + d2)
addExp e1 e2 = Add e1 e2

subExp :: ArithExpr -> ArithExpr -> ArithExpr
subExp e (ALit 0.0) = e
subExp (ALit 0.0) e = negExp e
subExp (ALit d1) (ALit d2) = ALit (d1 - d2)
subExp e1 e2 = Sub e1 e2

mulExp :: ArithExpr -> ArithExpr -> ArithExpr
mulExp (ALit 0.0) _ = ALit 0.0
mulExp _ (ALit 0.0) = ALit 0.0
mulExp (ALit 1.0) e = e
mulExp e (ALit 1.0) = e
mulExp (ALit d1) (ALit d2) = ALit (d1 * d2)
mulExp e1 e2 = Mul e1 e2

divExp :: ArithExpr -> ArithExpr -> ArithExpr
divExp (ALit d1) (ALit d2) = ALit (d1/d2)
divExp e (ALit 1.0) = e
divExp e1 e2 = Div e1 e2

termList :: ArithExpr -> [ArithExpr]
termList e =
  case e of
    Add e1 e2 -> termList e1 ++ termList e2
    Sub e1 e2 -> termList e1 ++ (negExp <$> termList e2)
    _ -> [e]

sumTerms :: [ArithExpr] -> ArithExpr
sumTerms = foldr addExp (ALit 0.0)


-- 'interpreter' --------------------------------------------------------------

asEquationSystem :: Syntax.Model -> EqGen ([DiffEq], Map Text ArithExpr)
asEquationSystem mdl =
  do  declEqs <- letEq `traverse` letVars
      stateEqs <- stateEq `traverse` stateVars
      icMap <- mkInitialCondMap
      pure (timeEq : declEqs ++ stateEqs, icMap)

  where
    mkInitialCondMap :: EqGen (Map Text ArithExpr)
    mkInitialCondMap = 
      do decls <- icMapElt `traverse` Syntax.modelDecls mdl
         let time = Map.singleton "time" (ALit 0.0)
         pure $ Map.unions (time : decls)

    icMapElt :: Syntax.Decl -> EqGen (Map Text ArithExpr)
    icMapElt decl =
      case decl of
        Syntax.State n v -> Map.singleton n <$> inlineVars (Map.fromList letVars) v 
        _ -> pure Map.empty

    -- Don't support non-arithmetic expressions for DE generation
    asMbStateVar :: Syntax.Decl -> Maybe (Text, ArithExpr)
    asMbStateVar decl =
      case decl of
        Syntax.State n (Syntax.ArithExpr v) -> Just (n, v)
        _ -> Nothing

    -- Don't support non-arithmetic expressions for DE generation
    asMbLetVar :: Syntax.Decl -> Maybe (Text, ArithExpr)
    asMbLetVar decl =
      case decl of
        Syntax.Let n (Syntax.ArithExpr v) -> Just (n, v)
        _ -> Nothing

    stateVars :: [(Text, ArithExpr)]
    stateVars = 
      catMaybes (asMbStateVar <$> Syntax.modelDecls mdl)

    letVars :: [(Text, ArithExpr)]
    letVars =
      catMaybes (asMbLetVar <$> Syntax.modelDecls mdl)

    letEq :: (Text, ArithExpr) -> EqGen DiffEq
    letEq (name, exp) =
      VarEq name <$> inlineVars (Map.fromList letVars) (Syntax.ArithExpr exp)

    stateEq :: (Text, ArithExpr) -> EqGen DiffEq
    stateEq (sv, _) = StateEq sv . sumTerms <$> eventTerms sv

    eventTerms :: Text -> EqGen [ArithExpr]
    eventTerms sv =
      catMaybes <$> (eventTerm sv `traverse` (Syntax.modelEvents mdl))

    eventTerm :: Text -> Syntax.Event -> EqGen (Maybe ArithExpr)
    eventTerm sv event = 
      case sv `lookup` Syntax.eventEffect event of
        Nothing -> pure Nothing
        Just stExp -> 
          do  rate' <- inlineVars (Map.fromList letVars) (Syntax.eventRate event)
              stExp' <- inlineVars (Map.fromList letVars) (Syntax.ArithExpr stExp)
              pure . Just $ stateEventTerm stExp' sv rate'
    
    timeEq :: DiffEq
    timeEq = StateEq "time" (ALit 1.0) -- put "time" in scope

stateEventTerm :: ArithExpr -> Text -> ArithExpr -> ArithExpr
stateEventTerm e0 stateVar rateExp = rateExp `mulExp` (e0 `subExp` (Var stateVar))

inlineVars :: Map Text ArithExpr -> Syntax.ModelExpr -> EqGen ArithExpr
inlineVars vars e = goM e
  where
    goM :: Syntax.ModelExpr -> EqGen ArithExpr
    goM e =
      case e of
        Syntax.ArithExpr e -> goA e
        _ -> expNotImplemented e
    
    goA :: ArithExpr -> EqGen ArithExpr
    goA e =
      case e of 
        Add e1 e2 -> binop addExp e1 e2
        Sub e1 e2 -> binop subExp e1 e2
        Mul e1 e2 -> binop mulExp e1 e2
        Div e1 e2 -> binop divExp e1 e2
        Neg en -> negExp <$> goA en
        Var v -> varExp v 
        ALit r -> pure $ ALit r
        
    varExp :: Text -> EqGen ArithExpr
    varExp v =
      case vars Map.!? v of
        Nothing -> pure $ Var v
        Just e -> goA e

    binop op e1 e2 = op <$> goA e1 <*> goA e2
    expNotImplemented nexp = fail ("DiffEq: expression form not implemented: " ++ show nexp)

-- PP -------------------------------------------------------------------------

ppDiffEq :: DiffEq -> Pretty.Doc
ppDiffEq deq = Pretty.hsep [ intro, Pretty.text "=", expr ]
  where
    expr = 
      ppArithExpr $
        case deq of
          StateEq var e -> e
          VarEq var e -> e

    intro = 
      case deq of
        StateEq var e -> Pretty.text ("d" <> Text.unpack var <> "/dt")
        VarEq var e   -> Pretty.text $ Text.unpack var

ppArithExpr :: ArithExpr -> Pretty.Doc
ppArithExpr e =
  case e of
    Var t -> Pretty.text (Text.unpack t)
    ALit d -> Pretty.double d
    Add p1 p2 -> binop "+" p1 p2 
    Sub p1 p2 -> binop "-" p1 p2
    Mul p1 p2 -> binop "*" p1 p2
    Div p1 p2 -> binop "/" p1 p2
    Neg p1 -> undefined
      
  where
    (litP, negP, mulP, addP) = (0,1,2,3)
    plvl pe =
     case pe of
       Var _ -> litP
       ALit _ -> litP
       Add _ _ -> addP
       Sub _ _ -> addP
       Mul _ _ -> mulP
       Div _ _ -> mulP
       Neg _ -> negP

    ppPrec p =
      if plvl p > plvl e
        then Pretty.parens (ppArithExpr p)
        else ppArithExpr p

    binop op p1 p2 = Pretty.hsep [ppPrec p1, Pretty.text op, ppPrec p2]

latexDiffEq :: DiffEq -> Pretty.Doc
latexDiffEq deq = Pretty.hsep [ intro, Pretty.text "=", expr ]
  where
    expr = 
      latexArithExpr $
        case deq of
          StateEq var e -> e
          VarEq var e -> e

    intro = 
      case deq of
        StateEq var e -> Pretty.text ("\\frac{d"<>Text.unpack var<>"}{dt}")
        VarEq var e   -> Pretty.text $ Text.unpack var

latexArithExpr :: ArithExpr -> Pretty.Doc
latexArithExpr e =
  case e of
    Var t -> Pretty.text (Text.unpack t)
    ALit d -> Pretty.text (printf "%f" d) -- Pretty.double 0.04 => "4.0e-2", no good for latex
    Add p1 p2 -> binop "+" p1 p2 
    Sub p1 p2 -> binop "-" p1 p2
    Mul p1 p2 -> binop "*" p1 p2 -- or just whitespace?
    Div p1 p2 -> "\\frac{"<>latexArithExpr p1<>"}{"<>latexArithExpr p2<>"}"
    Neg p1 -> parenthesize $ latexArithExpr p1
      
  where
    (litP, negP, mulP, addP) = (0,1,2,3)
    plvl pe =
     case pe of
       Var _ -> litP
       ALit _ -> litP
       Add _ _ -> addP
       Sub _ _ -> addP
       Mul _ _ -> mulP
       Div _ _ -> mulP
       Neg _ -> negP

    ppPrec p =
      if plvl p > plvl e
        then parenthesize (latexArithExpr p)
        else latexArithExpr p

    parenthesize e = "\\left("<>e<>"\\right)"
    binop op p1 p2 = Pretty.hsep [ppPrec p1, Pretty.text op, ppPrec p2]

-- evaluator ------------------------------------------------------------------

stateEqs :: [DiffEq] -> [(Text, ArithExpr)]
stateEqs eqs = eqs >>= stEqElt
  where  
    stEqElt (StateEq n e) = [(n, e)]
    stEqElt (VarEq _ _) = []

-- this should be a transform on our input format?
-- TODO: make this emit nothing but state eqs instead of DiffEq
inlineNonStateVars :: [DiffEq] -> [DiffEq]
inlineNonStateVars eqs = eqs >>= inlineEq
  where
    inlineExp e =
      case e of
        Var v ->
          case Map.lookup v inlineMap of
            Just vexp -> inlineExp vexp
            Nothing   -> Var v

        Add e1 e2 -> bin e1 e2 Add
        Sub e1 e2 -> bin e1 e2 Sub
        Div e1 e2 -> bin e1 e2 Div
        Mul e1 e2 -> bin e1 e2 Mul
        Neg e0 -> Neg $ inlineExp e0
        ALit d -> ALit d
    
    bin e1 e2 c = c (inlineExp e1) (inlineExp e2)

    inlineEq eq =
      case eq of
        StateEq n e -> [ StateEq n (inlineExp e) ]
        VarEq _ _   -> []

    inlineMap = Map.unions (mkInlineMapElt <$> eqs)
    mkInlineMapElt eq = 
      case eq of
        StateEq _ _ -> Map.empty
        VarEq n e   -> Map.singleton n e

eval :: Map Text Int -> LinAlg.Vector Double -> ArithExpr -> Double
eval nameMap env e =
  case e of
    Var v -> env LinAlg.! (nameMap Map.! v) 
    ALit l -> l
    Add e1 e2 -> eval' e1 + eval' e2
    Sub e1 e2 -> eval' e1 - eval' e2
    Mul e1 e2 -> eval' e1 * eval' e2
    Div e1 e2 -> eval' e1 / eval' e2
    Neg e1 -> negate $ eval' e1
  where
    eval' = eval nameMap env

eval' :: ArithExpr -> Double
eval' eq = eval Map.empty (LinAlg.fromList []) eq

asGSLODEfn :: [DiffEq] -> Double -> [Double] -> [Double]
asGSLODEfn eqs t vals = eval nameMap (LinAlg.fromList vals) . deExpr <$> eqs' 
  where
    eqs' = inlineNonStateVars eqs
    nameMap = Map.fromList [(n, i) | StateEq n _ <- eqs'
                                   | i <- [0 ..]         ]

simulateModelDiffEq :: Syntax.Model -> [Double] -> Either String (Map Text [Double])
simulateModelDiffEq mdl time =
  do  (eqns, ic) <- asEquationSystem mdl
      let stateVars = fst <$> stateEqs eqns
          gslode = asGSLODEfn eqns
          ic' = eval' . (ic Map.!) <$> stateVars
          resultMatrix = ODE.odeSolve gslode ic' (LinAlg.fromList time)

      pure $ resultMap stateVars resultMatrix
      
  where
    resultMap :: [Text] -> LinAlg.Matrix Double -> Map Text [Double]
    resultMap stateVars matrix = 
      let rows = LinAlg.toList <$> LinAlg.toColumns matrix
      in Map.delete "time" $ Map.fromList (stateVars `zip` rows)
