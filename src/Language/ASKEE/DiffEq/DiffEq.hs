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

import           Language.ASKEE.Expr (Expr(..))
import qualified Language.ASKEE.Syntax as Syntax

import qualified Text.PrettyPrint as Pretty
import           Text.Printf (printf)

type Identifier = Text
type EqGen a = Either String a

data DiffEq =
    StateEq {deVarName :: Text, deExpr :: Expr } -- dx/dt = exp
  | VarEq   {deVarName :: Text, deExpr :: Expr } -- x = exp 
  deriving(Show, Eq, Ord)

eqVarName :: DiffEq -> Text
eqVarName eq =
  case eq of
    StateEq n _ -> n
    VarEq n _ -> n

-- algebraic constructors -----------------------------------------------------

negExp :: Expr -> Expr
negExp (Neg e0) = e0
negExp (LitD e1) = LitD (negate e1)
negExp e0 = Neg e0

addExp :: Expr -> Expr -> Expr
addExp (LitD 0.0) e = e
addExp e (LitD 0.0) = e
addExp (LitD d1) (LitD d2) = LitD (d1 + d2)
addExp e1 e2 = Add e1 e2

subExp :: Expr -> Expr -> Expr
subExp e (LitD 0.0) = e
subExp (LitD 0.0) e = negExp e
subExp (LitD d1) (LitD d2) = LitD (d1 - d2)
subExp e1 e2 = Sub e1 e2

mulExp :: Expr -> Expr -> Expr
mulExp (LitD 0.0) _ = LitD 0.0
mulExp _ (LitD 0.0) = LitD 0.0
mulExp (LitD 1.0) e = e
mulExp e (LitD 1.0) = e
mulExp (LitD d1) (LitD d2) = LitD (d1 * d2)
mulExp e1 e2 = Mul e1 e2

divExp :: Expr -> Expr -> Expr
divExp (LitD d1) (LitD d2) = LitD (d1/d2)
divExp e (LitD 1.0) = e
divExp e1 e2 = Div e1 e2

termList :: Expr -> [Expr]
termList e =
  case e of
    Add e1 e2 -> termList e1 ++ termList e2
    Sub e1 e2 -> termList e1 ++ (negExp <$> termList e2)
    _ -> [e]

prodList :: Expr -> [Expr]
prodList e =
  case e of
    Mul e1 e2 -> prodList e1 ++ prodList e2
    _ -> [e]

prodListPos :: Expr -> (Bool, [Expr])
prodListPos e = (isNeg False prodTerms, unNeg <$> prodTerms)
  where
    prodTerms = prodList e
    isNeg t (Neg e':tl) = not t
    isNeg t e = t

    unNeg (Neg e') = e'
    unNeg e' = e'


sumTerms :: [Expr] -> Expr
sumTerms = foldr addExp (LitD 0.0)

prodTerms :: [Expr] -> Expr
prodTerms = foldr mulExp (LitD 1.0)


-- 'interpreter' --------------------------------------------------------------

asEquationSystem :: Syntax.Model -> EqGen ([DiffEq], Map Text Expr)
asEquationSystem mdl =
  do  declEqs <- letEq `traverse` letVars
      stateEqs <- stateEq `traverse` stateVars
      icMap <- mkInitialCondMap
      pure (timeEq : declEqs ++ stateEqs, icMap)

  where
    mkInitialCondMap :: EqGen (Map Text Expr)
    mkInitialCondMap = 
      do decls <- icMapElt `traverse` Syntax.modelDecls mdl
         let time = Map.singleton "time" (LitD 0.0)
         pure $ Map.unions (time : decls)

    icMapElt :: Syntax.Decl -> EqGen (Map Text Expr)
    icMapElt decl =
      case decl of
        Syntax.State n v -> Map.singleton n <$> inlineVars (Map.fromList letVars) v 
        _ -> pure Map.empty

    -- Don't support non-arithmetic expressions for DE generation
    asMbStateVar :: Syntax.Decl -> Maybe (Text, Expr)
    asMbStateVar decl =
      case decl of
        Syntax.State n (v) -> Just (n, v)
        _ -> Nothing

    -- Don't support non-arithmetic expressions for DE generation
    asMbLetVar :: Syntax.Decl -> Maybe (Text, Expr)
    asMbLetVar decl =
      case decl of
        Syntax.Let n (v) -> Just (n, v)
        _ -> Nothing

    stateVars :: [(Text, Expr)]
    stateVars = 
      catMaybes (asMbStateVar <$> Syntax.modelDecls mdl)

    letVars :: [(Text, Expr)]
    letVars =
      catMaybes (asMbLetVar <$> Syntax.modelDecls mdl)

    letEq :: (Text, Expr) -> EqGen DiffEq
    letEq (name, exp) =
      VarEq name <$> inlineVars (Map.fromList letVars) (exp)

    stateEq :: (Text, Expr) -> EqGen DiffEq
    stateEq (sv, _) = StateEq sv . sumTerms <$> eventTerms sv

    eventTerms :: Text -> EqGen [Expr]
    eventTerms sv =
      catMaybes <$> (eventTerm sv `traverse` (Syntax.modelEvents mdl))

    eventTerm :: Text -> Syntax.Event -> EqGen (Maybe Expr)
    eventTerm sv event = 
      case sv `lookup` Syntax.eventEffect event of
        Nothing -> pure Nothing
        Just stExp -> 
          do  rate' <- inlineVars (Map.fromList letVars) (Syntax.eventRate event)
              stExp' <- inlineVars (Map.fromList letVars) (stExp)
              pure . Just $ stateEventTerm stExp' sv rate'
    
    timeEq :: DiffEq
    timeEq = StateEq "time" (LitD 1.0) -- put "time" in scope

stateEventTerm :: Expr -> Text -> Expr -> Expr
stateEventTerm e0 stateVar rateExp = rateExp `mulExp` (e0 `subExp` (Var stateVar))

inlineVars :: Map Text Expr -> Expr -> EqGen Expr
inlineVars vars e = go e
  where
    -- goM :: Syntax.ModelExpr -> EqGen Expr
    -- goM e =
    --   case e of
    --     e -> go e
    
    go :: Expr -> EqGen Expr
    go e =
      case e of 
        Add e1 e2 -> binop addExp e1 e2
        Sub e1 e2 -> binop subExp e1 e2
        Mul e1 e2 -> binop mulExp e1 e2
        Div e1 e2 -> binop divExp e1 e2
        Neg en -> negExp <$> go en
        Var v -> varExp v 
        LitD r -> pure $ LitD r
        _ -> expNotImplemented e
        
    varExp :: Text -> EqGen Expr
    varExp v =
      case vars Map.!? v of
        Nothing -> pure $ Var v
        Just e -> go e

    binop op e1 e2 = op <$> go e1 <*> go e2
    expNotImplemented nexp = fail ("DiffEq: expression form not implemented: " ++ show nexp)

-- PP -------------------------------------------------------------------------

ppDiffEq :: DiffEq -> Pretty.Doc
ppDiffEq deq = Pretty.hsep [ intro, Pretty.text "=", expr ]
  where
    expr = 
      ppExpr $
        case deq of
          StateEq var e -> e
          VarEq var e -> e

    intro = 
      case deq of
        StateEq var e -> Pretty.text ("d" <> Text.unpack var <> "/dt")
        VarEq var e   -> Pretty.text $ Text.unpack var

ppExpr :: Expr -> Pretty.Doc
ppExpr e =
  case e of
    Var t -> Pretty.text (Text.unpack t)
    LitD d -> Pretty.double d
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
       LitD _ -> litP
       Add _ _ -> addP
       Sub _ _ -> addP
       Mul _ _ -> mulP
       Div _ _ -> mulP
       Neg _ -> negP

    ppPrec p =
      if plvl p > plvl e
        then Pretty.parens (ppExpr p)
        else ppExpr p

    binop op p1 p2 = Pretty.hsep [ppPrec p1, Pretty.text op, ppPrec p2]

latexDiffEq :: DiffEq -> Pretty.Doc
latexDiffEq deq = Pretty.hsep [ intro, Pretty.text "=", expr ]
  where
    expr = 
      latexExpr $
        case deq of
          StateEq var e -> e
          VarEq var e -> e

    intro = 
      case deq of
        StateEq var e -> Pretty.text ("\\frac{d"<>Text.unpack var<>"}{dt}")
        VarEq var e   -> Pretty.text $ Text.unpack var

latexExpr :: Expr -> Pretty.Doc
latexExpr e =
  case e of
    Var t -> Pretty.text (Text.unpack t)
    LitD d -> Pretty.text (printf "%f" d) -- Pretty.double 0.04 => "4.0e-2", no good for latex
    Add p1 p2 -> binop "+" p1 p2 
    Sub p1 p2 -> binop "-" p1 p2
    Mul p1 p2 -> binop "*" p1 p2 -- or just whitespace?
    Div p1 p2 -> "\\frac{"<>latexExpr p1<>"}{"<>latexExpr p2<>"}"
    Neg p1 -> parenthesize $ latexExpr p1
      
  where
    (litP, negP, mulP, addP) = (0,1,2,3)
    plvl pe =
     case pe of
       Var _ -> litP
       LitD _ -> litP
       Add _ _ -> addP
       Sub _ _ -> addP
       Mul _ _ -> mulP
       Div _ _ -> mulP
       Neg _ -> negP

    ppPrec p =
      if plvl p > plvl e
        then parenthesize (latexExpr p)
        else latexExpr p

    parenthesize e = "\\left("<>e<>"\\right)"
    binop op p1 p2 = Pretty.hsep [ppPrec p1, Pretty.text op, ppPrec p2]

-- evaluator ------------------------------------------------------------------

stateEqs :: [DiffEq] -> [(Text, Expr)]
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
        LitD d -> LitD d
    
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

eval :: Map Text Int -> LinAlg.Vector Double -> Expr -> Double
eval nameMap env e =
  case e of
    Var v -> env LinAlg.! (nameMap Map.! v) 
    LitD l -> l
    Add e1 e2 -> eval' e1 + eval' e2
    Sub e1 e2 -> eval' e1 - eval' e2
    Mul e1 e2 -> eval' e1 * eval' e2
    Div e1 e2 -> eval' e1 / eval' e2
    Neg e1 -> negate $ eval' e1
  where
    eval' = eval nameMap env

eval' :: Expr -> Double
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


-- diffeq -> model ------------------------------------------------------------


--
-- in general we can take something of the form:
--
--  dX/dt = r + nY
--  dY/dt = -nY
--


-- WIP
-- equationSystemAsModel :: [DiffEq] -> Maybe Syntax.Model
-- equationSystemAsModel ds = undefined
--   where
--     stateEq (StateEq n e) = [(n, e)]
--     stateEq _ = []

--     stateEqs = ds >>= stateEq
--     stateVars = fst <$> stateEqs

--     events =
--       asEvent <$> [0..] `zip` Map.toList effMap 

--     asEvent (n, (rate, effs)) =
--       Syntax.Event { eventName = "event_" ++ show n
--                    , eventRate = rate
--                    , eventEffect = effs
--                    }

--     effMap = Map.unionWith (++) stateEqs >>= rateElts

--     rateElts (var, eq) = 
--       let effs = mkTermEffect var <$> termList eq
--       in undefined
          

--     mkTermEffect var term =
--       let (isNeg, rateTerms, svTerms) = decomposeStateTerm term
--           effect = 
--             if isNeg
--                 then Var var `subExp` prodTerms svTerms
--                 else Var var `addExp` prodTerms svTerms
--           in (prodTerms rateTerms, [(var, effect)])


    
--     -- decompose state terms into rate/state var pairs 
--     decomposeStateTerm t =
--       let (isNeg, term)        = prodListPos t
--           (svTerms, rateTerms) = partition (`elem` stateVars)
--       in (isNeg, rateTerms, svTerms)








