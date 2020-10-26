-- TODO: support enabling predicates
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

module Language.ASKEE.DiffEq.DiffEq where

import Data.Text(Text)
import Control.Monad.Fail(MonadFail)
import Data.List(partition)
import Data.Maybe(catMaybes)
import qualified Text.PrettyPrint as Pretty
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Text as Text
import qualified Numeric.LinearAlgebra.Data as LinAlg
import qualified Numeric.GSL.ODE as ODE

import qualified Language.ASKEE.Syntax as Syntax

type Identifier = Text
type EqGen a = Either String a
instance MonadFail (Either String) where
  fail = Left

data DiffEq =
    StateEq {deVarName :: Text, deExpr :: EqExp } -- dx/dt = exp
  | VarEq   {deVarName :: Text, deExpr :: EqExp } -- x = exp 
  deriving(Show, Eq, Ord)

data EqExp =
    Var Text          
  | Lit Double
  | Add EqExp EqExp
  | Sub EqExp EqExp 
  | Mul EqExp EqExp
  | Div EqExp EqExp
  | Neg EqExp
  deriving(Show, Eq, Ord)

eqVarName :: DiffEq -> Text
eqVarName eq =
  case eq of
    StateEq n _ -> n
    VarEq n _ -> n

-- algebraic constructors -----------------------------------------------------

negExp :: EqExp -> EqExp
negExp (Neg e0) = e0
negExp (Lit e1) = Lit (negate e1)
negExp e0 = Neg e0

addExp :: EqExp -> EqExp -> EqExp
addExp (Lit 0.0) e = e
addExp e (Lit 0.0) = e
addExp (Lit d1) (Lit d2) = Lit (d1 + d2)
addExp e1 e2 = Add e1 e2

subExp :: EqExp -> EqExp -> EqExp
subExp e (Lit 0.0) = e
subExp (Lit 0.0) e = negExp e
subExp (Lit d1) (Lit d2) = Lit (d1 - d2)
subExp e1 e2 = Sub e1 e2

mulExp :: EqExp -> EqExp -> EqExp
mulExp (Lit 0.0) _ = Lit 0.0
mulExp _ (Lit 0.0) = Lit 0.0
mulExp (Lit 1.0) e = e
mulExp e (Lit 1.0) = e
mulExp (Lit d1) (Lit d2) = Lit (d1 * d2)
mulExp e1 e2 = Mul e1 e2

divExp :: EqExp -> EqExp -> EqExp
divExp (Lit d1) (Lit d2) = Lit (d1/d2)
divExp e (Lit 1.0) = e
divExp e1 e2 = Div e1 e2

termList :: EqExp -> [EqExp]
termList e =
  case e of
    Add e1 e2 -> termList e1 ++ termList e2
    Sub e1 e2 -> termList e1 ++ (negExp <$> termList e2)
    _ -> [e]

sumTerms :: [EqExp] -> EqExp
sumTerms = foldr addExp (Lit 0.0)


-- 'interpreter' --------------------------------------------------------------

asEquationSystem :: Syntax.Model -> EqGen ([DiffEq], Map Text EqExp)
asEquationSystem mdl =
  do  declEqs <- letEq `traverse` letVars
      stateEqs <- stateEq `traverse` stateVars
      icMap <- mkInitialCondMap
      pure (declEqs ++ stateEqs, icMap)

  where
    mkInitialCondMap :: EqGen (Map Text EqExp)
    mkInitialCondMap = Map.unions <$> (icMapElt `traverse` Syntax.modelDecls mdl)

    icMapElt :: Syntax.Decl -> EqGen (Map Text EqExp)
    icMapElt decl =
      case decl of
        Syntax.State n v -> Map.singleton n <$> asEqExp (Map.fromList letVars) v 
        _ -> pure Map.empty

    asMbStateVar :: Syntax.Decl -> Maybe (Text, Syntax.Exp)
    asMbStateVar decl =
      case decl of
        Syntax.State n v -> Just (n, v)
        Syntax.Let _ _ -> Nothing

    asMbLetVar :: Syntax.Decl -> Maybe (Text, Syntax.Exp)
    asMbLetVar decl =
      case decl of
        Syntax.Let n v -> Just (n, v)
        Syntax.State _ _ -> Nothing

    stateVars :: [(Text, Syntax.Exp)]
    stateVars = 
      catMaybes (asMbStateVar <$> Syntax.modelDecls mdl)

    letVars :: [(Text, Syntax.Exp)]
    letVars =
      catMaybes (asMbLetVar <$> Syntax.modelDecls mdl)

    letEq :: (Text, Syntax.Exp) -> EqGen DiffEq
    letEq (name, exp) =
      VarEq name <$> asEqExp (Map.fromList letVars) exp

    stateEq :: (Text, Syntax.Exp) -> EqGen DiffEq
    stateEq (sv, _) = StateEq sv . sumTerms <$> eventTerms sv

    eventTerms :: Text -> EqGen [EqExp]
    eventTerms sv =
      catMaybes <$> (eventTerm sv `traverse` (Syntax.modelEvents mdl))

    eventTerm :: Text -> Syntax.Event -> EqGen (Maybe EqExp)
    eventTerm sv event = 
      case sv `lookup` Syntax.eventEffect event of
        Nothing -> pure Nothing
        Just stExp -> 
          do  rate' <- asEqExp (Map.fromList letVars) (Syntax.eventRate event)
              stExp' <- asEqExp (Map.fromList letVars) stExp
              pure . Just $ stateEventTerm stExp' sv rate'

stateEventTerm :: EqExp -> Text -> EqExp -> EqExp
stateEventTerm e0 stateVar rateExp = rateExp `mulExp` (e0 `subExp` (Var stateVar))

asEqExp :: Map Text Syntax.Exp -> Syntax.Exp -> EqGen EqExp
asEqExp vars e = go e
  where
    go e =
      case e of
        Syntax.Add e1 e2 -> binop addExp e1 e2
        Syntax.Sub e1 e2 -> binop subExp e1 e2
        Syntax.Mul e1 e2 -> binop mulExp e1 e2
        Syntax.Div e1 e2 -> binop divExp e1 e2
        Syntax.Neg en -> negExp <$> go en
        Syntax.Var v -> varExp v 
        Syntax.Real r -> pure $ Lit r
        _ -> expNotImplemented e

    varExp v =
      case vars Map.!? v of
        Nothing -> pure $ Var v
        Just e -> go e
    binop op e1 e2 = op <$> go e1 <*> go e2
    expNotImplemented nexp = fail ("Expression form not implemented: " ++ show nexp)

-- PP -------------------------------------------------------------------------

ppDiffEq :: DiffEq -> Pretty.Doc
ppDiffEq deq = Pretty.hsep [ intro, Pretty.text "=", expr ]
  where
    expr = 
      ppEqExp $
        case deq of
          StateEq var e -> e
          VarEq var e -> e

    intro = 
      case deq of
        StateEq var e -> Pretty.text ("d" <> Text.unpack var <> "/dt")
        VarEq var e   -> Pretty.text $ Text.unpack var

ppEqExp :: EqExp -> Pretty.Doc
ppEqExp e =
  case e of
    Var t -> Pretty.text (Text.unpack t)
    Lit d -> Pretty.double d
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
       Lit _ -> litP
       Add _ _ -> addP
       Sub _ _ -> addP
       Mul _ _ -> mulP
       Div _ _ -> mulP
       Neg _ -> negP

    ppPrec p =
      if plvl p > plvl e
        then Pretty.parens (ppEqExp p)
        else ppEqExp p

    binop op p1 p2 = Pretty.hsep [ppPrec p1, Pretty.text op, ppEqExp p2]

-- evaluator ------------------------------------------------------------------

stateEqs :: [DiffEq] -> [(Text, EqExp)]
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
        Lit d -> Lit d
    
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

eval :: Map Text Int -> LinAlg.Vector Double -> EqExp -> Double
eval nameMap env e =
  case e of
    Var v -> env LinAlg.! (nameMap Map.! v) 
    Lit l -> l
    Add e1 e2 -> eval' e1 + eval' e2
    Sub e1 e2 -> eval' e1 - eval' e2
    Mul e1 e2 -> eval' e1 * eval' e2
    Div e1 e2 -> eval' e1 / eval' e2
    Neg e1 -> negate $ eval' e1
  where
    eval' = eval nameMap env

eval' :: EqExp -> Double
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
      in Map.fromList (stateVars `zip` rows)
