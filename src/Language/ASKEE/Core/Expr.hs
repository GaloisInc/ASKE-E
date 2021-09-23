{-# Language PatternSynonyms, ApplicativeDo, RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.ASKEE.Core.Expr where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text ( Text )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Graph ( stronglyConnComp, SCC(..) )
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Functor.Identity (runIdentity)
import qualified Data.Functor.Const as Const

import Language.ASKEE.Panic (panic)
import qualified Language.ASKEE.Expr as Expr

type Ident = Text

asText :: Ident -> Text
asText = id

data Expr =
    Literal Literal
  | Op1 Op1 Expr
  | Op2 Op2 Expr Expr
  | Var Ident
  | If Expr Expr Expr
  | Fail String
    deriving (Show,Eq,Ord,Generic,NFData)

data Op1 = Not | Neg | Exp | Log
  deriving (Show,Eq,Ord,Generic,NFData)

data Op2 = Add | Mul | Sub | Div | Lt | Leq | Eq | And | Or | Pow
  deriving (Show,Eq,Ord,Generic,NFData)

data Literal =
    Num Double
  | Bool Bool
    deriving (Show,Eq,Ord,Generic,NFData)


--------------------------------------------------------------------------------
-- Conveninece

pattern (:+:) :: Expr -> Expr -> Expr
pattern (:+:) e1 e2 = Op2 Add e1 e2

pattern (:-:) :: Expr -> Expr -> Expr
pattern (:-:) e1 e2 = Op2 Sub e1 e2

pattern (:*:) :: Expr -> Expr -> Expr
pattern (:*:) e1 e2 = Op2 Mul e1 e2

pattern (:/:) :: Expr -> Expr -> Expr
pattern (:/:) e1 e2 = Op2 Div e1 e2

pattern (:<:) :: Expr -> Expr -> Expr
pattern (:<:) e1 e2 = Op2 Lt e1 e2

pattern (:<=:) :: Expr -> Expr -> Expr
pattern (:<=:) e1 e2 = Op2 Leq e1 e2

pattern (:==:) :: Expr -> Expr -> Expr
pattern (:==:) e1 e2 = Op2 Eq e1 e2

pattern (:&&:) :: Expr -> Expr -> Expr
pattern (:&&:) e1 e2 = Op2 And e1 e2

pattern (:||:) :: Expr -> Expr -> Expr
pattern (:||:) e1 e2 = Op2 Or e1 e2

pattern BoolLit :: Bool -> Expr
pattern BoolLit x = Literal (Bool x)

pattern NumLit :: Double -> Expr
pattern NumLit  x = Literal (Num  x)

-------------------------------------------------------------------------------

-- One level traversals

-- | Apply a function to the "locations" specified by the first argument.
mapAt ::
  (forall f. Applicative f => (a -> f b) -> x -> f y)
  {- ^ locatiosn where to map -} ->

  (a -> b) -> x -> y
mapAt it f = runIdentity . it (pure . f)

-- | Collect the values at the "locations" specified by the first argument.
collectFrom ::
  (Monoid m) =>
  (forall f. Applicative f => (a -> f b) -> x -> f y) ->
  (a -> m) -> x -> m
collectFrom it f t = Const.getConst (it (Const.Const . f) t)




-- | Locations of expressions in a structure
class TraverseExprs t where
  traverseExprs :: Applicative f => (Expr -> f Expr) -> t -> f t

-- | Apply a function to expressions contained in something.
mapExprs :: TraverseExprs t => (Expr -> Expr) -> t -> t
mapExprs = mapAt traverseExprs

-- | Collect stuff from expressions contained in something
collectExprs :: (TraverseExprs t, Monoid m) => (Expr -> m) -> t -> m
collectExprs = collectFrom traverseExprs



exprChildren :: Applicative f => (Expr -> f Expr) -> Expr -> f Expr
exprChildren f expr =
    case expr of
      Literal {}   -> pure expr
      Op1 op e     -> Op1 op <$> f e
      Op2 op e1 e2 -> Op2 op <$> f e1 <*> f e2
      Var {}       -> pure expr
      If e1 e2 e3  -> If <$> f e1 <*> f e2 <*> f e3
      Fail {}      -> pure expr

-- | Replace variable with expressions as specified by the map.
substExpr :: Map Ident Expr -> Expr -> Expr
substExpr su expr =
  case expr of
    Var x | Just e <- Map.lookup x su -> e
    _ -> mapAt exprChildren (substExpr su) expr


collectVars :: (TraverseExprs t) => t -> Set.Set Ident
collectVars = collectExprs collectExprVars

collectExprVars :: Expr -> Set.Set Ident
collectExprVars e =
  case e of
    Var n -> Set.singleton n
    _     -> collectFrom exprChildren collectExprVars e

simplifyExpr :: Expr -> Expr
simplifyExpr expr =
  case mapAt exprChildren (toFrom . simplifyExpr . toFrom) expr of
    If (BoolLit b) e1 e2          -> if b then e1 else e2

    -- Numerics
    Op1 Neg (Op1 Neg e)           -> e
    Op1 Neg (NumLit x)            -> NumLit (negate x)
    Op1 Neg (Op2 Div x y)         -> simplifyExpr (Op2 Div (Op1 Neg x) y)
    Op1 Neg (Op2 Mul x y)         -> simplifyExpr (Op2 Mul (Op1 Neg x) y)

    Op2 Add (NumLit 0) e          -> e
    Op2 Add e (NumLit 0)          -> e
    Op2 Add (NumLit x) (NumLit y) -> NumLit (x + y)

    Op2 Sub (NumLit 0) e          -> simplifyExpr (Op1 Neg e)
    Op2 Sub e (NumLit 0)          -> e
    Op2 Sub (NumLit x) (NumLit y) -> NumLit (x - y)

    Op2 Mul (NumLit 0) _          -> NumLit 0
    Op2 Mul _ (NumLit 0)          -> NumLit 0
    Op2 Mul (NumLit 1) e          -> e
    Op2 Mul e (NumLit 1)          -> e

    Op2 Mul e (NumLit (-1))       -> simplifyExpr (Op1 Neg e)
    Op2 Mul (NumLit (-1)) e       -> simplifyExpr (Op1 Neg e)
    Op2 Mul (NumLit x) (NumLit y) -> NumLit (x * y)
    Op2 Mul x (Op2 Div y z)       -> simplifyExpr (Op2 Div (Op2 Mul x y) z)
    Op2 Mul (Op2 Div y z) x       -> simplifyExpr (Op2 Div (Op2 Mul y x) z)

    Op2 Div e (NumLit 1)          -> e
    Op2 Div (NumLit x) (NumLit y) -> NumLit (x / y)


    -- Booleans
    Op1 Not (Op1 Not e)           -> e
    Op1 Not (BoolLit x)           -> BoolLit (not x)

    Op2 Lt  (NumLit x) (NumLit y) -> BoolLit (x < y)
    Op2 Leq (NumLit x) (NumLit y) -> BoolLit (x <= y)
    Op2 Eq  (NumLit x) (NumLit y) -> BoolLit (x == y)

    Op2 And (BoolLit False) _     -> BoolLit False
    Op2 And _ (BoolLit False)     -> BoolLit False
    Op2 And (BoolLit True) e      -> e
    Op2 And e (BoolLit True)      -> e

    Op2 Or (BoolLit False) e      -> e
    Op2 Or e (BoolLit False)      -> e
    Op2 Or (BoolLit True) _       -> BoolLit True
    Op2 Or _ (BoolLit True)       -> BoolLit True

    e                             -> e


-- | Return the list of terms of a linear expression, negating them if they are subtracted
exprToTermList :: Expr -> [Expr]
exprToTermList e =
  case e of
    Op2 Add e1 e2 -> exprToTermList e1 ++ exprToTermList e2
    Op2 Sub e1 e2 -> exprToTermList e1 ++ (simplifyExpr . Op1 Neg <$> exprToTermList e2)
    _ -> [e]

-- | turn a term list back into a linear expression
termListToExpr :: [Expr] -> Expr
termListToExpr = foldr mkTerm (NumLit 0.0)
  where
    mkTerm e1 (Op1 Neg e2) = Op2 Sub e1 e2
    mkTerm e1 e2 = Op2 Add e1 e2


--------------------------------------------------------------------------------

toFrom :: Expr -> Expr
toFrom = fromSum . toSum

data Sum = Sum Double (Map Expr Double)   -- a * x + b * y + c

add :: Sum -> Sum -> Sum
add (Sum a xs) (Sum b ys) = Sum (a+b) (Map.unionWith (+) xs ys)

mulK :: Double -> Sum -> Sum
mulK x (Sum a xs) = Sum (x * a) ((* x) <$> xs)

atom :: Expr -> Sum
atom x = Sum 0 (Map.singleton x 1)

lit :: Double -> Sum
lit x = Sum x Map.empty

toSum :: Expr -> Sum
toSum expr =
  case expr of
    Literal l ->
      case l of
        Num x -> lit x
        _      -> atom expr

    Op1 op e ->
      case op of
        Neg -> mulK (-1) (toSum e)
        _   -> atom expr

    Op2 op e1 e2 ->
      case op of
        Add -> add (toSum e1) (toSum e2)
        Sub -> add (toSum e1) (mulK (-1) (toSum e2))
        Mul | NumLit x <- e1 -> mulK x (toSum e2)
            | NumLit x <- e2 -> mulK x (toSum e1)
        _   -> atom expr

    _ -> atom expr

fromSum :: Sum -> Expr
fromSum (Sum k0 xs) =
  case reverse $ sortBy (comparing snd)(Map.toList (Map.filter (/= 0) xs)) of
    []       -> NumLit k0
    t0@(e,k) : rest ->
      case compare k0 0 of
        LT -> Op2 Sub (termFrom start rest) (NumLit (negate k0))
        EQ -> termFrom start rest
        GT | k < 0      -> termFrom (NumLit k0) (t0 : rest)
           | otherwise  -> Op2 Add (termFrom start rest) (NumLit k0)

        where
        termFrom = foldl' term

        start
          | k == 1      = e
          | k == (-1)   = Op1 Neg e
          | otherwise   = Op2 Mul (NumLit k) e

  where
  term t (e,k)
    | k == 1    = Op2 Add t e
    | k == (-1) = Op2 Add t (Op1 Neg e)
    | k < 0     = Op2 Sub t (Op2 Mul (NumLit (negate k)) e)
    | otherwise = Op2 Add t (Op2 Mul (NumLit k) e)


-- | Traverse the immediate children of an expression
instance TraverseExprs Expr where
  traverseExprs f expr =
    case expr of
      Literal {}   -> pure expr
      Op1 op e     -> Op1 op <$> f e
      Op2 op e1 e2 -> Op2 op <$> f e1 <*> f e2
      Var {}       -> f expr
      If e1 e2 e3  -> If <$> f e1 <*> f e2 <*> f e3
      Fail {}      -> pure expr


--------------------------------------------------------------------------------

-- | Order declarations in dependency order, and partition them into
-- two groups:  the first one only contains declarations that *DO NOT*
-- depend on the given set, while the second ones do.
-- Assumes that no variables are recursive
partOrderDecls ::
  Set Ident -> [(Ident,Expr)] -> ( [(Ident,Expr)], [(Ident,Expr)] )
partOrderDecls s0 decls =
  case foldl' part ([], [], s0) ordered of
    (ps,ss,_) -> (reverse ps, reverse ss)

  where
  ordered = stronglyConnComp
    [ ((v, e, vs), v, Set.toList vs)
    | (v, e) <- decls
    , let vs = collectExprVars e
    ]

  part (params, lets, stateful) x =
    case x of
      CyclicSCC _ -> panic "orderDecls" ["cyclic declarations"]
      AcyclicSCC (v, e, vs) ->
        -- if variable does not involve something stateful
        if Set.null (stateful `Set.intersection` vs)
          then ((v,e) : params, lets, stateful)
          else (params, (v,e) : lets, Set.insert v stateful)

-- | Order declaratoins in dependency order, assumng no recursion
orderDecls :: [(Ident,Expr)] -> [(Ident,Expr)]
orderDecls = fst . partOrderDecls Set.empty

asConst :: Expr -> Maybe Double
asConst e =
  case simplifyExpr e of
    NumLit n -> Just n
    Op1 Neg (NumLit n) -> Just (negate n)
    _ -> Nothing

asExpr :: Expr -> Expr.Expr
asExpr expr =
  case expr of
    Literal l -> doLit l
    Op1 op1 e1 -> doOp1 op1 e1
    Op2 op2 e1 e2 -> doOp2 op2 e1 e2
    Var i -> Expr.Var i
    If p t f -> Expr.If (asExpr p) (asExpr t) (asExpr f)
    Fail err -> error err

  where
    doLit l =
      case l of
        Num d -> Expr.LitD d
        Bool b -> Expr.LitB b

    doOp1 op1 e1 =
      case op1 of
        Not -> Expr.Not (asExpr e1)
        Neg -> Expr.Neg (asExpr e1)
        Exp -> Expr.Exp (asExpr e1)
        Log -> Expr.Log (asExpr e1)

    doOp2 op2 e1 e2 =
      case op2 of
        Add -> Expr.Add (asExpr e1) (asExpr e2)
        Mul -> Expr.Mul (asExpr e1) (asExpr e2)
        Sub -> Expr.Sub (asExpr e1) (asExpr e2)
        Div -> Expr.Div (asExpr e1) (asExpr e2)
        Lt -> Expr.LT (asExpr e1) (asExpr e2)
        Leq -> Expr.LTE (asExpr e1) (asExpr e2)
        Eq -> Expr.EQ (asExpr e1) (asExpr e2)
        And -> Expr.And (asExpr e1) (asExpr e2)
        Or -> Expr.Or (asExpr e1) (asExpr e2)
        Pow -> Expr.Pow (asExpr e1) (asExpr e2)
