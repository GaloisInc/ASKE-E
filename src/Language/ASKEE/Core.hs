{-# Language PatternSynonyms, ApplicativeDo, RankNTypes #-}
module Language.ASKEE.Core where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Functor.Identity(runIdentity)

type Ident = Text

data Model =
  Model { modelName      :: Text
        , modelParams    :: [Ident]
        , modelInitState :: Map Ident Expr
        , modelEvents    :: [Event]
        , modelLets      :: Map Ident Expr
          -- ^ These are the `let` bound variables from the original model
          -- We keep them here, in case one wants to observe them
          -- (e.g., measure their values)
          -- These should not be recursive.
        }
  deriving (Show, Eq)


data Event =
  Event { eventName   :: Ident
        , eventRate   :: Expr
        , eventWhen   :: Expr
        , eventEffect :: Map Ident Expr
        }
  deriving (Show, Eq)


data Expr =
    Literal Literal
  | Op1 Op1 Expr
  | Op2 Op2 Expr Expr
  | Var Ident
  | If Expr Expr Expr
  | Fail String
    deriving (Show,Eq)

data Op1 = Not | Neg
  deriving (Show,Eq)

data Op2 = Add | Mul | Sub | Div | Lt | Leq | Eq | And | Or
  deriving (Show,Eq)

data Literal =
    Num Double
  | Bool Bool
    deriving (Show,Eq)


--------------------------------------------------------------------------------
-- Conveninece

pattern (:+:) :: Expr -> Expr -> Expr
pattern (:+:) e1 e2 = Op2 Add e1 e2

pattern (:-:) :: Expr -> Expr -> Expr
pattern (:-:) e1 e2 = Op2 Sub e1 e2

pattern (:*:) :: Expr -> Expr -> Expr
pattern (:*:) e1 e2 = Op2 Mul e1 e2

pattern BoolLit :: Bool -> Expr
pattern BoolLit x = Literal (Bool x)

pattern NumLit :: Double -> Expr
pattern NumLit  x = Literal (Num  x)


modelStateVars :: Model -> [Ident]
modelStateVars mdl = fst <$> Map.toList (modelInitState mdl)

isStateVar :: Ident -> Model -> Bool
isStateVar x m = Map.member x (modelInitState m)



--------------------------------------------------------------------------------
-- One level traversals

-- | Traverse the expressions in `t`
class TraverseExprs t where
  traverseExprs :: Applicative f => (Expr -> f Expr) -> t -> f t

instance TraverseExprs Model where
  traverseExprs f m =
    do ins  <- traverse f                 (modelInitState m)
       evs  <- traverse (traverseExprs f) (modelEvents m)
       lets <- traverse f                 (modelLets m)
       pure m { modelInitState = ins, modelEvents = evs, modelLets = lets }

instance TraverseExprs Event where
  traverseExprs f ev =
    do rate <- f (eventRate ev)
       cond <- f (eventWhen ev)
       eff  <- traverse f (eventEffect ev)
       pure ev { eventRate = rate, eventWhen = cond, eventEffect = eff }

-- | Traverse the immediate children of an expression
instance TraverseExprs Expr where
  traverseExprs f expr =
    case expr of
      Literal {}   -> pure expr
      Op1 op e     -> Op1 op <$> f e
      Op2 op e1 e2 -> Op2 op <$> f e1 <*> f e2
      Var {}       -> pure expr
      If e1 e2 e3  -> If <$> f e1 <*> f e2 <*> f e3
      Fail {}      -> pure expr

-- | Apply a function to expressions contained in something.
mapExprs :: TraverseExprs t => (Expr -> Expr) -> t -> t
mapExprs f = runIdentity . traverseExprs (pure . f)

-- | Inline all occurances of let-bound variables.
inlineLets :: Model -> Model
inlineLets model = model { modelEvents = map substEvent (modelEvents model)
                         , modelLets   = su
                         }
  where
  su         = substExpr su <$> modelLets model
  substEvent = mapExprs (substExpr su)



-- | Replace variable with expressions as specified by the map.
substExpr :: Map Ident Expr -> Expr -> Expr
substExpr su expr =
  case expr of
    Var x | Just e <- Map.lookup x su -> e
    _ -> mapExprs (substExpr su) expr

