{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}

module Language.ASKEE.Core.Syntax where

import qualified Data.Functor.Const as Const
import           Data.Map  ( Map )
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           Data.Text ( Text )

import Language.ASKEE.Core.Expr

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





modelStateVars :: Model -> [Ident]
modelStateVars mdl = fst <$> Map.toList (modelInitState mdl)

isStateVar :: Ident -> Model -> Bool
isStateVar x m = Map.member x (modelInitState m)



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





-- | Inline all occurances of let-bound variables.
inlineLets :: Model -> Model
inlineLets model = model { modelEvents = map substEvent (modelEvents model)
                         , modelLets   = su
                         }
  where
  su         = substExpr su <$> modelLets model
  substEvent = mapExprs (substExpr su)


-- | Instantiate some of the model parameters
applyParams' :: Map Ident Expr -> Model -> Model
applyParams' su = dropParams . mapExprs (substExpr su)
  where
  dropParams m = m { modelParams = [ x | x <- modelParams m
                                       , not (x `Set.member` pSet) ] }
  pSet = Map.keysSet su



collect :: (TraverseExprs t, Monoid m) => (Expr -> m) -> t -> m
collect f t =
  Const.getConst $ traverseExprs (Const.Const . f) t

collectVars :: (TraverseExprs t) => t -> Set.Set Ident
collectVars = collect var
  where
    var v =
      case v of
        Var n -> Set.singleton n
        _ -> Set.empty

applyParams :: Map Text Double -> Model -> Model
applyParams parameters = applyParams' parameters'
  where
    parameters' = Map.map NumLit parameters