module Language.ASKEE.Reaction where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import Language.ASKEE.Expr ( ArithExpr( ALit, Var, Add, Sub ), LogExpr )

-- [1] https://catalyst.sciml.ai/stable/tutorials/basics/

{-
Concrete syntax:

unconditional:
  ArithExp ',' ReactionExp '-->' ReactionExp

guarded:
  ArithExp 'when' LogExp ',' ReactionExp '-->' ReactionExp 

-}


-- for a surface syntax, we may also want to support the "multiple reactions in one line" from [1]
data Reaction =
  Reaction { reactionRate       :: ArithExpr
           , reactionEnabled    :: Maybe LogExpr
           , reactionSubstrates :: ReactionExp
           , reactionProducts   :: ReactionExp 
           } 

data ReactionExp = Nil | ReactionTerms [ReactionTerm]
data ReactionTerm = ReactTerm Int Text

reactionEffects :: ReactionExp -> ReactionExp -> Map Text ArithExpr
reactionEffects substrates products =
    Map.mapWithKey reifyEffect effs 
  where
    effs = Map.unionWith (.) (substrateEffects substrates) (productEffects products)
    reifyEffect k eff = eff (Var k)

substrateEffects :: ReactionExp -> Map Text (ArithExpr -> ArithExpr)
substrateEffects = reactionExpEffects Sub

productEffects :: ReactionExp -> Map Text (ArithExpr -> ArithExpr)
productEffects = reactionExpEffects Add

reactionExpEffects :: (ArithExpr -> ArithExpr -> ArithExpr) -> ReactionExp -> Map Text (ArithExpr -> ArithExpr)
reactionExpEffects effectOp rexp =
  case rexp of
    Nil -> Map.empty
    ReactionTerms ts -> Map.fromList (fromTerm <$> ts)
  where
    fromTerm (ReactTerm scale n) = (n, \e -> e `effectOp` ALit (fromIntegral scale))
