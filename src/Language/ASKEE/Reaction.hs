module Language.ASKEE.Reaction where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)

-- [1] https://catalyst.sciml.ai/stable/tutorials/basics/

{-
Concrete syntax:

unconditional:
  ArithExp ',' ReactionExp '-->' ReactionExp

guarded:
  ArithExp 'when' LogicalExpression ',' ReactionExp '-->' ReactionExp 

-}


-- TASK: factor this into its own language - this is like the 2rd version of this
data LogicalExpression =
    And LogicalExpression LogicalExpression
  | Or LogicalExpression LogicalExpression
  | Not LogicalExpression
  | LessThan ArithExp ArithExp
  | GreaterThan ArithExp ArithExp
  | Equals ArithExp ArithExp

-- TASK: factor this into its own language - this is like the 3rd version of this
data ArithExp =
    Var Text
  | Lit Double
  | Add ArithExp ArithExp
  | Mul ArithExp ArithExp
  | Sub ArithExp ArithExp
  | Div ArithExp ArithExp
  | Neg ArithExp

-- for a surface syntax, we may also want to support the "multiple reactions in one line" from [1]
data Reaction =
  Reaction { reactionRate       :: ArithExp
           , reactionEnabled    :: Maybe LogicalExpression
           , reactionSubstrates :: ReactionExp
           , reactionProducts   :: ReactionExp 
           } 

data ReactionExp = Nil | ReactionTerms [ReactionTerm]
data ReactionTerm = ReactTerm Int Text

reactionEffects :: ReactionExp -> ReactionExp -> Map Text ArithExp
reactionEffects substrates products =
    Map.mapWithKey reifyEffect effs 
  where
    effs = Map.unionWith (.) (substrateEffects substrates) (productEffects products)
    reifyEffect k eff = eff (Var k)

substrateEffects :: ReactionExp -> Map Text (ArithExp -> ArithExp)
substrateEffects = reactionExpEffects Sub

productEffects :: ReactionExp -> Map Text (ArithExp -> ArithExp)
productEffects = reactionExpEffects Add

reactionExpEffects :: (ArithExp -> ArithExp -> ArithExp) -> ReactionExp -> Map Text (ArithExp -> ArithExp)
reactionExpEffects effectOp rexp =
  case rexp of
    Nil -> Map.empty
    ReactionTerms ts -> Map.fromList (fromTerm <$> ts)
  where
    fromTerm (ReactTerm scale n) = (n, \e -> e `effectOp` Lit (fromIntegral scale))
