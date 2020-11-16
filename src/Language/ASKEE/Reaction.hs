module Language.ASKEE.Reaction where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import Language.ASKEE.Expr ( Expr(..) )
import qualified Data.Set as Set
import qualified Language.ASKEE.Syntax as Syntax
import qualified Data.List as List

-- [1] https://catalyst.sciml.ai/stable/tutorials/basics/

{-
Concrete syntax:

unconditional:
  Expr ',' ReactionExp '-->' ReactionExp

guarded:
  Expr 'when' Expr ',' ReactionExp '-->' ReactionExp 

-}

-- for a surface syntax, we may also want to support the "multiple reactions in one line" from [1]
-- TODO: fixed actions instead of mass actions
data Reaction =
  Reaction { reactionRate       :: Expr
           , reactionType       :: ReactionRate
           , reactionEnabled    :: Maybe Expr
           , reactionSubstrates :: ReactionExp
           , reactionProducts   :: ReactionExp 
           } 

data ReactionExp = Nil | ReactionTerms [ReactionTerm]
data ReactionTerm = ReactionTerm Int Text
data ReactionRate = MassAction | FixedRate

reactionEffects :: ReactionExp -> ReactionExp -> Map Text Expr
reactionEffects substrates products =
    Map.mapWithKey reifyEffect effs 
  where
    effs = Map.unionWith (.) (substrateEffects substrates) (productEffects products)
    reifyEffect k eff = eff (Var k)

substrateEffects :: ReactionExp -> Map Text (Expr -> Expr)
substrateEffects = reactionExpEffects Sub

productEffects :: ReactionExp -> Map Text (Expr -> Expr)
productEffects = reactionExpEffects Add

reactionExpEffects :: (Expr -> Expr -> Expr) -> ReactionExp -> Map Text (Expr -> Expr)
reactionExpEffects effectOp rexp =
  case rexp of
    Nil -> Map.empty
    ReactionTerms ts -> Map.fromList (fromTerm <$> ts)
  where
    fromTerm (ReactionTerm scale n) = (n, \e -> e `effectOp` LitD (fromIntegral scale))

reactionRateExp :: Reaction -> Expr
reactionRateExp r =
  case reactionType r of
    FixedRate -> reactionRate r
    MassAction ->
      case reactionSubstrates r of
        Nil -> reactionRate r -- TODO: is this right?
        ReactionTerms ts -> sumTerms $ rterm <$> ts
  where
    rterm (ReactionTerm scaling v) = pow (Var v) scaling
    pow x y = (iterate (Mul x) x) !! (y - 1)
    sumTerms terms =
      case terms of
        [] -> LitD 0
        t0:ts -> foldr Add t0 ts


reactionAsEvent :: Text -> Reaction -> Syntax.Event
reactionAsEvent name r = 
  Syntax.Event { Syntax.eventName = name
               , Syntax.eventWhen = reactionEnabled r
               , Syntax.eventEffect = effects
               , Syntax.eventMetadata = Nothing
               , Syntax.eventRate = reactionRateExp r
               }
  where
    effMap = reactionEffects (reactionSubstrates r) (reactionProducts r)  
    effects = Map.toList effMap

-- TODO: really inefficient
tsort :: (i -> i -> Bool) -> [i] -> Maybe [i]
tsort dependsOn elts =
  case List.partition noDeps elts of
    (l, []) -> Just l
    ([], _) -> Nothing
    (l1, l2) ->
      do  rst <- tsort dependsOn l2
          return $ l1 ++ rst
  where
    noDeps e = not $ any (e `dependsOn`) elts 

-- make a model with automatically generated names
reactionsAsModel :: Text -> Map Text (Expr) -> [Reaction] -> Either String Syntax.Model 
reactionsAsModel mname env rs = undefined
  where
    reactionVar (ReactionTerm _ v) = v
    reactionVars Nil = []
    reactionVars (ReactionTerms ts) = reactionVar <$> ts
    
    reactionStateVars r = reactionVars (reactionSubstrates r) 
                       <> reactionVars (reactionProducts r)

    stateVars = rs >>= reactionStateVars

    mkStateDecl v =
      do e <- Map.lookup v env
         return (Syntax.State v e)

    stateDecls =
      case mkStateDecl `traverse` stateVars of
        -- TODO: which?
        Nothing -> Left "some state vars do not have initial conditions"
        Just decls -> Right decls

    letDecls = Map.withoutKeys env (Set.fromList stateVars)
         


