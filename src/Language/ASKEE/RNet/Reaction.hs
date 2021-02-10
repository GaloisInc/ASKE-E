{-# LANGUAGE TypeApplications #-}
module Language.ASKEE.RNet.Reaction where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text, pack)
import Language.ASKEE.Expr ( Expr(..) )
import qualified Data.Set as Set
import qualified Language.ASKEE.Syntax as Syntax
import Language.ASKEE.RNet.Syntax
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
        ReactionTerms ts -> Mul (reactionRate r) (mulTerms $ rterm <$> ts)
  where
    rterm (ReactionTerm scaling v) = pow (Var v) scaling
    pow x y = iterate (Mul x) x !! (y - 1)
    mulTerms terms =
      case terms of
        [] -> LitD 1
        t0:ts -> foldr Mul t0 ts


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
reactionsAsModel :: Text -> Map Text Expr -> [Reaction] -> Either String Syntax.Model 
reactionsAsModel mname env rs = 
  do  stDecls <- stateDecls
      Right $ Syntax.Model mname (stDecls ++ letDecls) (map (uncurry reactionAsEvent) namedReactions) 
      
  where
    reactionVar :: ReactionTerm -> Text
    reactionVar (ReactionTerm _ v) = v

    reactionVars :: ReactionExp -> [Text]
    reactionVars Nil = []
    reactionVars (ReactionTerms ts) = reactionVar <$> ts
    
    reactionStateVars :: Reaction -> [Text]
    reactionStateVars r = reactionVars (reactionSubstrates r) 
                       <> reactionVars (reactionProducts r)

    stateVars :: [Text]
    stateVars = Set.toList $ Set.fromList $ rs >>= reactionStateVars

    mkStateDecl :: Text -> Maybe Syntax.Decl
    mkStateDecl v =
      do e <- Map.lookup v env
         return (Syntax.State v e)

    stateDecls :: Either String [Syntax.Decl]
    stateDecls =
      case mkStateDecl `traverse` stateVars of
        -- TODO: which?
        Nothing -> Left "some state vars do not have initial conditions"
        Just decls -> Right decls

    letDecls :: [Syntax.Decl]
    letDecls = map (uncurry Syntax.Let) $ Map.toList $ Map.withoutKeys env (Set.fromList stateVars)

    namedReactions :: [(Text, Reaction)]
    namedReactions = zip names rs
      where
        -- names = map chrToTxt ['a'..'z']
        -- chrToTxt = pack . (:[])

        ltrs = map (pack . (:[])) ['a'..'z']
        ids = map (pack . show @Int) [1..]
        names = concatMap (\i -> map (<> i) ltrs) ids
        -- names = map (\i -> show (i+1) ++ [letters !! (i `mod` 26)]) [0..]
        -- idx i xs = xs !! 

         


