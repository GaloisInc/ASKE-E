{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Language.ASKEE.SBML.L2V3.Convert where

import Control.Monad.Identity

import qualified Data.Map   as Map
import           Data.Maybe
import qualified Data.Set   as Set

import Language.ASKEE.ESL.Syntax          as ESL
import Language.ASKEE.Expr
import Language.ASKEE.ExprTransform       ( collectExprVars
                                          , substitute)
import Language.ASKEE.SBML.Common.Syntax  as SBML
import Language.ASKEE.SBML.L2V3.Syntax    as SBML

import Prelude hiding ( GT )

sbmlToESL :: SBML.SBML -> ESL.Model
sbmlToESL SBML{..} = modelToESL sbmlModel

modelToESL :: SBML.Model -> ESL.Model
modelToESL SBML.Model{..} = 
  ESL.Model "foo" (map pure (parametersLets ++ speciesStates)) reactionEvents mempty
  where

    letOrParam v e =
      case eval paramMap e of
        Right d -> ESL.Parameter v (Just (LitD d))
        Left _ -> Let v e
    paramMap = Map.fromList (map parameterExpr parameterList)
    parametersLets = [ letOrParam v e | (v, e) <- map parameterExpr parameterList ]
    speciesStates = [ State v e | (v, e) <- map speciesExpr speciesList ]
    reactionEvents = concatMap reactionAsEvents reactionList

    parameterList = fromMaybe mempty modelParameters
    speciesList = fromMaybe mempty modelSpecies
    reactionList = fromMaybe mempty modelReactions

    parameterExpr SBML.Parameter{..} = (parameterID,)
      case override Map.!? parameterID of
        Nothing -> LitD (fromJust parameterValue)
        Just e -> e

    speciesExpr Species{..} = (speciesID,)
      case override Map.!? speciesID of
        Nothing ->
          case (speciesInitialAmount, speciesInitialConc) of
            (Just ia, Nothing) -> LitD ia
            (Nothing, Just ic) -> LitD ic `Mul` (compartmentSizes Map.! speciesCompartment)
            _ -> undefined
        Just e ->
          if speciesHasOnlySubstanceUnits
            then e
            else e `Mul` (compartmentSizes Map.! speciesCompartment)

    override =
      let initAssigns = fromMaybe mempty modelInitialAssignments
          rules = fromMaybe mempty modelRules
      in  Map.fromList $
            [ (ident, val)
            | InitialAssignment ident val <- initAssigns ] 
            ++
            [ (ident, val)
            | AssignmentRule ident val <- rules ]

    compartmentSizes = Map.fromList
      [ (compartmentID, size)
      | Compartment{..} <- fromMaybe mempty modelCompartments 
      , let size = maybe (override Map.! compartmentID) LitD compartmentSize ]

reactionAsEvents :: Reaction -> [ESL.Event]
reactionAsEvents Reaction{..}
  | reactionReversible = [forward, backward]
  | otherwise = [forward]
  where
    forward = ESL.Event (reactionID<>"_forward") forwardWhen forwardRate forwardEffect mempty
    backward = ESL.Event (reactionID<>"_backward") backwardWhen backwardRate backwardEffect mempty

    forwardRate = findProductWith reactants kMath
    backwardRate = findProductWith products kMath

    forwardWhen = nonemptySpecies reactants
    backwardWhen = nonemptySpecies products

    forwardEffect = reactionEffect reactants products
    backwardEffect = reactionEffect products reactants

    -- this is an implementation choice - we treat an absent reaction
    -- rate as a rate of zero
    kMath = fromMaybe (LitD 0) (reactionKineticLaw >>= kineticMath)

    reactants = fromMaybe mempty reactionReactants
    products = fromMaybe mempty reactionProducts

-- condition that every species in the provided list is nonempty
nonemptySpecies :: [SpeciesRef] -> Maybe Expr
nonemptySpecies species =
  case species of
    [] -> Nothing
    _ -> Just $
      foldl1 And 
        [ Var speciesRefSpecies `GT` LitD 0
        | SpeciesRef{..} <- species ]

-- draw from reactants, add to products
reactionEffect :: [SpeciesRef] -> [SpeciesRef] -> [Statement]
reactionEffect reactants products =
  [ (speciesRefSpecies, Var speciesRefSpecies `Sub` LitD speciesRefStoichiometry)
  | SpeciesRef{..} <- reactants ]
  ++
  [ (speciesRefSpecies, Var speciesRefSpecies `Add` LitD speciesRefStoichiometry)
  | SpeciesRef{..} <- products ]
  
-- find the largest multiplication statement with (all? any?) of the
-- provided species as products
findProductWith :: [SpeciesRef] -> Expr -> Expr
findProductWith species expr =
  case findProduct expr of
    Left e -> e
    Right _ -> error (unlines ["", show species, show expr])
  where
    findProduct e =
      case e of
        Mul e1 e2
          | speciesNames `Set.isSubsetOf` (collectExprVars e1 <> collectExprVars e2) -> Left e
          | otherwise -> def2 e1 e2
        Add e1 e2 -> def2 e1 e2
        Sub e1 e2 -> def2 e1 e2
        Div e1 e2 -> def2 e1 e2
        Neg e' -> def1 e'
        Exp e' -> def1 e'
        Log e' -> def1 e'
        Pow e1 e2 -> def2 e1 e2
        Var _ -> Right e
        LitD _ -> Right e
        _ -> error (show e)

    def1 = findProduct
    def2 e1 e2 = def1 e1 >> def1 e2

    speciesNames = Set.fromList [ speciesRefSpecies | SpeciesRef{..} <- species ]

instantiate :: Maybe [SBML.Parameter] -> Expr -> Expr
instantiate paramsM e =
  case paramsM of
    Nothing -> e
    Just ps ->
      let paramMap = Map.fromList
            [ (parameterID, LitD (fromJust parameterValue))
            | SBML.Parameter{..} <- ps
            ]
      in  substitute paramMap e
