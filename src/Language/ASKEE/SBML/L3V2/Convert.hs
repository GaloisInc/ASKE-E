{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Language.ASKEE.SBML.L3V2.Convert where

import Control.Monad.Identity

import qualified Data.Map   as Map
import           Data.Maybe
import qualified Data.Set   as Set

import Language.ASKEE.ESL.Syntax        as ESL
import Language.ASKEE.Expr
import Language.ASKEE.ExprTransform     ( transformExpr
                                        , collectExprVars)
import Language.ASKEE.SBML.L3V2.Syntax  as SBML

import Prelude hiding ( GT )

sbmlToESL :: SBML.SBML -> [ESL.Model]
sbmlToESL SBML{..} =
  case sbmlModel of
    Nothing -> []
    Just m ->
      case modelCompartments m of
        Nothing -> mempty
        Just cs -> map (modelFromCompartment m) cs

modelFromCompartment :: SBML.Model -> Compartment -> ESL.Model
modelFromCompartment SBML.Model{..} Compartment{..} = 
  ESL.Model "foo" (map pure (parametersLets ++ speciesStates)) reactionEvents mempty
  where
    parametersLets = [ Let v e | (v, e) <- map parameterExpr parameterList ]
    speciesStates = [ State v e | (v, e) <- map speciesExpr speciesList ]
    reactionEvents = concatMap reactionAsEvents reactionList

    parameterList = fromMaybe mempty modelParameters
    speciesList =
      case modelSpecies of
        Nothing -> mempty
        Just ss -> filter (\Species{..} -> speciesCompartment == compartmentID) ss
    reactionList =
      case modelReactions of
        Nothing -> mempty
        Just rs -> filter (\Reaction{..} -> reactionCompartment == Just compartmentID || isNothing reactionCompartment) rs

    parameterExpr SBML.Parameter{..} = (parameterID,)
      case override Map.!? parameterID of
        Nothing -> LitD (fromJust parameterValue)
        Just e -> e

    speciesExpr Species{..} = (speciesID,)
      case override Map.!? speciesID of
        Nothing ->
          case (speciesInitialAmount, speciesInitialConc) of
            (Just ia, Nothing) -> LitD ia
            (Nothing, Just ic) -> LitD ic `Mul` compartmentSize'
            _ -> undefined
        Just e ->
          if speciesHasOnlySubstanceUnits
            then e
            else e `Mul` compartmentSize'

    override =
      case modelInitialAssignments of
        Nothing -> mempty
        Just initial -> Map.fromList
          [ (ident, val)
          | InitialAssignment _ ident val <- initial ]

    compartmentSize' =
      case (override Map.!? compartmentID, compartmentSize) of
        (Just e, _) -> e
        (Nothing, Just d) -> LitD d
        _ -> undefined

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
  [ (speciesRefSpecies, Var speciesRefSpecies `Sub` LitD stoich)
  | SpeciesRef{..} <- reactants
  , let stoich = fromMaybe 1 speciesRefStoichiometry ] ++
  [ (speciesRefSpecies, Var speciesRefSpecies `Add` LitD stoich)
  | SpeciesRef{..} <- products
  , let stoich = fromMaybe 1 speciesRefStoichiometry ]
  
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

instantiate :: Maybe [LocalParam] -> Expr -> Expr
instantiate paramsM e =
  case paramsM of
    Nothing -> e
    Just ps ->
      let paramMap = Map.fromList
            [ (localParamID, LitD (fromJust localParamValue))
            | LocalParam{..} <- ps
            ]
          replace expr =
            case expr of
              Var v ->
                case paramMap Map.!? v of
                  Just e' -> replace e'
                  Nothing -> pure expr
              _ -> pure expr
      in  runIdentity (transformExpr replace e)
