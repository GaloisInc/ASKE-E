{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.SBML.L3V2.Parse where

import Control.Monad ( unless )

import Language.ASKEE.SBML.L3V2.Syntax
import Language.ASKEE.SBML.Common.Parse
import Language.ASKEE.SBML.Common.Syntax

import Text.Printf    ( printf )
import Text.XML.Light ( Element(..) )

parseSBML :: Element -> Parser SBML
parseSBML e =
  do  guardName "sbml" e
      sbmlLevel <- reqAttr parseRead e "level"
      sbmlVersion <- reqAttr parseRead e "version"
      unless (sbmlLevel == 3 && sbmlVersion == 2) $
        die $ printf "unsupported SBML version: %i.%i" sbmlLevel sbmlVersion
      sbmlModel <- optChild parseModel e "model"
      pure SBML{..}

parseModel :: Element -> Parser Model
parseModel e =
  do  guardName "model" e
      modelName <- optAttr parseText e "name"
      let modelFunctionDefs = Nothing
      modelUnitDefs <-
        optChild (appChildren parseUnitDef) e "listOfUnitDefinitions"
      modelCompartments <- 
        optChild (appChildren parseCompartment) e "listOfCompartments"
      modelSpecies <- 
        optChild (appChildren parseSpecies) e "listOfSpecies"
      modelParameters <- 
        optChild (appChildren parseParameter) e "listOfParameters"
      modelInitialAssignments <- 
        optChild (appChildren parseInitialAssignment) e "listOfInitialAssignments"
      let modelRules = Nothing
      let modelConstraints = Nothing
      modelReactions <- 
        optChild (appChildren parseReaction) e "listOfReactions"
      let modelEvents = Nothing

      pure Model{..}

parseUnitDef :: Element -> Parser UnitDef
parseUnitDef e =
  do  guardName "unitDefinition" e
      unitDefID <- reqAttr parseText e "id"
      unitDefUnits <-
        optChild (appChildren parseUnit) e "listOfUnits"
      pure UnitDef{..}

parseUnit :: Element -> Parser Unit
parseUnit e =
  do  guardName "unit" e
      unitKind <- reqAttr parseUnitKind e "kind"
      unitExponent <- reqAttr parseRead e "exponent"
      unitScale <- reqAttr parseRead e "scale"
      unitMultiplier <- reqAttr parseRead e "multiplier"
      pure Unit{..}

parseUnitKind :: String -> Parser UnitKind
parseUnitKind s =
  case s of
    "ampere" -> pure Ampere
    "avogadro" -> pure Avogadro
    "becquerel" -> pure Becquerel
    "candela" -> pure Candela
    "coulomb" -> pure Coulomb
    "dimensionless" -> pure Dimensionless
    "farad" -> pure Farad
    "gram" -> pure Gram
    "gray" -> pure Gray
    "henry" -> pure Henry
    "hertz" -> pure Hertz
    "item" -> pure Item
    "joule" -> pure Joule
    "katal" -> pure Katal
    "kelvin" -> pure Kelvin
    "kilogram" -> pure Kilogram
    "litre" -> pure Litre
    "lumen" -> pure Lumen
    "lux" -> pure Lux
    "metre" -> pure Metre
    "mole" -> pure Mole
    "newton" -> pure Newton
    "ohm" -> pure Ohm
    "pascal" -> pure Pascal
    "radian" -> pure Radian
    "second" -> pure Second
    "siemens" -> pure Siemens
    "sievert" -> pure Sievert
    "steradian" -> pure Steradian
    "tesla" -> pure Tesla
    "volt" -> pure Volt
    "watt" -> pure Watt
    "weber" -> pure Weber
    _ -> die $ printf "unknown unit kind '%s'" s

parseCompartment :: Element -> Parser Compartment
parseCompartment e =
  do  guardName "compartment" e
      compartmentID         <- reqAttr parseText e "id"
      compartmentDimensions <- optAttr parseRead e "spatialDimensions"
      compartmentSize       <- optAttr parseRead e "size"
      compartmentUnits      <- optAttr parseText e "units"
      compartmentConstant   <- reqAttr parseBool e "constant"
      pure Compartment{..}

parseSpecies :: Element -> Parser Species
parseSpecies e =
  do  guardName "species" e
      speciesID                    <- reqAttr parseText e "id"
      speciesName                  <- optAttr parseText e "name"
      speciesCompartment           <- reqAttr parseText e "compartment"
      speciesInitialAmount         <- optAttr parseRead e "initialAmount"
      speciesInitialConc           <- optAttr parseRead e "initialConcentration"
      speciesSubstanceUnits        <- optAttr parseText e "substanceUnits"
      speciesHasOnlySubstanceUnits <- reqAttr parseBool e "hasOnlySubstanceUnits"
      speciesBoundaryCondition     <- reqAttr parseBool e "boundaryCondition"
      speciesConstant              <- reqAttr parseBool e "constant"
      speciesConversionFactor      <- optAttr parseText e "conversionFactor"
      pure Species{..}

parseParameter :: Element -> Parser Parameter
parseParameter e =
  do  guardName "parameter" e
      parameterID       <- reqAttr parseText e "id"
      parameterName     <- optAttr parseText e "name"
      parameterValue    <- optAttr parseRead e "value"
      parameterUnits    <- optAttr parseText e "units"
      parameterConstant <- reqAttr parseBool e "constant"
      pure Parameter{..}

parseInitialAssignment :: Element -> Parser InitialAssignment
parseInitialAssignment e =
  do  guardName "initialAssignment" e
      initialID     <- optAttr parseText e "id"
      initialSymbol <- reqAttr parseText e "symbol"
      initialMath   <- reqChild parseMath e "math"
      pure InitialAssignment{..}

parseReaction :: Element -> Parser Reaction
parseReaction e =
  do  guardName "reaction" e
      reactionID <- reqAttr parseText e "id"
      reactionReversible <- reqAttr parseBool e "reversible"
      reactionCompartment <- optAttr parseText e "compartment"
      reactionReactants <- 
        optChild (appChildren parseSpeciesRef) e "listOfReactants"
      reactionProducts <- 
        optChild (appChildren parseSpeciesRef) e "listOfProducts"
      reactionModifiers <-
        optChild (appChildren parseModifierSpeciesRef) e "listOfModifiers"
      reactionKineticLaw <-
        optChild parseKineticLaw e "kineticLaw"
      pure Reaction{..}

parseSpeciesRef :: Element -> Parser SpeciesRef
parseSpeciesRef e =
  do  guardName "speciesReference" e
      speciesRefID <- optAttr parseText e "id"
      speciesRefName <- optAttr parseText e "name"
      speciesRefSpecies <- reqAttr parseText e "species"
      speciesRefStoichiometry <- optAttr parseRead e "stoichiometry"
      speciesRefConstant <- reqAttr parseBool e "constant"
      pure SpeciesRef{..}

parseModifierSpeciesRef :: Element -> Parser ModifierSpeciesRef
parseModifierSpeciesRef e =
  do  guardName "modifierSpeciesReference" e
      modifierSpeciesRefID <- optAttr parseText e "id"
      modifierSpeciesRefSpecies <- reqAttr parseText e "species"
      pure ModifierSpeciesRef{..}

parseKineticLaw :: Element -> Parser KineticLaw
parseKineticLaw e =
  do  guardName "kineticLaw" e
      kineticMath <- optChild parseMath e "math"
      kineticLocalParams <-
        optChild (appChildren parseLocalParam) e "listOfLocalParameters"
      pure KineticLaw{..}

parseLocalParam :: Element -> Parser LocalParam
parseLocalParam e =
  do  guardName "localParameter" e
      localParamID <- reqAttr parseText e "id"
      localParamValue <- optAttr parseRead e "value"
      localParamUnits <- optAttr parseText e "units"
      pure LocalParam{..}

