{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.SBML.L2V3.Parse where

import Control.Monad ( when )

import Language.ASKEE.SBML.Common.Parse
import Language.ASKEE.SBML.Common.Syntax
import Language.ASKEE.SBML.L2V3.Syntax
import Text.Printf    ( printf )
import Text.XML.Light

parseSBML :: Element -> Parser SBML
parseSBML e =
  do  sbmlLevel <- reqAttr parseRead e "level"
      sbmlVersion <- reqAttr parseRead e "version"
      when (sbmlLevel /= 2 || sbmlVersion /= 3) $
        die ""
      sbmlNotes <- optChild parseNotes e "notes"
      sbmlAnnotation <- optChild parseAnnotation e "annotation"
      sbmlModel <- reqChild parseModel e "model"
      pure SBML{..}

parseModel :: Element -> Parser Model
parseModel e =
  do  modelID <- optAttr parseText e "id"
      modelName <- optAttr parseText e "name"
      modelFunctionDefs <-
        optChild (appChildren parseFunction) e "listOfFunctionDefinitions"
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
      modelRules <-
        optChild (appChildren parseRule) e "listOfRules"
      modelReactions <-
        optChild (appChildren parseReaction) e "listOfReactions"

      let modelCompartmentTypes    = Nothing
      let modelSpeciesTypes        = Nothing
      let modelConstraints         = Nothing
      let modelEvents              = Nothing

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
      unitExponent <- optAttrDef 1 parseRead e "exponent"
      unitScale <- optAttrDef 0 parseRead e "scale"
      unitMultiplier <- optAttrDef 1 parseRead e "multiplier"
      pure Unit{..}

parseUnitKind :: String -> Parser UnitKind
parseUnitKind s =
  case s of
    "ampere" -> pure Ampere
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
      compartmentName       <- optAttr parseText e "name"
      compartmentType       <- optAttr parseText e "compartmentType"
      compartmentDimensions <- optAttrDef 3 parseRead e "spatialDimensions"
      compartmentSize       <- optAttr parseRead e "size"
      compartmentUnits      <- optAttr parseText e "units"
      compartmentOutside    <- optAttr parseText e "outside"
      compartmentConstant   <- optAttrDef True parseBool e "constant"
      compartmentAnnotation <- optChild parseAnnotation e "annotation"
      pure Compartment{..}

parseSpecies :: Element -> Parser Species
parseSpecies e =
  do  guardName "species" e
      speciesID                    <- reqAttr parseText e "id"
      speciesName                  <- optAttr parseText e "name"
      speciesType                  <- optAttr parseText e "speciesType"
      speciesCompartment           <- reqAttr parseText e "compartment"
      speciesInitialAmount         <- optAttr parseRead e "initialAmount"
      speciesInitialConc           <- optAttr parseRead e "initialConcentration"
      speciesSubstanceUnits        <- optAttr parseText e "substanceUnits"
      speciesHasOnlySubstanceUnits <- optAttrDef False parseBool e "hasOnlySubstanceUnits"
      speciesBoundaryCondition     <- optAttrDef False parseBool e "boundaryCondition"
      speciesCharge                <- optAttr parseRead e "charge"
      speciesConstant              <- optAttrDef False parseBool e "constant"
      speciesNotes                 <- optChild parseNotes e "notes"
      speciesAnnotation            <- optChild parseAnnotation e "annotation"
      pure Species{..}

parseParameter :: Element -> Parser Parameter
parseParameter e =
  do  guardName "parameter" e
      parameterID       <- reqAttr parseText e "id"
      parameterName     <- optAttr parseText e "name"
      parameterValue    <- optAttr parseRead e "value"
      parameterUnits    <- optAttr parseText e "units"
      parameterConstant <- optAttrDef True parseBool e "constant"
      parameterNotes    <- optChild parseNotes e "notes"
      pure Parameter{..}

parseInitialAssignment :: Element -> Parser InitialAssignment
parseInitialAssignment e =
  do  guardName "initialAssignment" e
      initialSymbol <- reqAttr parseText e "symbol"
      initialMath   <- reqChild parseMath e "math"
      pure InitialAssignment{..}

parseRule :: Element -> Parser Rule
parseRule e =
  case qName (elName e) of
    "algebraicRule" -> parseAlgebraicRule e
    "assignmentRule" -> parseAssignmentRule e
    "rateRule" -> parseRateRule e
    n -> die $ printf "undefined rule type '%s'" n

parseAlgebraicRule :: Element -> Parser Rule
parseAlgebraicRule e =
  do  guardName "algebraicRule" e
      ruleMath <- reqChild parseMath e "math"
      pure AlgebraicRule{..}

parseAssignmentRule :: Element -> Parser Rule
parseAssignmentRule e =
  do  guardName "assignmentRule" e
      ruleVariable <- reqAttr parseText e "variable"
      ruleMath <- reqChild parseMath e "math"
      pure AssignmentRule{..}

parseRateRule :: Element -> Parser Rule
parseRateRule e =
  do  guardName "rateRule" e
      ruleVariable <- reqAttr parseText e "variable"
      ruleMath <- reqChild parseMath e "math"
      pure RateRule{..}

parseReaction :: Element -> Parser Reaction
parseReaction e =
  do  guardName "reaction" e
      reactionID <- reqAttr parseText e "id"
      reactionName <- optAttr parseText e "name"
      reactionReversible <- optAttrDef True parseBool e "reversible"
      reactionFast <- optAttrDef False parseBool e "fast"
      reactionReactants <-
        optChild (appChildren parseSpeciesRef) e "listOfReactants"
      reactionProducts <-
        optChild (appChildren parseSpeciesRef) e "listOfProducts"
      reactionModifiers <-
        optChild (appChildren parseModifierSpeciesRef) e "listOfModifiers"
      reactionKineticLaw <- optChild parseKineticLaw e "kineticLaw"
      pure Reaction{..}

parseSpeciesRef :: Element -> Parser SpeciesRef
parseSpeciesRef e =
  do  guardName "speciesReference" e
      speciesRefID <- optAttr parseText e "id"
      speciesRefName <- optAttr parseText e "name"
      speciesRefSpecies <- reqAttr parseText e "species"
      speciesRefStoichiometry <- optAttrDef 1 parseRead e "stoichiometry"
      speciesRefStoichiometryMath <- optChild parseMath e "stoichiometryMath"
      speciesRefConstant <- reqAttr parseBool e "constant"
      pure SpeciesRef{..}

parseModifierSpeciesRef :: Element -> Parser ModifierSpeciesRef
parseModifierSpeciesRef e =
  do  guardName "modifierSpeciesReference" e
      modifierSpeciesRefID <- optAttr parseText e "id"
      modifierSpeciesRefName <- optAttr parseText e "name"
      modifierSpeciesRefSpecies <- reqAttr parseText e "species"
      pure ModifierSpeciesRef{..}

parseKineticLaw :: Element -> Parser KineticLaw
parseKineticLaw e =
  do  guardName "kineticLaw" e
      kineticMath <- optChild parseMath e "math"
      kineticLocalParams <-
        optChild (appChildren parseParameter) e "listOfParameters"
      pure KineticLaw{..}
