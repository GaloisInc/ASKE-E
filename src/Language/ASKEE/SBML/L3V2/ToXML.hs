{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.ASKEE.SBML.L3V2.ToXML
  ( sbmlToXML
  ) where

import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (Ordering(..))
import Text.XML.Light

import Language.ASKEE.Expr (Expr(..))
import Language.ASKEE.SBML.L3V2.Syntax
import Language.ASKEE.SBML.Common.Syntax

sbmlToXML :: SBML -> Text
sbmlToXML sbml =
  T.unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , T.pack $ ppElement $ unode "sbml" sbml
            ]

instance Node SBML where
  node n SBML{sbmlLevel, sbmlVersion, sbmlModel} =
    add_attrs [ attr "xmlns" "http://www.sbml.org/sbml/level3/version2/core"
              , attr "level" $ ppInt sbmlLevel
              , attr "version" $ ppInt sbmlVersion
              ] $
    node n $ catMaybes
      [ mbUnode "model" sbmlModel
      ]

instance Node Model where
  node n Model{ modelName
              , modelFunctionDefs, modelUnitDefs
              , modelCompartments, modelSpecies
              , modelParameters, modelInitialAssignments
              , modelRules, modelConstraints
              , modelReactions, modelEvents
              } =
    add_attrs (catMaybes [ mbAttr "name" ppText modelName
                         ]) $
    node n $ catMaybes
      [ listOf "FunctionDefinitions" (unode "functionDefinition") modelFunctionDefs
      , listOf "UnitDefinitions"     (unode "unitDefinition")     modelUnitDefs
      , listOf "Compartments"        (unode "compartment")        modelCompartments
      , listOf "Species"             (unode "species")            modelSpecies
      , listOf "Parameters"          (unode "parameter")          modelParameters
      , listOf "InitialAssignments"  (unode "initialAssignment")  modelInitialAssignments
      , listOf "Rules"               (unode "rule")               modelRules
      , listOf "Constraints"         (unode "constraint")         modelConstraints
      , listOf "Reactions"           (unode "reaction")           modelReactions
      , listOf "Events"              (unode "event")              modelEvents
      ]

instance Node Function where
  node _n x = case x of {}

instance Node UnitDef where
  node n UnitDef{unitDefID, unitDefUnits} =
    add_attr (attr "id" $ ppText unitDefID) $
    node n $ catMaybes
      [ listOf "Units" (unode "unit") unitDefUnits
      ]

instance Node Compartment where
  node n Compartment{ compartmentID, compartmentDimensions
                    , compartmentSize, compartmentUnits
                    , compartmentConstant
                    } =
    add_attrs (catMaybes [ justAttr "id" ppText compartmentID
                         , mbAttr   "spatialDimensions" ppDouble compartmentDimensions
                         , mbAttr   "size" ppDouble compartmentSize
                         , mbAttr   "units" ppText compartmentUnits
                         , justAttr "constant" ppBool compartmentConstant
                         ]) $
    node n ()

instance Node Species where
  node n Species{ speciesID, speciesName
                , speciesCompartment, speciesInitialAmount
                , speciesInitialConc, speciesSubstanceUnits
                , speciesHasOnlySubstanceUnits, speciesBoundaryCondition
                , speciesConstant, speciesConversionFactor
                } =
    add_attrs (catMaybes [ justAttr "id" ppText speciesID
                         , mbAttr   "name" ppText speciesName
                         , justAttr "compartment" ppText speciesCompartment
                         , mbAttr   "initialAmount" ppDouble speciesInitialAmount
                         , mbAttr   "initialConcentration" ppDouble speciesInitialConc
                         , mbAttr   "substanceUnits" ppText speciesSubstanceUnits
                         , justAttr "hasOnlySubstanceUnits" ppBool speciesHasOnlySubstanceUnits
                         , justAttr "boundaryCondition" ppBool speciesBoundaryCondition
                         , justAttr "constant" ppBool speciesConstant
                         , mbAttr   "conversionFactor" ppText speciesConversionFactor
                         ]) $
    node n ()

instance Node Parameter where
  node n Parameter{ parameterID, parameterName
                  , parameterValue, parameterUnits
                  , parameterConstant
                  } =
    add_attrs (catMaybes [ justAttr "id" ppText parameterID
                         , mbAttr   "name" ppText parameterName
                         , mbAttr   "value" ppDouble parameterValue
                         , mbAttr   "units" ppText parameterUnits
                         , justAttr "constant" ppBool parameterConstant
                         ]) $
    node n ()

instance Node InitialAssignment where
  node n InitialAssignment{initialID, initialSymbol, initialMath} =
    add_attrs (catMaybes [ mbAttr   "id" ppText initialID
                         , justAttr "symbol" ppText initialSymbol
                         ]) $
    node n
      [ unode "math" initialMath
      ]

instance Node Rule where
  node _n rule =
    case rule of
      AlgebraicRule  -> unode "algebraicRule"  ()
      AssignmentRule -> unode "assignmentRule" ()
      RateRule       -> unode "rateRule"       ()

instance Node Constraint where
  node _n x = case x of {}

instance Node Reaction where
  node n Reaction{ reactionID, reactionReversible
                 , reactionCompartment, reactionReactants
                 , reactionProducts, reactionModifiers
                 , reactionKineticLaw
                 } =
    add_attrs (catMaybes [ justAttr "id" ppText reactionID
                         , justAttr "reversible" ppBool reactionReversible
                         , mbAttr   "compartment" ppText reactionCompartment
                         ]) $
    node n $ catMaybes
      [ listOf "Reactants" (unode "speciesReference")         reactionReactants
      , listOf "Products"  (unode "speciesReference")         reactionProducts
      , listOf "Modifiers" (unode "modifierSpeciesReference") reactionModifiers
      , mbUnode "kineticLaw" reactionKineticLaw
      ]

instance Node Event where
  node _n x = case x of {}

instance Node Unit where
  node n Unit{ unitKind, unitExponent
             , unitScale, unitMultiplier
             } =
    add_attrs [ attr "kind" $ ppUnitKind unitKind
              , attr "exponent" $ ppDouble unitExponent
              , attr "scale" $ ppInt unitScale
              , attr "multiplier" $ ppDouble unitMultiplier
              ] $
    node n ()

instance Node Expr where
  node n expr =
    add_attrs [ attr "xmlns" "http://www.w3.org/1998/Math/MathML"
              ] $
    node n
      [ exprToElement expr
      ]
    where
      exprToElement :: Expr -> Element
      exprToElement e =
        case e of
          Neg e1    -> apply1 "minus" e1

          Add e1 e2 -> apply2 "plus" e1 e2
          Sub e1 e2 -> apply2 "minus" e1 e2
          Mul e1 e2 -> apply2 "times" e1 e2
          Div e1 e2 -> apply2 "divide" e1 e2
          Pow e1 e2 -> apply2 "power" e1 e2
          And e1 e2 -> apply2 "and" e1 e2
          Or e1 e2  -> apply2 "or" e1 e2

          Var  i    -> unode "ci" $ ppText i
          LitD d    -> unode "cn" $ ppDouble d

          Exp{}     -> notSupported "exp"
          Log{}     -> notSupported "log"
          Not{}     -> notSupported "not"

          LT{}      -> notInMathML "LT"
          LTE{}     -> notInMathML "LTE"
          EQ{}      -> notInMathML "EQ"
          GTE{}     -> notInMathML "GTE"
          GT{}      -> notInMathML "GT"
          If{}      -> notInMathML "If"
          Cond{}    -> notInMathML "Cond"
          LitB{}    -> notInMathML "LitB"

      notSupported :: String -> a
      notSupported name = error $
        "The `" ++ name ++ "` operator is not yet supported"

      notInMathML :: String -> a
      notInMathML name = error $
        "`" ++ name ++ "` does not have a counterpart in MathML"

      apply1 :: String -> Expr -> Element
      apply1 name arg =
        unode "apply"
          [ unode name ()
          , exprToElement arg
          ]

      apply2 :: String -> Expr -> Expr -> Element
      apply2 name arg1 arg2 =
        unode "apply"
          [ unode name ()
          , exprToElement arg1
          , exprToElement arg2
          ]

instance Node SpeciesRef where
  node n SpeciesRef{ speciesRefID, speciesRefName, speciesRefSpecies
                   , speciesRefStoichiometry, speciesRefConstant
                   } =
    add_attrs (catMaybes [ mbAttr   "id" ppText speciesRefID
                         , mbAttr   "name" ppText speciesRefName
                         , justAttr "species" ppText speciesRefSpecies
                         , mbAttr   "stoichiometry" ppDouble speciesRefStoichiometry
                         , justAttr "constant" ppBool speciesRefConstant
                         ]) $
    node n ()

instance Node ModifierSpeciesRef where
  node n ModifierSpeciesRef{modifierSpeciesRefID, modifierSpeciesRefSpecies} =
    add_attrs (catMaybes [ mbAttr   "id" ppText modifierSpeciesRefID
                         , justAttr "species" ppText modifierSpeciesRefSpecies
                         ]) $
    node n ()

instance Node KineticLaw where
  node n KineticLaw{kineticMath, kineticLocalParams} =
    node n $ catMaybes
      [ mbUnode "math" kineticMath
      , listOf "LocalParameters" (unode "localParameter") kineticLocalParams
      ]

instance Node LocalParam where
  node n LocalParam{localParamID, localParamValue, localParamUnits} =
    add_attrs (catMaybes [ justAttr "id" ppText localParamID
                         , mbAttr   "value" ppDouble localParamValue
                         , mbAttr   "units" ppText localParamUnits
                         ]) $
    node n ()

listOf :: String -> (a -> Element) -> Maybe [a] -> Maybe Element
listOf listName toElement mbL =
  fmap (unode ("listOf" ++ listName) . map toElement) mbL

attr :: String -> String -> Attr
attr key val = Attr{attrKey = unqual key, attrVal = val}

justAttr :: String -> (a -> String) -> a -> Maybe Attr
justAttr key ppVal val = Just Attr{attrKey = unqual key, attrVal = ppVal val}

mbAttr :: String -> (a -> String) -> Maybe a -> Maybe Attr
mbAttr key ppVal mbVal =
  (\val -> Attr{attrKey = unqual key, attrVal = ppVal val}) <$> mbVal

mbUnode :: Node n => String -> Maybe n -> Maybe Element
mbUnode name mbN = unode name <$> mbN

ppBool :: Bool -> String
ppBool = showButLowercase

ppDouble :: Double -> String
ppDouble = show

ppInt :: Int -> String
ppInt = show

ppText :: Text -> String
ppText = T.unpack

ppUnitKind :: UnitKind -> String
ppUnitKind = showButLowercase

showButLowercase :: Show a => a -> String
showButLowercase x =
  case show x of
    ""     -> ""
    (c:cs) -> toLower c:cs
