{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.ASKEE.SBML.L2V3.ToXML
  ( sbmlToXML
  ) where

import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (Ordering(..))
import Text.XML.Light

import Language.ASKEE.Expr (Expr(..))
import Language.ASKEE.SBML.L2V3.Syntax
import Language.ASKEE.SBML.Common.Syntax

sbmlToXML :: SBML -> Text
sbmlToXML sbml =
  T.unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , T.pack $ ppElement $ unode "sbml" sbml
            ]

instance Node SBML where
  node n SBML{ sbmlLevel, sbmlVersion, sbmlModel
             , sbmlNotes, sbmlAnnotation
             } =
    add_attrs [ attr "xmlns" "http://www.sbml.org/sbml/level3/version2/core"
              , attr "level" $ ppInt sbmlLevel
              , attr "version" $ ppInt sbmlVersion
              ] $
    node n $ catMaybes
      [ Just $ unode "model" sbmlModel
      , mbUnode "notes" sbmlNotes
      , mbUnode "annotation" sbmlAnnotation
      ]

instance Node Model where
  node n Model{ modelID, modelName
              , modelFunctionDefs, modelUnitDefs
              , modelCompartments, modelSpecies
              , modelParameters, modelInitialAssignments
              , modelRules, modelConstraints
              , modelReactions, modelEvents
              } =
    add_attrs (catMaybes [ mbAttr "id" ppText modelID
                         , mbAttr "name" ppText modelName
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
  node n Function{ functionID, functionName
                 , functionArgs, functionBody
                 } =
    add_attrs (catMaybes [ justAttr "id" ppText functionID
                         , mbAttr   "name" ppText functionName
                         ]) $
    node n
      [ wrapMathElements (unqual "math") $
        [ unode "lambda" $
          map argToXML functionArgs ++
          [ mathToXML functionBody
          ]
        ]
      ]
    where
      argToXML :: ID -> Element
      argToXML functionArg =
        unode "bvar"
          [ mathToXML $ Var functionArg
          ]

instance Node UnitDef where
  node n UnitDef{unitDefID, unitDefUnits} =
    add_attr (attr "id" $ ppText unitDefID) $
    node n $ catMaybes
      [ listOf "Units" (unode "unit") unitDefUnits
      ]

instance Node Compartment where
  node n Compartment{ compartmentID, compartmentName, compartmentDimensions
                    , compartmentSize, compartmentUnits
                    , compartmentConstant, compartmentAnnotation
                    } =
    add_attrs (catMaybes [ justAttr "id" ppText compartmentID
                         , mbAttr   "name" ppText compartmentName
                         , justAttr "spatialDimensions" ppInt compartmentDimensions
                         , mbAttr   "size" ppDouble compartmentSize
                         , mbAttr   "units" ppText compartmentUnits
                         , justAttr "constant" ppBool compartmentConstant
                         ]) $
    node n $ catMaybes
      [ mbUnode "annotation" compartmentAnnotation
      ]

instance Node Species where
  node n Species{ speciesID, speciesName
                , speciesCompartment, speciesInitialAmount
                , speciesInitialConc, speciesSubstanceUnits
                , speciesHasOnlySubstanceUnits, speciesBoundaryCondition
                , speciesConstant
                , speciesNotes, speciesAnnotation
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
                         ]) $
    node n $ catMaybes
      [ mbUnode "notes" speciesNotes
      , mbUnode "annotatin" speciesAnnotation
      ]

instance Node Parameter where
  node n Parameter{ parameterID, parameterName
                  , parameterValue, parameterUnits
                  , parameterConstant, parameterNotes
                  } =
    add_attrs (catMaybes [ justAttr "id" ppText parameterID
                         , mbAttr   "name" ppText parameterName
                         , mbAttr   "value" ppDouble parameterValue
                         , mbAttr   "units" ppText parameterUnits
                         , justAttr "constant" ppBool parameterConstant
                         ]) $
    node n $ catMaybes
      [ mbUnode "notes" parameterNotes
      ]

instance Node InitialAssignment where
  node n InitialAssignment{initialSymbol, initialMath} =
    add_attr (attr "symbol" $ ppText initialSymbol) $
    node n
      [ unode "math" initialMath
      ]

instance Node Rule where
  node _n rule =
    case rule of
      AlgebraicRule{ruleMath} ->
        unode "algebraicRule"
          [ unode "math" ruleMath
          ]
      AssignmentRule{ruleMath, ruleVariable} ->
        add_attr (attr "variable" $ ppText ruleVariable) $
        unode "assignmentRule"
          [ unode "math" ruleMath
          ]
      RateRule{ruleMath, ruleVariable} ->
        add_attr (attr "variable" $ ppText ruleVariable) $
        unode "rateRule"
          [ unode "math" ruleMath
          ]

instance Node Constraint where
  node _n x = case x of {}

instance Node Reaction where
  node n Reaction{ reactionID, reactionName
                 , reactionReversible, reactionReactants
                 , reactionProducts, reactionModifiers
                 , reactionKineticLaw, reactionAnnotation
                 } =
    add_attrs (catMaybes [ justAttr "id" ppText reactionID
                         , mbAttr "name" ppText reactionName
                         , justAttr "reversible" ppBool reactionReversible
                         ]) $
    node n $ catMaybes
      [ listOf "Reactants" (unode "speciesReference")         reactionReactants
      , listOf "Products"  (unode "speciesReference")         reactionProducts
      , listOf "Modifiers" (unode "modifierSpeciesReference") reactionModifiers
      , mbUnode "kineticLaw" reactionKineticLaw
      , mbUnode "annotation" reactionAnnotation
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
  node n math = wrapMathElements n [mathToXML math]

wrapMathElements :: QName -> [Element] -> Element
wrapMathElements n es =
  add_attrs [ attr "xmlns" "http://www.w3.org/1998/Math/MathML"
            ] $
  node n es

mathToXML :: Math -> Element
mathToXML = exprToElement
  where
    exprToElement :: Expr -> Element
    exprToElement e =
      case e of
        Neg e1    -> apply1 "minus" e1
        Sin e1    -> apply1 "sin" e1
        Not e1    -> apply1 "not" e1
        Exp e1    -> apply1 "exp" e1
        Log e1    -> apply1 "ln" e1

        Add e1 e2 -> apply2 "plus" e1 e2
        Sub e1 e2 -> apply2 "minus" e1 e2
        Mul e1 e2 -> apply2 "times" e1 e2
        Div e1 e2 -> apply2 "divide" e1 e2
        Pow e1 e2 -> apply2 "power" e1 e2
        And e1 e2 -> apply2 "and" e1 e2
        Or e1 e2  -> apply2 "or" e1 e2
        LT e1 e2  -> apply2 "lt" e1 e2
        LTE e1 e2 -> apply2 "lte" e1 e2
        EQ e1 e2  -> apply2 "eq" e1 e2
        GTE e1 e2 -> apply2 "gte" e1 e2
        GT e1 e2  -> apply2 "gt" e1 e2

        If c t f                       -> piecewiseToXML [(t, c)] (Just f)
        Cond initPieces otherwisePiece -> piecewiseToXML initPieces otherwisePiece

        Var  i    -> unode "ci" $ ppText i
        LitD d    -> unode "cn" $ ppDouble d
        LitB b    -> unode (ppBool b) ()

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

    piecewiseToXML :: [(Expr, Expr)] -> Maybe Expr -> Element
    piecewiseToXML initPieces otherwisePiece =
      unode "piecewise" $ catMaybes $
        map (Just . pieceToXML) initPieces ++
        [otherwisePieceToXML <$> otherwisePiece]

    pieceToXML :: (Expr, Expr) -> Element
    pieceToXML (action, condition) =
      unode "piece"
        [ exprToElement action
        , exprToElement condition
        ]

    otherwisePieceToXML :: Expr -> Element
    otherwisePieceToXML action =
      unode "otherwise"
        [ exprToElement action
        ]

instance Node SpeciesRef where
  node n SpeciesRef{ speciesRefID, speciesRefName, speciesRefSpecies
                   , speciesRefStoichiometry
                   } =
    add_attrs (catMaybes [ mbAttr   "id" ppText speciesRefID
                         , mbAttr   "name" ppText speciesRefName
                         , justAttr "species" ppText speciesRefSpecies
                         , justAttr "stoichiometry" ppDouble speciesRefStoichiometry
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
      , listOf "Parameters" (unode "parameter") kineticLocalParams
      ]

instance Node Notes where
  node n Notes{getNotes = ns} =
    node n ns

instance Node Annotation where
  node n Annotation{getAnnotation = ns} =
    node n ns

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
