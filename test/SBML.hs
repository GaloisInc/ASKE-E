{-# LANGUAGE OverloadedStrings #-}

module SBML where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text ( Text )

import Language.ASKEE.Expr
import qualified Language.ASKEE.SBML as SBML
import Language.ASKEE.SBML.Parse
import Language.ASKEE.SBML.Syntax

import Test.Tasty
import Test.Tasty.HUnit

import Text.XML.Light

expectLeft :: Show r => Either l r -> IO ()
expectLeft x =
  case x of
    Right r -> assertFailure ("Expected `Left`, received Right "<>show r)
    Left _ -> pure ()

expectRight :: Show l => Either l r -> IO ()
expectRight x =
  case x of
    Right _ -> pure ()
    Left l -> assertFailure ("Expected `Right`, received Left "<>show l)

parseSBMLWrongLevel :: Assertion
parseSBMLWrongLevel =
  expectLeft (parse src parseSBML)
  where
    src = "<sbml level=\"2\" version=\"2\"></sbml>"

parseSBMLWrongVersion :: Assertion
parseSBMLWrongVersion =
  expectLeft (parse src parseSBML)
  where
    src = "<sbml level=\"3\" version=\"1\"></sbml>"

-------------------------------------------------------------------------------

parseUnitKindSuccess :: Assertion
parseUnitKindSuccess = expected @=? runParser (parseUnitKind "tesla")
  where
    expected = Right Tesla

parseUnitKindFailure :: Assertion
parseUnitKindFailure = expectLeft $ runParser (parseUnitKind "general motors")

defaultUnit :: Unit
defaultUnit = Unit
  { unitKind = undefined
  , unitExponent = 0
  , unitScale = 1
  , unitMultiplier = 1
  }

parseFullUnit :: Assertion
parseFullUnit = expected @=? parse src parseUnit
  where
    src = "<unit kind=\"second\" exponent=\"-1\" scale=\"0\" multiplier=\"1\"/>"
    expected = Right defaultUnit
      { unitKind = Second
      , unitExponent = -1
      , unitScale = 0
      }

parseFullUnitDef :: Assertion
parseFullUnitDef = expected @=? parse src parseUnitDef
  where
    src = unlines
      [ "<unitDefinition id=\"litre_per_mole_second\">"
      , "  <listOfUnits>"
      , "     <unit kind=\"mole\"   exponent=\"-1\" scale=\"0\" multiplier=\"1\"/>"
      , "     <unit kind=\"litre\"  exponent=\"1\"  scale=\"0\" multiplier=\"1\"/>"
      , "     <unit kind=\"second\" exponent=\"-1\" scale=\"0\" multiplier=\"1\"/>"
      , "  </listOfUnits>"
      , "</unitDefinition>"
      ]
    expected = Right UnitDef
      { unitDefID = "litre_per_mole_second"
      , unitDefUnits = Just [u1,u2,u3]
      }
    u1 = defaultUnit
      { unitKind = Mole
      , unitExponent = -1
      , unitScale = 0
      }
    u2 = defaultUnit
      { unitKind = Litre
      , unitExponent = 1
      , unitScale = 0
      }
    u3 = defaultUnit
      { unitKind = Second
      , unitExponent = -1
      , unitScale = 0
      }

-------------------------------------------------------------------------------

parseFullCompartment :: Assertion
parseFullCompartment = expected @=? parse src parseCompartment
  where
    src = "<compartment id=\"comp\" size=\"1e-14\" spatialDimensions=\"3\" units=\"litre\" constant=\"true\"/>"
    expected = Right Compartment
      { compartmentID = "comp"
      , compartmentSize = Just 1e-14
      , compartmentDimensions = Just 3
      , compartmentUnits = Just "litre"
      , compartmentConstant = True
      }

-------------------------------------------------------------------------------

parseFullSpecies :: Assertion
parseFullSpecies = expected @=? parse src parseSpecies
  where
    src = "<species compartment=\"comp\" id=\"E\"  initialAmount=\"5e-21\" boundaryCondition=\"false\" hasOnlySubstanceUnits=\"false\" substanceUnits=\"mole\" constant=\"false\"/>"
    expected = Right Species
      { speciesID = "E"
      , speciesName = Nothing
      , speciesCompartment = "comp"
      , speciesInitialAmount = Just 5e-21
      , speciesInitialConc = Nothing
      , speciesSubstanceUnits = Just "mole"
      , speciesHasOnlySubstanceUnits = False
      , speciesBoundaryCondition = False
      , speciesConstant = False
      , speciesConversionFactor = Nothing
      }

-------------------------------------------------------------------------------

parseFullParameter :: Assertion
parseFullParameter = expected @=? parse src parseParameter
  where
    src = "<parameter id=\"convertToMilliMole\" value=\"1000\" units=\"millimole_per_mole\" constant=\"true\"/>"
    expected = Right Parameter
      { parameterID = "convertToMilliMole"
      , parameterName = Nothing
      , parameterValue = Just 1000
      , parameterUnits = Just "millimole_per_mole"
      , parameterConstant = True
      }

-------------------------------------------------------------------------------

parseMathLit :: Assertion
parseMathLit = expected @=? parse src parseMath
  where
    src = unlines
      [ "<math xmlns=\"http://www.w3.org/1998/Math/MathML\""
      , "      xmlns:sbml=\"http://www.sbml.org/sbml/level3/version2/core\">"
      , "  <cn sbml:units=\"dimensionless\"> 2 </cn>"
      , "</math>"
      ]
    expected = Right (LitD 2)

parseMathVar :: Assertion
parseMathVar = expected @=? parse src parseMath
  where
    src = unlines
      [ "<math xmlns=\"http://www.w3.org/1998/Math/MathML\""
      , "      xmlns:sbml=\"http://www.sbml.org/sbml/level3/version2/core\">"
      , "  <ci> y </ci>"
      , "</math>"
      ]
    expected = Right (Var "y")

parseMathApp :: Assertion
parseMathApp = expected @=? parse src parseMath
  where
    src = unlines
      [ "<math xmlns=\"http://www.w3.org/1998/Math/MathML\""
      , "      xmlns:sbml=\"http://www.sbml.org/sbml/level3/version2/core\">"
      , "    <apply>"
      , "        <times/>"
      , "        <ci> y </ci>"
      , "        <cn sbml:units=\"dimensionless\"> 2 </cn>"
      , "    </apply>"
      , "</math>"
      ]
    expected = Right (Mul (Var "y") (LitD 2))

parseMathSingleton :: Assertion
parseMathSingleton = expected @=? parse src parseMath
  where
    src = unlines
      [ "<math xmlns=\"http://www.w3.org/1998/Math/MathML\""
      , "      xmlns:sbml=\"http://www.sbml.org/sbml/level3/version2/core\">"
      , "    <apply>"
      , "        <minus/>"
      , "        <ci> y </ci>"
      , "    </apply>"
      , "</math>"
      ]
    expected = Right (Neg (Var "y"))

parseMathIdentity :: Assertion
parseMathIdentity = expected @=? parse src parseMath
  where
    src = unlines
      [ "<math xmlns=\"http://www.w3.org/1998/Math/MathML\""
      , "      xmlns:sbml=\"http://www.sbml.org/sbml/level3/version2/core\">"
      , "    <apply>"
      , "        <plus/>"
      , "    </apply>"
      , "</math>"
      ]
    expected = Right (LitD 0)

-------------------------------------------------------------------------------

parseFullInitialAssignment :: Assertion
parseFullInitialAssignment = expected @=? parse src parseInitialAssignment
  where
    src = unlines
      [ "<initialAssignment symbol=\"x\">"
      , "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\""
      , "          xmlns:sbml=\"http://www.sbml.org/sbml/level3/version2/core\">"
      , "        <apply>"
      , "            <times/>"
      , "            <ci> y </ci>"
      , "            <cn sbml:units=\"dimensionless\"> 2 </cn>"
      , "        </apply>"
      , "    </math>"
      , "</initialAssignment>"
      ]
    expected = Right InitialAssignment
      { initialID = Nothing
      , initialSymbol = "x"
      , initialMath = Mul (Var "y") (LitD 2)
      }

-------------------------------------------------------------------------------

parseFullSpeciesRef :: Assertion
parseFullSpeciesRef = expected @=? parse src parseSpeciesRef
  where
    src = "<speciesReference species=\"T\" stoichiometry=\"1\" constant=\"true\"/>"
    expected = Right SpeciesRef
      { speciesRefID = Nothing
      , speciesRefName = Nothing
      , speciesRefSpecies = "T"
      , speciesRefStoichiometry = Just 1
      , speciesRefConstant = True
      }

parseFullModifierSpeciesRef :: Assertion
parseFullModifierSpeciesRef = expected @=? parse src parseModifierSpeciesRef
  where
    src = "<modifierSpeciesReference species=\"S2\"/>"
    expected = Right ModifierSpeciesRef
      { modifierSpeciesRefID = Nothing
      , modifierSpeciesRefSpecies = "S2"
      }

parseFullLocalParameter :: Assertion
parseFullLocalParameter = expected @=? parse src parseLocalParam
  where
    src = "<localParameter id=\"k2\" value=\"0.15\" units=\"per_second\"/>"
    expected = Right LocalParam
      { localParamID = "k2"
      , localParamValue = Just 0.15
      , localParamUnits = Just "per_second"
      }

parseFullKineticLaw :: Assertion
parseFullKineticLaw = expected @=? parse src parseKineticLaw
  where
    src = unlines
      [ "<kineticLaw>"
      , "    <math xmlns=\"http://www.w3.org/1998/Math/MathML\">"
      , "        <apply>"
      , "            <times/>"
      , "            <ci> k2 </ci>"
      , "            <ci> S2 </ci>"
      , "            <ci> cell </ci>"
      , "        </apply>"
      , "    </math>"
      , "    <listOfLocalParameters>"
      , "        <localParameter id=\"k2\" value=\"0.15\" units=\"per_second\"/>"
      , "    </listOfLocalParameters>"
      , "</kineticLaw>"
      ]
    l = LocalParam
      { localParamID = "k2"
      , localParamValue = Just 0.15
      , localParamUnits = Just "per_second"
      }
    expected = Right KineticLaw
      { kineticMath = Just (Mul (Mul (Var "k2") (Var "S2")) (Var "cell"))
      , kineticLocalParams = Just [l]
      }


parseFullReaction :: Assertion
parseFullReaction = expected @=? parse src parseReaction
  where
    src = unlines
      [ "<reaction id=\"out\" reversible=\"false\">"
      , "  <listOfReactants>"
      , "      <speciesReference species=\"T\" stoichiometry=\"1\" constant=\"true\"/>"
      , "  </listOfReactants>"
      , "  <listOfProducts>"
      , "      <speciesReference species=\"X1\" stoichiometry=\"1\" constant=\"true\"/>"
      , "  </listOfProducts>"
      , "  <listOfModifiers>"
      , "      <modifierSpeciesReference species=\"S2\"/>"
      , "  </listOfModifiers>"
      , "  <kineticLaw>"
      , "      <math xmlns=\"http://www.w3.org/1998/Math/MathML\">"
      , "          <apply>"
      , "              <times/>"
      , "              <ci> k2 </ci>"
      , "              <ci> S2 </ci>"
      , "              <ci> cell </ci>"
      , "          </apply>"
      , "      </math>"
      , "      <listOfLocalParameters>"
      , "          <localParameter id=\"k2\" value=\"0.15\" units=\"per_second\"/>"
      , "      </listOfLocalParameters>"
      , "  </kineticLaw>"
      , "</reaction>"
      ]

    expected = Right Reaction
      { reactionID = "out"
      , reactionReversible = False
      , reactionCompartment = Nothing
      , reactionReactants = Just [r]
      , reactionProducts = Just [p]
      , reactionModifiers = Just [m]
      , reactionKineticLaw = Just k
      }
    r = SpeciesRef
      { speciesRefID = Nothing
      , speciesRefName = Nothing
      , speciesRefSpecies = "T"
      , speciesRefStoichiometry = Just 1
      , speciesRefConstant = True
      }
    p = SpeciesRef
      { speciesRefID = Nothing
      , speciesRefName = Nothing
      , speciesRefSpecies = "X1"
      , speciesRefStoichiometry = Just 1
      , speciesRefConstant = True
      }
    m = ModifierSpeciesRef
      { modifierSpeciesRefID = Nothing
      , modifierSpeciesRefSpecies = "S2"
      }
    l = LocalParam
      { localParamID = "k2"
      , localParamValue = Just 0.15
      , localParamUnits = Just "per_second"
      }
    k = KineticLaw
      { kineticMath = Just (Mul (Mul (Var "k2") (Var "S2")) (Var "cell"))
      , kineticLocalParams = Just [l]
      }

-------------------------------------------------------------------------------


parseSampleText :: Assertion
parseSampleText = expected @=? runParser (parseText "hey")
  where
    expected = Right "hey"

parseTrue :: Assertion
parseTrue = expected @=? runParser (parseBool "true")
  where
    expected = Right True

parseFalse :: Assertion
parseFalse = expected @=? runParser (parseBool "false")
  where
    expected = Right False

parseInt :: Assertion
parseInt = expected @=? runParser (parseRead "3")
  where
    expected = Right (3 :: Int)

parseDouble :: Assertion
parseDouble = expected @=? runParser (parseRead "3")
  where
    expected = Right (3.0 :: Double)

parseReqTextAttr :: QName -> Element -> Parser Text
parseReqTextAttr s e = reqAttr parseText e s

parseOptTextAttr :: QName -> Element -> Parser (Maybe Text)
parseOptTextAttr s e = optAttr parseText e s

parseReqAttrPresent :: Assertion
parseReqAttrPresent =
  expected @=? parse src (parseReqTextAttr "bar")
  where
    src = "<foo bar=\"hey\">"
    expected = Right "hey"

parseReqAttrAbsent :: Assertion
parseReqAttrAbsent =
  expectLeft $ parse src (parseReqTextAttr "foo")
  where
    src = "<foo bar=\"hey\">"

parseOptAttrPresent :: Assertion
parseOptAttrPresent =
  expected @=? parse src (parseOptTextAttr "bar")
  where
    src = "<foo bar=\"hey\">"
    expected = Right (Just "hey")

parseOptAttrAbsent :: Assertion
parseOptAttrAbsent =
  expected @=? parse src (parseOptTextAttr "foo")
  where
    src = "<foo bar=\"hey\">"
    expected = Right Nothing

parseReqChildPresent :: Assertion
parseReqChildPresent =
  expected @=?
    parse src (\e -> reqChild (parseReqTextAttr "bar") e "foo")
  where
    src = "<any><foo bar=\"baz\"></foo></any>"
    expected = Right "baz"

parseReqChildAbsent :: Assertion
parseReqChildAbsent =
  expectLeft $
    parse src (\e -> reqChild (parseReqTextAttr "bar") e "bar")
  where
    src = "<any><foo bar=\"baz\"></foo></any>"

parseOptChildPresent :: Assertion
parseOptChildPresent =
  expected @=?
    parse src (\e -> optChild (parseReqTextAttr "bar") e "foo")
  where
    src = "<any><foo bar=\"baz\"></foo></any>"
    expected = Right (Just "baz")

parseOptChildAbsent :: Assertion
parseOptChildAbsent =
  expected @=?
    parse src (\e -> optChild (\_ -> die "" :: Parser ()) e "bar")
  where
    src = "<any><foo bar=\"baz\"></foo></any>"
    expected = Right Nothing

parseAllChildren :: Assertion
parseAllChildren =
  expected @=?
    parse src (appChildren (parseOptTextAttr "bar"))
  where
    src = "<any><foo bar=\"baz\"></foo><foo></foo></any>"
    expected = Right [Just "baz", Nothing]

parseRightName :: Assertion
parseRightName =
  expected @=? parse src (guardName "foo")
  where
    src = "<foo bar=\"hey\">"
    expected = Right ()

parseWrongName :: Assertion
parseWrongName =
  expectLeft $ parse src (guardName "bar")
  where
    src = "<foo bar=\"hey\">"

-------------------------------------------------------------------------------

indraModelRoundtrip :: Assertion
indraModelRoundtrip = do
  fileContents <- T.readFile "modelRepo/sbml/indra_model.sbml"
  case SBML.parseSBML (T.unpack fileContents) of
    Left err   -> assertFailure err
    Right sbml -> do
      let ppSBML = SBML.sbmlToXML sbml
      case SBML.parseSBML (T.unpack ppSBML) of
        Left err    -> assertFailure err
        Right sbml' -> sbml @=? sbml'

-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "SBML tests"
  [ testGroup "SBML parsing tests"
    [ testCase "parseSBMLWrongLevel" parseSBMLWrongLevel
    , testCase "parseSBMLWrongVersion" parseSBMLWrongVersion

    , testCase "parseUnitKindSuccess" parseUnitKindSuccess
    , testCase "parseUnitKindFailure" parseUnitKindFailure
    , testCase "parseFullUnit" parseFullUnit
    , testCase "parseFullUnitDef" parseFullUnitDef

    , testCase "parseFullCompartment" parseFullCompartment

    , testCase "parseFullSpecies" parseFullSpecies

    , testCase "parseFullParameter" parseFullParameter

    , testCase "parseMathLit" parseMathLit
    , testCase "parseMathVar" parseMathVar
    , testCase "parseMathApp" parseMathApp
    , testCase "parseMathSingleton" parseMathSingleton
    , testCase "parseMathIdentity" parseMathIdentity

    , testCase "parseFullInitialAssignment" parseFullInitialAssignment

    , testCase "parseFullSpeciesRef" parseFullSpeciesRef
    , testCase "parseFullModifierSpeciesRef" parseFullModifierSpeciesRef
    , testCase "parseFullLocalParameter" parseFullLocalParameter
    , testCase "parseFullKineticLaw" parseFullKineticLaw
    , testCase "parseFullReaction" parseFullReaction

    , testCase "parseSampleText" parseSampleText
    , testCase "parseTrue" parseTrue
    , testCase "parseFalse" parseFalse
    , testCase "parseInt" parseInt
    , testCase "parseDouble" parseDouble

    , testCase "parseReqAttrPresent" parseReqAttrPresent
    , testCase "parseReqAttrAbsent" parseReqAttrAbsent
    , testCase "parseOptAttrPresent" parseOptAttrPresent
    , testCase "parseOptAttrAbsent" parseOptAttrAbsent
    , testCase "parseReqChildAbsent" parseReqChildAbsent
    , testCase "parseReqChildAbsent" parseReqChildAbsent
    , testCase "parseOptChildAbsent" parseOptChildAbsent
    , testCase "parseOptChildAbsent" parseOptChildAbsent
    , testCase "parseAllChildren" parseAllChildren

    , testCase "parseRightName" parseRightName
    , testCase "parseWrongName" parseWrongName
    ]
  , testGroup "SBML real-world unit tests"
    [ testCase "indra_model roundtrip" indraModelRoundtrip
    ]
  ]
