{-# LANGUAGE OverloadedStrings #-}

module SBML where

import Data.Text ( Text )

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

emptyModel = Model
  { modelName               = Nothing
  , modelFunctionDefs       = Nothing
  , modelUnitDefs           = Nothing
  , modelCompartments       = Nothing
  , modelSpecies            = Nothing
  , modelParameters         = Nothing
  , modelInitialAssignments = Nothing
  , modelRules              = Nothing
  , modelConstraints        = Nothing
  , modelReactions          = Nothing
  , modelEvents             = Nothing
  }

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
    expected = Right u
    u = defaultUnit
      { unitKind = Second
      , unitExponent = -1
      , unitScale = 0
      }
      
parseFullUnitDef :: Assertion
parseFullUnitDef = expected @=? parse src parseUnitDef
  where
    src = unwords
      [ "<unitDefinition id=\"litre_per_mole_second\">"
      , "  <listOfUnits>"
      , "     <unit kind=\"mole\"   exponent=\"-1\" scale=\"0\" multiplier=\"1\"/>"
      , "     <unit kind=\"litre\"  exponent=\"1\"  scale=\"0\" multiplier=\"1\"/>"
      , "     <unit kind=\"second\" exponent=\"-1\" scale=\"0\" multiplier=\"1\"/>"
      , "  </listOfUnits>"
      , "</unitDefinition>"
      ]
    expected = Right perSecond
    perSecond = UnitDef
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
parseInt = expected @=? runParser (parseAny "3")
  where
    expected = Right (3 :: Int)

parseDouble :: Assertion
parseDouble = expected @=? runParser (parseAny "3")
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

tests :: TestTree
tests = testGroup "SBML parsing tests"
  [ testCase "parseSBMLWrongLevel" parseSBMLWrongLevel
  , testCase "parseSBMLWrongVersion" parseSBMLWrongVersion
  
  , testCase "parseUnitKindSuccess" parseUnitKindSuccess
  , testCase "parseUnitKindFailure" parseUnitKindFailure
  , testCase "parseFullUnit" parseFullUnit
  , testCase "parseFullUnitDef" parseFullUnitDef

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