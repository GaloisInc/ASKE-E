{-# LANGUAGE OverloadedStrings #-}

module SBML where

import Data.Text ( Text )

import Language.ASKEE.SBML.Parse
import Language.ASKEE.SBML.Syntax

import Test.Tasty
import Test.Tasty.HUnit

import Text.XML.Light

expectLeft :: Show a => Either String a -> IO ()
expectLeft x =
  case x of
    Right _ -> assertFailure ("Expected `Left`, received "<>show x)
    Left _ -> pure ()

expectRight :: Show a => Either String a -> IO ()
expectRight x =
  case x of
    Right _ -> pure ()
    Left _ -> assertFailure ("Expected `Right`, received "<>show x)


parseSBMLWrongName = 
  expectLeft (parse src parseSBML)
  where
    src = "<smbl level=\"3\" version=\"2\"></sbml>"

parseSBMLWrongLevel = 
  expectLeft (parse src parseSBML)
  where
    src = "<sbml level=\"2\" version=\"2\"></sbml>"

parseSBMLWrongVersion = 
  expectLeft (parse src parseSBML)
  where
    src = "<sbml level=\"3\" version=\"1\"></sbml>"

parseSBMLNoModel = 
  expected @=? parse src parseSBML
  where
    src = "<sbml level=\"3\" version=\"2\"></sbml>"
    expected = Right SBML { sbmlLevel = 3, sbmlVersion = 2, sbmlModel = Nothing }

parseModelWrongName = 
  expectLeft (parse src parseModel)
  where
    src = "<modle></model>"

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

parseModelNoName = 
  expected @=? parse src parseModel
  where
    src = "<model></model>"
    expected = Right emptyModel

-------------------------------------------------------------------------------

parseSampleText = expected @=? runParser (parseText "hey")
  where
    expected = Right "hey"

parseTrue = expected @=? runParser (parseBool "true")
  where
    expected = Right True

parseFalse = expected @=? runParser (parseBool "false")
  where
    expected = Right False

parseInt = expected @=? runParser (parseAny "3")
  where
    expected = Right (3 :: Int)

parseDouble = expected @=? runParser (parseAny "3")
  where
    expected = Right (3.0 :: Double)

parseReqTextAttr :: QName -> Element -> Parser Text
parseReqTextAttr s e = reqAttr parseText e s

parseOptTextAttr :: QName -> Element -> Parser (Maybe Text)
parseOptTextAttr s e = optAttr parseText e s

parseReqAttrPresent = 
  expected @=? parse src (parseReqTextAttr "bar")
  where
    src = "<foo bar=\"hey\">"
    expected = Right "hey"

parseReqAttrAbsent = 
  expectLeft $ parse src (parseReqTextAttr "foo")
  where
    src = "<foo bar=\"hey\">"

parseOptAttrPresent = 
  expected @=? parse src (parseOptTextAttr "bar")
  where
    src = "<foo bar=\"hey\">"
    expected = Right (Just "hey")

parseOptAttrAbsent = 
  expected @=? parse src (parseOptTextAttr "foo")
  where
    src = "<foo bar=\"hey\">"
    expected = Right Nothing

parseReqChildPresent = 
  expected @=? 
    parse src (\e -> reqChild (parseReqTextAttr "bar") e "foo")
  where
    src = "<any><foo bar=\"baz\"></foo></any>"
    expected = Right "baz"

parseReqChildAbsent = 
  expectLeft $ 
    parse src (\e -> reqChild (parseReqTextAttr "bar") e "bar")
  where
    src = "<any><foo bar=\"baz\"></foo></any>"

parseOptChildPresent = 
  expected @=? 
    parse src (\e -> optChild (parseReqTextAttr "bar") e "foo")
  where
    src = "<any><foo bar=\"baz\"></foo></any>"
    expected = Right (Just "baz")

parseOptChildAbsent = 
  expected @=? 
    parse src (\e -> optChild (\_ -> die "" :: Parser ()) e "bar")
  where
    src = "<any><foo bar=\"baz\"></foo></any>"
    expected = Right Nothing

parseAllChildren =
  expected @=?
    parse src (appChildren (parseOptTextAttr "bar"))
  where
    src = "<any><foo bar=\"baz\"></foo><foo></foo></any>"
    expected = Right [Just "baz", Nothing]

parseRightName = 
  expected @=? parse src (guardName "foo")
  where
    src = "<foo bar=\"hey\">"
    expected = Right ()

parseWrongName = 
  expectLeft $ parse src (guardName "bar")
  where
    src = "<foo bar=\"hey\">"

tests = testGroup "SBML parsing tests"
  [ testCase "parseSBMLWrongName" parseSBMLWrongName
  , testCase "parseSBMLWrongLevel" parseSBMLWrongLevel
  , testCase "parseSBMLWrongVersion" parseSBMLWrongVersion
  , testCase "parseSBMLNoModel" parseSBMLNoModel
  , testCase "parseModelWrongName" parseModelWrongName
  
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