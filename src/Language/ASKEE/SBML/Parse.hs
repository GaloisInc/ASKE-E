{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.ASKEE.SBML.Parse where

import Control.Monad            ( unless )
import Control.Monad.State.Lazy ( evalState
                                , get
                                , MonadState(..)
                                , State )
import Control.Monad.Except     ( throwError
                                , runExceptT
                                , MonadError(..)
                                , ExceptT(..) )

import Data.Char   ( toLower, isSpace )
import Data.String ( IsString(..) )
import Data.Text   ( Text, pack )

import Language.ASKEE.Expr    ( Expr(..) )
import Language.ASKEE.SBML.Syntax

import Text.Printf            ( formatString
                              , printf
                              , PrintfArg(formatArg) )
import Text.Read              ( readEither )
import Text.XML.Light         ( elChildren
                              , filterChild
                              , lookupAttrBy
                              , parseXML
                              , Attr
                              , CData(..)
                              , Content(..)
                              , Element(..)
                              , Line
                              , QName(..) )

parse :: String -> (Element -> Parser a) -> Either String a
parse src parser =
  case runParser (parseString src parser) of
    Left err -> Left (ppError err)
    Right a -> Right a

parseString :: String -> (Element -> Parser a) -> Parser a
parseString src parser = xml >>= asElement >>= parser
  where
    xml = stripHeader (concatMap removeWS (parseXML src))
    stripHeader x =
      do  setLinum (Just 0)
          case length x of
            0 -> die "this document was too empty"
            1 -> pure $ head x
            2 -> pure (x !! 1)
            _ -> die "this document was too full"

-------------------------------------------------------------------------------

newtype Parser a = Parser
  { unParser :: ExceptT Error (State Location) a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadError Error
    , MonadState Location
    )

data Error = Error
  { eLoc :: Location
  , eMessage :: String
  }
  deriving (Eq, Show)

ppError :: Error -> String
ppError Error{..} = printf "error at line %s: %s" ppLoc eMessage
  where
    ppLoc =
      case eLoc of
        Location Nothing -> "<no location>"
        Location (Just l) -> show l

newtype Location = Location
  { locLinum :: Maybe Line
  }
  deriving (Eq, Show)

runParser :: Parser a -> Either Error a
runParser (Parser i) = evalState (runExceptT i) (Location Nothing)

die :: String -> Parser a
die eMessage =
  do  eLoc <- get
      throwError Error{..}

setLinum :: Maybe Line -> Parser ()
setLinum = put . Location

-------------------------------------------------------------------------------

parseSBML :: Element -> Parser SBML
parseSBML e =
  do  guardName "sbml" e
      sbmlLevel <- reqAttr parseAny e "level"
      sbmlVersion <- reqAttr parseAny e "version"
      unless (supportedSBMLVersion sbmlLevel sbmlVersion) $
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
      unitExponent <- reqAttr parseAny e "exponent"
      unitScale <- reqAttr parseAny e "scale"
      unitMultiplier <- reqAttr parseAny e "multiplier"
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
      compartmentDimensions <- optAttr parseAny  e "spatialDimensions"
      compartmentSize       <- optAttr parseAny  e "size"
      compartmentUnits      <- optAttr parseText e "units"
      compartmentConstant   <- reqAttr parseBool e "constant"
      pure Compartment{..}

parseSpecies :: Element -> Parser Species
parseSpecies e =
  do  guardName "species" e
      speciesID                    <- reqAttr parseText e "id"
      speciesName                  <- optAttr parseText e "name"
      speciesCompartment           <- reqAttr parseText e "compartment"
      speciesInitialAmount         <- optAttr parseAny  e "initialAmount"
      speciesInitialConc           <- optAttr parseAny  e "initialConcentration"
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
      parameterValue    <- optAttr parseAny  e "value"
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

parseMath :: Element -> Parser Math
parseMath e =
  do  guardName "math" e
      kids <- traverse asElement (elContent e)
      case kids of
        [k] -> parseTop k
        _ -> die $ printf "don't know how to interpret top-level math expression '%s' with more than one element" (show e)

  where
    -- use on top-level applications and raw values
    parseTop :: Element -> Parser Math
    parseTop el =
      case qName (elName el) of
        "apply" -> traverse asElement (elContent el) >>= parseArgs
        "ci" ->
          do  body <- traverse asText (elContent el)
              case body of
                [b] -> pure (Var (pack b))
                _ -> die $ printf "could not interpret perhaps multi-part variable '%s'" (show body)
        "cn" ->
          do  tyM <- optAttr parseText el "type"
              body <- traverse asText (elContent el)
              case (tyM, body) of
                (Just "e-notation", _) -> die $ printf "e-notation not yet supported"
                (_, [b]) -> LitD <$> parseAny b
                _ -> die $ printf "could not interpret number '%s'" (show body)
        _ -> die $ printf "could not interpret math expression '%s'" (show el)

    -- use on application arguments, including "plus"/"minus" etc.
    parseArgs :: [Element] -> Parser Math
    parseArgs elems =
      case elems of
        (el:els) ->
          case qName (elName el) of
            "plus" -> foldl1 Add <$> traverse parseTop els
            "minus" -> foldl1 Sub <$> traverse parseTop els
            "times" -> foldl1 Mul <$> traverse parseTop els
            "divide" -> foldl1 Div <$> traverse parseTop els
            "and" -> foldl1 And <$> traverse parseTop els
            "or" -> foldl1 Or <$> traverse parseTop els
            "power" -> foldl1 Pow <$> traverse parseTop els
            n -> die $ printf "unknown mathematical operator '%s'" n
        [] -> die $ printf "unexpected empty application in math element %s" (show e)

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
      speciesRefSpecies <- reqAttr parseText e "species"
      speciesRefStoichiometry <- optAttr parseAny e "stoichiometry"
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
      localParamValue <- optAttr parseAny e "value"
      localParamUnits <- optAttr parseText e "units"
      pure LocalParam{..}

parseAny :: Read a => String -> Parser a
parseAny s =
  case readEither s of
    Right a -> pure a
    Left _ -> die $ printf "failed to parse '%s'" s

parseBool :: String -> Parser Bool
parseBool s =
  case map toLower s of
    "true" -> pure True
    "false" -> pure False
    _ -> die $ printf "failed to parse '%s' as boolean" s

parseText :: String -> Parser Text
parseText = pure . pack

-------------------------------------------------------------------------------

reqChild :: (Element -> Parser a) -> Element -> QName -> Parser a
reqChild parser e@(Element eName _ _ linum) cName =
  do  setLinum linum
      case findChild' cName e of
        Just e' -> parser e'
        Nothing -> die $ printf "couldn't find child named '%s' in element '%s'" cName eName

optChild :: (Element -> Parser a) -> Element -> QName -> Parser (Maybe a)
optChild parser e@(Element _ _ _ linum) cName =
  do  setLinum linum
      case findChild' cName e of
        Just e' -> Just <$> parser e'
        Nothing -> pure Nothing

findChild' :: QName -> Element -> Maybe Element
findChild' qn = filterChild (\(Element qn' _ _ _) -> qName qn == qName qn')

reqAttr :: (String -> Parser a) -> Element -> QName -> Parser a
reqAttr parser (Element _ attrs _ linum) key =
  do  setLinum linum
      case lookupAttr' key attrs of
        Just val -> parser val
        Nothing -> die $ printf "data error: required key %s not found" (qName key)

optAttr :: (String -> Parser a) -> Element -> QName -> Parser (Maybe a)
optAttr parser (Element _ attrs _ linum) key =
  do  setLinum linum
      case lookupAttr' key attrs of
        Just val -> Just <$> parser val
        Nothing -> pure Nothing

lookupAttr' :: QName -> [Attr] -> Maybe String
lookupAttr' k = lookupAttrBy (\q -> qName q == qName k)

withChildren :: Element -> (Element -> Parser a) -> Parser [a]
withChildren e@(Element _ _ _ linum) p =
  do  setLinum linum
      traverse p (elChildren e)

appChildren :: (Element -> Parser a) -> Element -> Parser [a]
appChildren = flip withChildren

guardName :: QName -> Element -> Parser ()
guardName s (Element n _ _ linum) =
  do  setLinum linum
      unless (qName s == qName n) (die $ printf "expected element to be named '%s', but it was named '%s'" s n)

asElement :: Content -> Parser Element
asElement c =
  case c of
    Elem el -> setLinum (elLine el) >> pure el
    _ -> die $ printf "expected an element, but found a %s" (show c)

asText :: Content -> Parser String
asText c =
  case c of
    Text cd -> setLinum (cdLine cd) >> pure (cdData cd)
    _ -> die $ printf "expected a text, but found a '%s'" (show c)

supportedSBMLVersion :: Int -> Int -> Bool
supportedSBMLVersion level version =
  level == 3 && version == 2

-------------------------------------------------------------------------------

removeWS :: Content -> [Content]
removeWS content =
  case content of
    Text (CData k s l)
      | all isSpace s -> []
      | otherwise -> [Text (CData k (strip s) l)]
    Elem (Element name attrs contents linum) ->
      [Elem (Element name attrs (concatMap removeWS contents) linum)]
    CRef _ -> undefined
  where
    strip = lStrip . rStrip
    lStrip = dropWhile isSpace
    rStrip = reverse . dropWhile isSpace . reverse

-------------------------------------------------------------------------------

instance IsString QName where
  fromString s = QName s uri prefix
    where
      uri = error "internal error: unexpected attempt to examine a QName's URI"
      prefix = error "internal error: unexpected attempt to examine a QName's prefix"

instance PrintfArg QName where
  formatArg (QName n _ _) = formatString n
