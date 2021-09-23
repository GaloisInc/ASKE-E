{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.ASKEE.SBML where

import Control.DeepSeq          ( NFData )
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

import GHC.Generics ( Generic )

import Language.ASKEE.Expr    ( Expr(..) )

import Text.Printf            ( formatString
                              , printf
                              , PrintfArg(formatArg) )
import Text.Read              ( readEither )
import Text.XML.Light         ( elChildren
                              , filterChild 
                              , lookupAttr
                              , parseXML
                              , CData(..)
                              , Content(..)
                              , Element(..)
                              , Line
                              , QName(..) )

type ID = Text
type Math = Expr

data SBML = SBML
  { sbmlName               :: ID
  , sbmlVersion            :: Version
  , sbmlFunctionDefs       :: Maybe [Function]
  , sbmlUnitDefs           :: Maybe [UnitDef]
  , sbmlCompartments       :: Maybe [Compartment]
  , sbmlSpecies            :: Maybe [Species]
  , sbmlParameters         :: Maybe [Parameter]
  , sbmlInitialAssignments :: Maybe [InitialAssignment]
  , sbmlRules              :: Maybe [Rule]
  , sbmlConstraints        :: Maybe [Constraint]
  , sbmlReactions          :: Maybe [Reaction]
  , sbmlEvents             :: Maybe [Event]
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Version = Version
  { versionLevel   :: Int
  , versionVersion :: Int
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Function
  deriving (Eq, Generic, NFData, Ord, Show)

data UnitDef
  deriving (Eq, Generic, NFData, Ord, Show)

data Compartment = Compartment
  { compartmentID         :: ID
  , compartmentDimensions :: Maybe Int
  , compartmentSize       :: Maybe Double
  , compartmentUnits      :: Maybe ID
  , compartmentConstant   :: Bool
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Species = Species
  { speciesID                    :: ID
  , speciesName                  :: Maybe ID -- not in the spec
  , speciesCompartment           :: ID
  , speciesInitialAmount         :: Maybe Double
  , speciesInitialConc           :: Maybe Double
  , speciesSubstanceUnits        :: Maybe Text
  , speciesHasOnlySubstanceUnits :: Bool
  , speciesBoundaryCondition     :: Bool
  , speciesConstant              :: Bool
  , speciesConversionFactor      :: Maybe ID
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Parameter = Parameter
  { parameterID       :: ID
  , parameterName     :: Maybe ID -- not in the spec
  , parameterValue    :: Maybe Double
  , parameterUnits    :: Maybe ID
  , parameterConstant :: Bool
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data InitialAssignment = InitialAssignment
  { initialID     :: Maybe ID
  , initialSymbol :: ID
  , initialMath   :: Math
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Rule = AlgebraicRule | AssignmentRule | RateRule
  deriving (Eq, Generic, NFData, Ord, Show)

data Constraint
  deriving (Eq, Generic, NFData, Ord, Show)

data Reaction = Reaction
  { reactionID          :: ID
  , reactionReversible  :: Bool
  , reactionCompartment :: Maybe ID
  , reactionReactants   :: Maybe [SpeciesRef]
  , reactionProducts    :: Maybe [SpeciesRef]
  , reactionModifiers   :: Maybe [ModifierSpeciesRef]
  , reactionKineticLaw  :: Maybe KineticLaw
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data SpeciesRef = SpeciesRef
  { speciesRefID            :: Maybe ID
  , speciesRefSpecies       :: ID
  , speciesRefStoichiometry :: Maybe Double
  , speciesRefConstant      :: Bool
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data ModifierSpeciesRef = ModifierSpeciesRef
  { modifierSpeciesRefID      :: Maybe ID
  , modifierSpeciesRefSpecies :: ID
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data KineticLaw = KineticLaw
  { kineticMath        :: Maybe Math
  , kineticLocalParams :: Maybe [LocalParam]
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data LocalParam = LocalParam
  { localParamID    :: ID
  , localParamValue :: Maybe Double
  , localParamUnits :: Maybe ID
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Event
  deriving (Eq, Generic, NFData, Ord, Show)

data Required = Mandatory | Optional

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

runParser :: Parser a -> Either Error a
runParser (Parser i) = evalState (runExceptT i) (Location Nothing)

newtype Location = Location
  { locLinum :: Maybe Line
  }
  deriving (Show)

data Error = Error
  { eLoc :: Location
  , eMessage :: String
  }
  deriving (Show)

die :: String -> Parser a
die eMessage =
  do  eLoc <- get
      throwError Error{..}

setLinum :: Maybe Line -> Parser ()
setLinum = put . Location

parseSBML :: String -> Parser SBML
parseSBML src =
  do  sbml <- xml >>= asElement
      sbmlVersion <- parseVersion sbml

      model <- reqChild pure sbml "model"
      sbmlName <- reqAttr parseText model "name"

      let sbmlFunctionDefs = Nothing
      let sbmlUnitDefs = Nothing
      
      sbmlCompartments <- 
        optChild (appChildElements parseCompartment) model "listOfCompartments"
      sbmlSpecies <- 
        optChild (appChildElements parseSpecies) model "listOfSpecies"
      sbmlParameters <- 
        optChild (appChildElements parseParameter) model "listOfParameters"
      sbmlInitialAssignments <- 
        optChild (appChildElements parseInitialAssignment) model "listOfInitialAssignments"

      let sbmlRules = Nothing
      let sbmlConstraints = Nothing

      sbmlReactions <- 
        optChild (appChildElements parseReaction) model "listOfReactions"

      let sbmlEvents = Nothing

      pure SBML{..}
  where
    xml = stripHeader (concatMap removeWS (parseXML src))

    stripHeader x =
      do  setLinum (Just 0)
          case length x of
            0 -> die "this document was too empty"
            1 -> pure $ head x
            2 -> pure (x !! 1)
            _ -> die "this document was too full"

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

findChild' :: QName -> Element -> Maybe Element
findChild' qn = filterChild (\(Element qn' _ _ _) -> qName qn == qName qn')

reqChild :: (Element -> Parser a) -> Element -> QName -> Parser a
reqChild parse e@(Element eName _ _ linum) cName =
  do  setLinum linum
      case findChild' cName e of
        Just e' -> parse e'
        Nothing -> die $ printf "couldn't find child named '%s' in element '%s'" cName eName

optChild :: (Element -> Parser a) -> Element -> QName -> Parser (Maybe a)
optChild parse e@(Element _ _ _ linum) cName =
  do  setLinum linum
      case findChild' cName e of
        Just e' -> Just <$> parse e'
        Nothing -> pure Nothing

reqAttr :: (String -> Parser a) -> Element -> QName -> Parser a
reqAttr parse (Element _ attrs _ linum) key =
  do  setLinum linum
      case lookupAttr key attrs of
        Just val -> parse val
        Nothing -> die $ printf "data error: required key %s not found" (qName key)

optAttr :: (String -> Parser a) -> Element -> QName -> Parser (Maybe a)
optAttr parse (Element _ attrs _ linum) key =
  do  setLinum linum
      case lookupAttr key attrs of
        Just val -> Just <$> parse val
        Nothing -> pure Nothing

withChildElements :: Element -> (Element -> Parser a) -> Parser [a]
withChildElements e@(Element _ _ _ linum) p = 
  do  setLinum linum
      traverse p (elChildren e)

appChildElements :: (Element -> Parser a) -> Element -> Parser [a]
appChildElements = flip withChildElements

guardName :: QName -> Element -> Parser ()
guardName s (Element n _ _ linum) =
  do  setLinum linum
      unless (qName s == qName n) (die $ printf "expected element to be named '%s', but it was named '%s'" s n)

-------------------------------------------------------------------------------

parseVersion :: Element -> Parser Version
parseVersion e =
  do  guardName "sbml" e
      versionLevel <- reqAttr parseAny e "level"
      versionVersion <- reqAttr parseAny e "version"
      unless (versionLevel == 3 && versionVersion == 2) $
        die $ printf "unsupported SBML version: %i.%i" versionLevel versionVersion
      pure Version{..}

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
            "power" -> foldl1 Add <$> traverse parseTop els
            n -> die $ printf "unknown mathematical operator '%s'" n
        [] -> undefined

parseReaction :: Element -> Parser Reaction
parseReaction e =
  do  guardName "reaction" e
      reactionID <- reqAttr parseText e "id"
      reactionReversible <- reqAttr parseBool e "reversible"
      reactionCompartment <- optAttr parseText e "id"
      reactionReactants <- 
        optChild (appChildElements parseSpeciesRef) e "listOfReactants"
      reactionProducts <- 
        optChild (appChildElements parseSpeciesRef) e "listOfProducts"
      reactionModifiers <-
        optChild (appChildElements parseModifierSpeciesRef) e "listOfModifiers"
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
  do  guardName "speciesReference" e
      modifierSpeciesRefID <- optAttr parseText e "id"
      modifierSpeciesRefSpecies <- reqAttr parseText e "species"
      pure ModifierSpeciesRef{..}

parseKineticLaw :: Element -> Parser KineticLaw
parseKineticLaw e =
  do  guardName "kineticLaw" e
      kineticMath <- optChild parseMath e "math"
      kineticLocalParams <-
        optChild (appChildElements parseLocalParam) e "listOfLocalParameters"
      pure KineticLaw{..}

parseLocalParam :: Element -> Parser LocalParam
parseLocalParam e =
  do  guardName "" e
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

instance IsString QName where
  fromString s = QName s Nothing Nothing

instance PrintfArg QName where
  formatArg (QName n _ _) = formatString n

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