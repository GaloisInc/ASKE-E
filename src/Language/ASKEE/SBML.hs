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
import Data.Maybe  ( fromJust, isJust )
import Data.String ( IsString(..) )
import Data.Text   ( Text, pack )

import GHC.Generics ( Generic )

import Language.ASKEE.Expr ( Expr )

import Text.Printf            ( printf )
import Text.Read              ( readEither )
import Text.XML.Light         ( parseXML
                              , lookupAttr
                              , Attr
                              , CData(..)
                              , Content(..)
                              , Element(..)
                              , Line
                              , QName(..) )
import Text.XML.Light.Cursor  ( findChild
                              , firstChild
                              , fromContent
                              , getChild
                              , Cursor(..) )

type ID = Text
type Math = Expr

data SBML = SBML
  { sbmlName              :: ID
  , sbmlVersion           :: Version
  , sbmlFunctionDefs      :: [Function]
  , sbmlUnitDefs          :: [UnitDef]
  , sbmlCompartments      :: [Compartment]
  , sbmlSpecies           :: [Species]
  , sbmlParameters        :: [Parameter]
  , sbmlInitialConditions :: [InitialAssignment]
  , sbmlRules             :: [Rule]
  , sbmlConstraints       :: [Constraint]
  , sbmlReactions         :: [Reaction]
  , sbmlEvents            :: [Event]
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
  , reactionReactants   :: [SpeciesRef]
  , reactionProducts    :: [SpeciesRef]
  , reactionModifiers   :: [ModifierSpeciesRef]
  , reactionKineticLaw  :: Maybe KineticLaw
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data SpeciesRef = SpeciesRef
  { speciesRefID :: ID
  , speciesRefStoichiometry :: Maybe Double
  , speciesRefConstant :: Bool
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data ModifierSpeciesRef = ModifierSpeciesRef
  { modifierSpeciesRefID :: ID
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data KineticLaw = KineticLaw
  { kineticMath        :: Maybe Math
  , kineticLocalParams :: [LocalParam]
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data LocalParam = LocalParam
  { localParamID :: ID
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
runParser (Parser i) =
  evalState (runExceptT i) (Location Nothing)

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
  do  sbmlCursor <- fromContent <$> xml
      sbmlVersion <- parseVersion (current sbmlCursor)

      modelCursor <- 
        case namedChild "model" sbmlCursor of
          Just c -> pure c
          Nothing -> die "sbml did not have a \"model\" child"

      sbmlName <- withNamedElement "model" (current modelCursor) \attrs _ ->
        reqAttr parseText attrs "name"

      let sbmlFunctionDefs = []
      let sbmlUnitDefs = []
      
      sbmlCompartments <- parseChildList "listOfCompartments" parseCompartment modelCursor
      sbmlSpecies <- parseChildList "listOfSpecies" parseSpecies modelCursor
      
      let sbmlParameters = []
      let sbmlInitialConditions = []
      let sbmlRules = []
      let sbmlConstraints = []
      let sbmlReactions = []
      let sbmlEvents = []

      pure SBML{..}
  where
    xml = stripHeader (concatMap stripXMLWS (parseXML src))

    stripXMLWS c =
      case c of
        Text (CData _ s _) 
          | all isSpace s -> []
          | otherwise -> [c]
        Elem (Element name attrs contents linum) -> 
          [Elem (Element name attrs (concatMap stripXMLWS contents) linum)]
        CRef _ -> undefined

    stripHeader x =
      do  setLinum (Just 0)
          case length x of
            0 -> die "this document was too empty"
            1 -> pure $ head x
            2 -> pure (x !! 1)
            _ -> die "this document was too full"

parseChildList :: String -> (Content -> Parser a) -> Cursor -> Parser [a]
parseChildList name parse cur =
  case namedChild name cur of
    Nothing -> pure []
    Just c -> traverse (parse . current) (children c)

namedChild :: String -> Cursor -> Maybe Cursor
namedChild s = findChild elementHasName
  where
    elementHasName c =
      case current c of
        Elem (Element (QName n _ _) _ _ _)
          | n == s -> True
        _ -> False

withNamedElement :: String -> Content -> ([Attr] -> [Content] -> Parser a) -> Parser a
withNamedElement s c f =
  case c of
    Elem (Element (QName n _ _) attrs content linum)
      | n == s -> setLinum linum >> f attrs content
      | otherwise -> die $ printf "expected element '%s', but found '%s'" s n
    _ -> die $ printf "expected element-type content, but found %s" (show c)

parseVersion :: Content -> Parser Version
parseVersion c =
  withNamedElement "sbml" c \attrs _ ->
    do  versionLevel   <- reqAttr parseAny attrs "level"
        versionVersion <- reqAttr parseAny attrs "version"
        unless (versionLevel == 3 && versionVersion == 2) $
          die $ printf "unsupported SBML version: %i.%i" versionLevel versionVersion
        pure Version{..}

parseCompartment :: Content -> Parser Compartment
parseCompartment c =
  withNamedElement "compartment" c \attrs _ ->
    do  compartmentID         <- reqAttr parseText attrs "id"
        compartmentDimensions <- optAttr parseAny  attrs "spatialDimensions"
        compartmentSize       <- optAttr parseAny  attrs "size"
        compartmentUnits      <- optAttr parseText attrs "units"
        compartmentConstant   <- reqAttr parseBool attrs "constant"
        pure Compartment{..}

parseSpecies :: Content -> Parser Species
parseSpecies c =
  withNamedElement "species" c \attrs _ ->
    do  speciesID                    <- reqAttr parseText attrs "id"
        speciesName                  <- optAttr parseText attrs "name"
        speciesCompartment           <- reqAttr parseText attrs "compartment"
        speciesInitialAmount         <- optAttr parseAny  attrs "initialAmount"
        speciesInitialConc           <- optAttr parseAny  attrs "initialConcentration"
        speciesSubstanceUnits        <- optAttr parseText attrs "substanceUnits"
        speciesHasOnlySubstanceUnits <- reqAttr parseBool attrs "hasOnlySubstanceUnits"
        speciesBoundaryCondition     <- reqAttr parseBool attrs "boundaryCondition"
        speciesConstant              <- reqAttr parseBool attrs "constant"
        speciesConversionFactor      <- optAttr parseText attrs "conversionFactor"
        pure Species{..}

parseParameter :: Content -> Parser Parameter
parseParameter c =
  withNamedElement "parameter" c \attrs _ ->
    do  parameterID       <- reqAttr parseText attrs "id"
        parameterName     <- optAttr parseText attrs "name"
        parameterValue    <- optAttr parseAny  attrs "value"
        parameterUnits    <- optAttr parseText attrs "units"
        parameterConstant <- reqAttr parseBool attrs "constant"
        pure Parameter{..}

parseInitialAssignment :: Content -> Parser InitialAssignment
parseInitialAssignment c =
  withNamedElement "initialAssignment" c \attrs contents ->
    do  initialID     <- optAttr parseText attrs "id"
        initialSymbol <- reqAttr parseText attrs "symbol"
        pure InitialAssignment{..}

parseMath :: [Content] -> Parser Math
parseMath cs = undefined

reqAttr :: (String -> Parser a) -> [Attr] -> QName -> Parser a
reqAttr parse attrs key =
  case lookupAttr key attrs of
    Nothing -> die $ printf "data error: required key %s not found" (qName key)
    Just val -> parse val

optAttr :: (String -> Parser a) -> [Attr] -> QName -> Parser (Maybe a)
optAttr parse attrs key =
  case lookupAttr key attrs of
    Nothing -> pure Nothing
    Just val -> Just <$> parse val

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

children :: Cursor -> [Cursor]
children c =
  map fromJust $
    takeWhile isJust [ getChild i c | i <- [0..] ]