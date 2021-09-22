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
  , sbmlInitialConditions :: [InitialCondition]
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
  , parameterValue    :: Maybe Double
  , parameterUnits    :: Maybe ID
  , parameterConstant :: Bool
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data InitialCondition = InitialCondition
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

newtype Interpreter a = Interpreter
  { unInterpreter :: ExceptT Error (State Location) a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadError Error
    , MonadState Location
    )

runInterpreter :: Interpreter a -> Either Error a
runInterpreter (Interpreter i) =
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

die :: String -> Interpreter a
die eMessage =
  do  eLoc <- get
      throwError Error{..}

setLinum :: Maybe Line -> Interpreter ()
setLinum = put . Location

parseSBML :: String -> Interpreter SBML
parseSBML src =
  do  sbmlCursor <- fromContent <$> xml
      sbmlVersion <- parseVersion (current sbmlCursor)

      modelCursor <- 
        case namedChild "model" sbmlCursor of
          Just c -> pure c
          Nothing -> die "sbml did not have a \"model\" child"

      sbmlName <- withNamedElement "model" (current modelCursor) \attrs _ ->
        reqVal asText attrs "name"

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

parseChildList :: String -> (Content -> Interpreter a) -> Cursor -> Interpreter [a]
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

withNamedElement :: String -> Content -> ([Attr] -> [Content] -> Interpreter a) -> Interpreter a
withNamedElement s c f =
  case c of
    Elem (Element (QName n _ _) attrs content linum)
      | n == s -> setLinum linum >> f attrs content
      | otherwise -> die $ printf "expected element '%s', but found '%s'" s n
    _ -> die $ printf "expected element-type content, but found %s" (show c)

parseVersion :: Content -> Interpreter Version
parseVersion c =
  withNamedElement "sbml" c \attrs _ ->
    do  versionLevel   <- reqVal asAny attrs "level"
        versionVersion <- reqVal asAny attrs "version"
        unless (versionLevel == 3 && versionVersion == 2) $
          die $ printf "unsupported SBML version: %i.%i" versionLevel versionVersion
        pure Version{..}

parseCompartment :: Content -> Interpreter Compartment
parseCompartment c =
  withNamedElement "compartment" c \attrs _ ->
    do  compartmentID         <- reqVal asText attrs "id"
        compartmentDimensions <- optVal asAny  attrs "spatialDimensions"
        compartmentSize       <- optVal asAny  attrs "size"
        compartmentUnits      <- optVal asText attrs "units"
        compartmentConstant   <- reqVal asBool attrs "constant"
        pure Compartment{..}

parseSpecies :: Content -> Interpreter Species
parseSpecies c =
  withNamedElement "species" c \attrs _ ->
    do  speciesID                    <- reqVal asText attrs "id"
        speciesName                  <- optVal asText attrs "name"
        speciesCompartment           <- reqVal asText attrs "compartment"
        speciesInitialAmount         <- optVal asAny  attrs "initialAmount"
        speciesInitialConc           <- optVal asAny  attrs "initialConcentration"
        speciesSubstanceUnits        <- optVal asText attrs "substanceUnits"
        speciesHasOnlySubstanceUnits <- reqVal asBool attrs "hasOnlySubstanceUnits"
        speciesBoundaryCondition     <- reqVal asBool attrs "boundaryCondition"
        speciesConstant              <- reqVal asBool attrs "constant"
        speciesConversionFactor      <- optVal asText attrs "conversionFactor"
        pure Species{..}

asText :: String -> Either String Text
asText = pure . pack

asBool :: String -> Either String Bool
asBool = readBool

asAny :: Read a => String -> Either String a
asAny = readEither'

reqVal :: Read a => (String -> Either String a) -> [Attr] -> QName -> Interpreter a
reqVal rd attrs key =
  case lookupAttr key attrs of
    Nothing -> die $ printf "data error: required key %s not found" (qName key)
    Just val -> either (die . printf "error when parsing key '%s': %s" (qName key)) pure (rd val)

optVal :: Read a => (String -> Either String a) -> [Attr] -> QName -> Interpreter (Maybe a)
optVal rd attrs key =
  case lookupAttr key attrs of
    Nothing -> pure Nothing
    Just val -> either (die . printf "error when parsing key '%s': %s" (qName key)) (pure . Just) (rd val)

readEither' :: Read a => String -> Either String a
readEither' s =
  case readEither s of
    Left _ -> Left $ printf "parse error: failed to interpret string '%s'" s
    Right a -> Right a

readBool :: String -> Either String Bool
readBool s =
  case map toLower s of
    "true" -> Right True
    "false" -> Right False
    _ -> Left $ "parse error: failed to parse purported boolean "<>s

instance IsString QName where
  fromString s = QName s Nothing Nothing

children :: Cursor -> [Cursor]
children c =
  map fromJust $
    takeWhile isJust [ getChild i c | i <- [0..] ]