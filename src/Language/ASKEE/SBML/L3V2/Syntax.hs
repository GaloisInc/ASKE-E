{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
module Language.ASKEE.SBML.L3V2.Syntax where

import Control.DeepSeq ( NFData )

import Data.Text ( Text )

import GHC.Generics ( Generic )

import Language.ASKEE.SBML.Common.Syntax

data SBML = SBML
  { sbmlLevel      :: Int
  , sbmlVersion    :: Int
  , sbmlNotes      :: Maybe Notes
  , sbmlAnnotation :: Maybe Annotation
  , sbmlModel      :: Maybe Model
  }
  deriving (Eq, Generic, NFData, Ord, Show)

-- TODO models have several more optional attributes, see p. 37
data Model = Model
  { modelName               :: Maybe ID
  , modelFunctionDefs       :: Maybe [Function]
  , modelUnitDefs           :: Maybe [UnitDef]
  , modelCompartments       :: Maybe [Compartment]
  , modelSpecies            :: Maybe [Species]
  , modelParameters         :: Maybe [Parameter]
  , modelInitialAssignments :: Maybe [InitialAssignment]
  , modelRules              :: Maybe [Rule]
  , modelConstraints        :: Maybe [Constraint]
  , modelReactions          :: Maybe [Reaction]
  , modelEvents             :: Maybe [Event]
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Compartment = Compartment
  { compartmentID         :: ID
  , compartmentDimensions :: Maybe Double
  , compartmentSize       :: Maybe Double
  , compartmentUnits      :: Maybe ID
  , compartmentConstant   :: Bool
  , compartmentAnnotation :: Maybe Annotation
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Species = Species
  { speciesID                    :: ID
  , speciesName                  :: Maybe ID
  , speciesCompartment           :: ID
  , speciesInitialAmount         :: Maybe Double
  , speciesInitialConc           :: Maybe Double
  , speciesSubstanceUnits        :: Maybe Text
  , speciesHasOnlySubstanceUnits :: Bool
  , speciesBoundaryCondition     :: Bool
  , speciesConstant              :: Bool
  , speciesConversionFactor      :: Maybe ID
  , speciesNotes                 :: Maybe Notes
  , speciesAnnotation            :: Maybe Annotation
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
  , reactionAnnotation  :: Maybe Annotation
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data SpeciesRef = SpeciesRef
  { speciesRefID            :: Maybe ID
  , speciesRefName          :: Maybe ID
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
