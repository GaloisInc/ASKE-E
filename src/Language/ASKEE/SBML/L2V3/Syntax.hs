{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
module Language.ASKEE.SBML.L2V3.Syntax where

import Control.DeepSeq ( NFData )

import Data.Text ( Text )

import GHC.Generics ( Generic )

import Language.ASKEE.SBML.Common.Syntax

data SBML = SBML
  { sbmlLevel      :: Int
  , sbmlVersion    :: Int
  , sbmlNotes      :: Maybe Notes
  , sbmlAnnotation :: Maybe Annotation
  , sbmlModel      :: Model
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Model = Model
  { modelID                 :: Maybe ID
  , modelName               :: Maybe ID
  , modelFunctionDefs       :: Maybe [Function]
  , modelUnitDefs           :: Maybe [UnitDef]
  , modelCompartmentTypes   :: Maybe [CompartmentType]
  , modelSpeciesTypes       :: Maybe [SpeciesType]
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

data CompartmentType
  deriving (Eq, Generic, NFData, Ord, Show)

data SpeciesType
  deriving (Eq, Generic, NFData, Ord, Show)

data Compartment = Compartment
  { compartmentID         :: ID
  , compartmentName       :: Maybe ID
  , compartmentType       :: Maybe ID
  , compartmentDimensions :: Int
  , compartmentSize       :: Maybe Double
  , compartmentUnits      :: Maybe ID
  , compartmentOutside    :: Maybe ID
  , compartmentConstant   :: Bool
  , compartmentAnnotation :: Maybe Annotation
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Species = Species
  { speciesID                    :: ID
  , speciesName                  :: Maybe ID
  , speciesType                  :: Maybe ID
  , speciesCompartment           :: ID
  , speciesInitialAmount         :: Maybe Double
  , speciesInitialConc           :: Maybe Double
  , speciesSubstanceUnits        :: Maybe Text
  , speciesHasOnlySubstanceUnits :: Bool
  , speciesBoundaryCondition     :: Bool
  , speciesCharge                :: Maybe Int
  , speciesConstant              :: Bool
  , speciesNotes                 :: Maybe Notes
  , speciesAnnotation            :: Maybe Annotation
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data InitialAssignment = InitialAssignment
  { initialSymbol :: ID
  , initialMath   :: Math
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Rule = 
    AlgebraicRule 
      { ruleMath :: Math 
      }
  | AssignmentRule
      { ruleMath :: Math
      , ruleVariable :: ID 
      } 
  | RateRule
      { ruleMath :: Math
      , ruleVariable :: ID
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data Constraint
  deriving (Eq, Generic, NFData, Ord, Show)

data Reaction = Reaction
  { reactionID          :: ID
  , reactionName        :: Maybe ID
  , reactionReversible  :: Bool
  , reactionFast        :: Bool
  -- , reactionCompartment :: Maybe ID
  , reactionReactants   :: Maybe [SpeciesRef]
  , reactionProducts    :: Maybe [SpeciesRef]
  , reactionModifiers   :: Maybe [ModifierSpeciesRef]
  , reactionKineticLaw  :: Maybe KineticLaw
  , reactionAnnotation  :: Maybe Annotation
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data SpeciesRef = SpeciesRef
  { speciesRefID                :: Maybe ID
  , speciesRefName              :: Maybe ID
  , speciesRefSpecies           :: ID
  , speciesRefStoichiometry     :: Double
  , speciesRefStoichiometryMath :: Maybe Math
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data ModifierSpeciesRef = ModifierSpeciesRef
  { modifierSpeciesRefID      :: Maybe ID
  , modifierSpeciesRefName    :: Maybe ID
  , modifierSpeciesRefSpecies :: ID
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data KineticLaw = KineticLaw
  { kineticMath        :: Maybe Math
  , kineticLocalParams :: Maybe [Parameter]
  }
  deriving (Eq, Generic, NFData, Ord, Show)

data Event
  deriving (Eq, Generic, NFData, Ord, Show)
