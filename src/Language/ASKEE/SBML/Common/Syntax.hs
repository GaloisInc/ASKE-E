{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.ASKEE.SBML.Common.Syntax where

import Control.DeepSeq ( NFData )

import Data.Data ( Data )
import Data.Text ( Text )

import GHC.Generics ( Generic )

import Language.ASKEE.Expr ( Expr )
import Language.ASKEE.SBML.Instances ()

import Text.XML.Light

type ID = Text
type Math = Expr

data Function = Function
  { functionID :: ID
  , functionName :: Maybe ID
  , functionArgs :: [ID]
  , functionBody :: Math
  }
  deriving (Eq, Generic, NFData, Ord, Show, Data)

data UnitDef = UnitDef
  { unitDefID :: ID
  , unitDefUnits :: Maybe [Unit]
  }
  deriving (Eq, Generic, NFData, Ord, Show, Data)

data Unit = Unit
  { unitKind       :: UnitKind
  , unitExponent   :: Double
  , unitScale      :: Int
  , unitMultiplier :: Double
  }
  deriving (Eq, Generic, NFData, Ord, Show, Data)

data UnitKind =
    Ampere
  | Avogadro
  | Becquerel
  | Candela
  | Coulomb
  | Dimensionless
  | Farad
  | Gram
  | Gray
  | Henry
  | Hertz
  | Item
  | Joule
  | Katal
  | Kelvin
  | Kilogram
  | Litre
  | Lumen
  | Lux
  | Metre
  | Mole
  | Newton
  | Ohm
  | Pascal
  | Radian
  | Second
  | Siemens
  | Sievert
  | Steradian
  | Tesla
  | Volt
  | Watt
  | Weber
  deriving (Eq, Generic, NFData, Ord, Show, Data)

data Parameter = Parameter
  { parameterID       :: ID
  , parameterName     :: Maybe ID
  , parameterValue    :: Maybe Double
  , parameterUnits    :: Maybe ID
  , parameterConstant :: Bool
  , parameterNotes    :: Maybe Notes
  }
  deriving (Eq, Generic, NFData, Ord, Show, Data)

newtype Notes = Notes { getNotes :: [Element] }
  deriving (Eq, Generic, NFData, Ord, Show, Data)

newtype Annotation = Annotation { getAnnotation :: [Element] }
  deriving (Eq, Generic, NFData, Ord, Show, Data)
