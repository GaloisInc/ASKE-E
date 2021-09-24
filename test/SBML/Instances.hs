{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | TODO RGS: Docs
module SBML.Instances () where

import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text)
import Generic.Random (GenericArbitraryU(..))
import Language.ASKEE.Expr (Expr(..))
import Language.ASKEE.SBML.Syntax
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary SBML where
  arbitrary = SBML 3 2 <$> arbitrary

-- TODO: Replace this instance with a derived one once Function, Constraint,
-- and Event are no longer empty data types
instance Arbitrary Model where
  arbitrary = do
    name               <- arbitrary
    -- functionDefs       <- arbitrary
    unitDefs           <- arbitrary
    compartments       <- arbitrary
    species            <- arbitrary
    parameters         <- arbitrary
    initialAssignments <- arbitrary
    -- rules              <- arbitrary
    -- constraints        <- arbitrary
    reactions          <- arbitrary
    -- events             <- arbitrary
    pure $ Model{ modelName               = name
                , modelFunctionDefs       = Nothing
                , modelUnitDefs           = unitDefs
                , modelCompartments       = compartments
                , modelSpecies            = species
                , modelParameters         = parameters
                , modelInitialAssignments = initialAssignments
                , modelRules              = Nothing
                , modelConstraints        = Nothing
                , modelReactions          = reactions
                , modelEvents             = Nothing
                }

-- deriving via GenericArbitraryU Function           instance Arbitrary Function
deriving via GenericArbitraryU UnitDef            instance Arbitrary UnitDef
deriving via GenericArbitraryU Unit               instance Arbitrary Unit
deriving via GenericArbitraryU UnitKind           instance Arbitrary UnitKind
deriving via GenericArbitraryU Compartment        instance Arbitrary Compartment
deriving via GenericArbitraryU Species            instance Arbitrary Species
deriving via GenericArbitraryU Parameter          instance Arbitrary Parameter
deriving via GenericArbitraryU InitialAssignment  instance Arbitrary InitialAssignment
deriving via GenericArbitraryU Rule               instance Arbitrary Rule
-- deriving via GenericArbitraryU Constraint         instance Arbitrary Constraint
deriving via GenericArbitraryU Reaction           instance Arbitrary Reaction
deriving via GenericArbitraryU SpeciesRef         instance Arbitrary SpeciesRef
deriving via GenericArbitraryU ModifierSpeciesRef instance Arbitrary ModifierSpeciesRef
deriving via GenericArbitraryU KineticLaw         instance Arbitrary KineticLaw
deriving via GenericArbitraryU LocalParam         instance Arbitrary LocalParam
-- deriving via GenericArbitraryU Event              instance Arbitrary Event

instance Arbitrary Expr where
  arbitrary = sized $ \s -> do
    n <- choose (0, min s 6)
    if n == 0
      then oneof [ Var  <$> nonEmptyText
                 , LitD <$> arbitrary
                 ]
      else let baseCase = max 0 (n - 1)
               recCase  = n `div` 2 in
           frequency
             [ (1, resize recCase $ Add <$> arbitrary <*> arbitrary)
             , (1, resize recCase $ Sub <$> arbitrary <*> arbitrary)
             , (1, resize recCase $ Mul <$> arbitrary <*> arbitrary)
             , (1, resize recCase $ Div <$> arbitrary <*> arbitrary)
             , (1, resize recCase $ Pow <$> arbitrary <*> arbitrary)
             , (1, resize recCase $ And <$> arbitrary <*> arbitrary)
             , (1, resize recCase $ Or  <$> arbitrary <*> arbitrary)

             , (20, resize baseCase $ Var  <$> nonEmptyText)
             , (20, resize baseCase $ LitD <$> arbitrary)
             ]
    where
      nonEmptyText :: Gen Text
      nonEmptyText = do
        t <- arbitrary
        pure $ if T.null t
               then "e"
               else T.map (\c -> if isSpace c then 's' else c) t
