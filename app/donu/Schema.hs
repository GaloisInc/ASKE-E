{-# Language ApplicativeDo, RecordWildCards, BlockArguments, OverloadedStrings #-}
module Schema
  ( module Schema
  , generateDocsJS
  ) where

import Data.Text(Text)
import SchemaJS

import Config.Schema

data Demo = Demo {
  numbers :: Maybe [Rational],
  yesOrNo :: Maybe Bool,
  string  :: Maybe Text,
  coords  :: [(Int, Int)]
  } deriving Show

demoSpec :: ValueSpec Demo
demoSpec = sectionsSpec "top-level configuration"
  do numbers <- optSection' "numbers" (oneOrList anySpec)
                "Try out the number syntax"
     yesOrNo <- optSection' "yes-or-no" yesOrNoSpec
                "Using atoms as enumerations"
     string  <- optSection "string"
                "Strings use Haskell syntax"
     coords  <- reqSection' "coordinates" (oneOrList nestedMapSpec)
                "Example required section of a nested map"
     return Demo{..}

nestedMapSpec :: ValueSpec (Int, Int)
nestedMapSpec = sectionsSpec "coord"
  do x <- reqSection "x" "x coordinate"
     y <- reqSection "y" "y coordinate"
     return (x,y)
