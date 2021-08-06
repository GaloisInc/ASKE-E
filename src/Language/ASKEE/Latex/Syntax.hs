{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.ASKEE.Latex.Syntax where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Language.ASKEE.DEQ.Syntax ( DiffEqs )

newtype Latex = Latex { unLatex :: DiffEqs }
  deriving (Show, Eq, Ord, Generic, NFData)
