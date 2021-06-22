module Language.ASKEE.Gromet 
  ( module G
  , grometString
  , PetriNetClassic
  ) where

import Language.ASKEE.Gromet.Syntax as G
import Language.ASKEE.Gromet.FromEasel as G (convertCoreToGromet)
import Language.ASKEE.Gromet.PetriNetClassic
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS8

grometString :: G.Gromet -> String
grometString g = BS8.unpack $ JSON.encode (JSON.toJSON g)
