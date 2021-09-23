module Language.ASKEE.SBML ( parseSBML, SBML(..) ) where

import qualified Language.ASKEE.SBML.Parse  as Parse
import           Language.ASKEE.SBML.Syntax ( SBML(..) )

parseSBML :: String -> Either String SBML
parseSBML src = Parse.parse src Parse.parseSBML