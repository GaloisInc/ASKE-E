module Language.ASKEE.SBML ( parseSBML, sbmlToXML, SBML(..) ) where

import qualified Language.ASKEE.SBML.Parse  as Parse
import           Language.ASKEE.SBML.Syntax ( SBML(..) )
import           Language.ASKEE.SBML.ToXML  ( sbmlToXML )

parseSBML :: String -> Either String SBML
parseSBML src = Parse.parse src Parse.parseSBML
