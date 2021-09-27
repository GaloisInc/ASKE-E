module Language.ASKEE.SBML ( parseSBML, sbmlToXML, SBML(..) ) where

import qualified Language.ASKEE.SBML.L3V2.Parse  as Parse
import           Language.ASKEE.SBML.L3V2.Syntax ( SBML(..) )
import           Language.ASKEE.SBML.L3V2.ToXML  ( sbmlToXML )

parseSBML :: String -> Either String SBML
parseSBML src = Parse.parse src Parse.parseSBML
