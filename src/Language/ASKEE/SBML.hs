module Language.ASKEE.SBML ( parseSBML, sbmlToXML, SBML(..) ) where

import           Language.ASKEE.SBML.Common.Parse ( parse )
import qualified Language.ASKEE.SBML.L3V2.Parse  as Parse
import           Language.ASKEE.SBML.L3V2.Syntax ( SBML(..) )
import           Language.ASKEE.SBML.L3V2.ToXML  ( sbmlToXML )

parseSBML :: String -> Either String SBML
parseSBML src = parse src Parse.parseSBML
