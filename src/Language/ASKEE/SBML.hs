module Language.ASKEE.SBML 
  ( parseL3V2
  , printL3V2 
  , parseL2V3
  , printL2V3

  , L3V2
  , L2V3
  ) where

import Data.Text ( unpack )

import           Language.ASKEE.SBML.Common.Parse ( parse )

import qualified Language.ASKEE.SBML.L2V3.Parse  as L2V3
import           Language.ASKEE.SBML.L2V3.Syntax as L2V3
import           Language.ASKEE.SBML.L2V3.ToXML  as L2V3

import qualified Language.ASKEE.SBML.L3V2.Parse  as L3V2
import           Language.ASKEE.SBML.L3V2.Syntax as L3V2
import           Language.ASKEE.SBML.L3V2.ToXML  as L3V2

type L3V2 = L3V2.SBML
type L2V3 = L2V3.SBML

parseL3V2 :: String -> Either String L3V2.SBML
parseL3V2 src = parse src L3V2.parseSBML

parseL2V3 :: String -> Either String L2V3.SBML
parseL2V3 src = parse src L2V3.parseSBML

printL3V2 :: L3V2.SBML -> String
printL3V2 = unpack . L3V2.sbmlToXML

printL2V3 :: L2V3.SBML -> String
printL2V3 = unpack . L2V3.sbmlToXML