module Language.ASKEE.DEQ
  ( printDiffEqs
  , parseDiffEqs

  , simulate
  , fitModel
  , applyParams
  , modelSquareError
  , computeErrorPerVar

  , DiffEqs
  ) where

import Control.Monad ( (>=>) )

import qualified Language.ASKEE.DEQ.GenParser as Parser
import           Language.ASKEE.DEQ.GenLexer  ( lexDiffEqs )
import           Language.ASKEE.DEQ.Print     ( printDiffEqs )
import           Language.ASKEE.DEQ.Simulate  ( simulate, fitModel, modelSquareError, computeErrorPerVar )
import           Language.ASKEE.DEQ.Syntax    ( applyParams, DiffEqs )

parseDiffEqs :: String -> Either String DiffEqs
parseDiffEqs = lexDiffEqs >=> Parser.parseDiffEqs