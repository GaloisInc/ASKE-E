module Language.ASKEE.DEQ
  ( parseDiffEqs
  , lexDiffEqs
  , printDiffEqs

  , simulate
  , fitModel
  , overwriteParameters

  , DiffEqs
  ) where

import Language.ASKEE.DEQ.GenParser ( parseDiffEqs )
import Language.ASKEE.DEQ.GenLexer  ( lexDiffEqs )
import Language.ASKEE.DEQ.Print     ( printDiffEqs )
import Language.ASKEE.DEQ.Simulate  ( simulate, fitModel )
import Language.ASKEE.DEQ.Syntax    ( overwriteParameters, DiffEqs )
