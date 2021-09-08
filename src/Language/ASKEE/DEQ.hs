module Language.ASKEE.DEQ
  ( printDiffEqs
  , parseDiffEqs

  , simulate
  , fitModel
  , applyParams
  , modelSquareError
  , computeErrorPerVar
  , paramValue

  , DiffEqs
  ) where

import Data.Text(Text)
import qualified Data.Text as Text

import qualified Language.ASKEE.DEQ.GenParser as Parser
import           Language.ASKEE.DEQ.GenLexer  ( lexDiffEqs )
import           Language.ASKEE.DEQ.Print     ( printDiffEqs )
import           Language.ASKEE.DEQ.Simulate  ( simulate, fitModel, modelSquareError, computeErrorPerVar )
import           Language.ASKEE.DEQ.Syntax    ( applyParams, paramValue, DiffEqs )

parseDiffEqs :: Text -> Either String DiffEqs
parseDiffEqs txt =
  do toks <- lexDiffEqs (Text.unpack txt)  -- XXX: lex text
     Parser.parseDiffEqs toks
