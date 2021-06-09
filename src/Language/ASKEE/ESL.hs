module Language.ASKEE.ESL 
  ( checkModel
  , parseESL
  -- , parseESLMeta
  , printESL

  , describeModelInterface
  , modelAsCore
  -- , stripMeta

  , Model(..)
  )
  where

import Control.Monad ( (>=>) )

import Language.ASKEE.ESL.Check     ( checkModel )
import Language.ASKEE.ESL.Convert   ( modelAsCore )
import Language.ASKEE.ESL.GenParser ( parseModel )
import Language.ASKEE.ESL.GenLexer  ( lexModel )
import Language.ASKEE.ESL.Interface ( describeModelInterface )
import Language.ASKEE.ESL.Print     ( printModel, Doc )
import Language.ASKEE.ESL.Syntax    ( Model(..) )

parseESL :: String -> Either String Model
parseESL = lexModel >=> parseModel

-- parseESLMeta :: String -> Either String Model
-- parseESLMeta = lexModel >=> parseModel

printESL :: Model -> Doc
printESL = printModel