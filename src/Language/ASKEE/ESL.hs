module Language.ASKEE.ESL 
  ( checkModel
  , parseESL
  , parseESLMeta
  , printESL

  , describeModelInterface
  , modelAsCore
  , stripMeta

  , Model
  , ModelMeta
  )
  where

import Control.Monad ( (>=>) )

import Language.ASKEE.ESL.Check     ( checkModel )
import Language.ASKEE.ESL.Convert   ( modelAsCore )
import Language.ASKEE.ESL.GenParser ( parseModel, parseModelMeta )
import Language.ASKEE.ESL.GenLexer  ( lexModel )
import Language.ASKEE.ESL.Interface ( describeModelInterface )
import Language.ASKEE.ESL.Print     ( printModel )
import Language.ASKEE.ESL.Syntax    ( stripMeta, Model, ModelMeta )

import Text.PrettyPrint ( Doc )

parseESL :: String -> Either String Model
parseESL = lexModel >=> parseModel

parseESLMeta :: String -> Either String ModelMeta
parseESLMeta = lexModel >=> parseModelMeta

printESL :: Model -> Doc
printESL = printModel