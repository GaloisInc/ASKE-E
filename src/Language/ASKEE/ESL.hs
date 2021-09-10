module Language.ASKEE.ESL 
  ( checkModel
  , parseESL
  -- , parseESLMeta
  , printESL

  , modelAsCore
  -- , stripMeta

  , Model(..)
  )
  where

import Data.Text(Text)
import qualified Data.Text as Text

import Language.ASKEE.ESL.Check     ( checkModel )
import Language.ASKEE.ESL.Convert   ( modelAsCore )
import Language.ASKEE.ESL.GenParser ( parseModel )
import Language.ASKEE.ESL.GenLexer  ( lexModel )
import Language.ASKEE.ESL.Print     ( printModel )
import Language.ASKEE.ESL.Syntax    ( Model(..) )

import Prettyprinter ( Doc )

parseESL :: Text -> Either String Model
parseESL txt =
  do toks <- lexModel (Text.unpack txt) -- XXX: lex Text
     parseModel toks

printESL :: Model -> Doc a
printESL = printModel
