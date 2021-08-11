module Language.ASKEE.ESL 
  ( checkModel
  , parseESL
  , parseExpr
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
import qualified Language.ASKEE.ESL.GenParser as Parser
import Language.ASKEE.ESL.GenLexer  ( lexModel )
import Language.ASKEE.ESL.Print     ( printModel, Doc )
import Language.ASKEE.ESL.Syntax    ( Model(..) )
import Language.ASKEE.Expr          ( Expr )

parseESL :: Text -> Either String Model
parseESL txt =
  do toks <- lexModel (Text.unpack txt) -- XXX: lex Text
     Parser.parseModel toks

parseExpr :: Text -> Either String Expr
parseExpr txt =
  do  toks <- lexModel (Text.unpack txt)
      Parser.parseExpr toks

printESL :: Model -> Doc
printESL = printModel
