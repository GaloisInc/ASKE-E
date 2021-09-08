module Language.ASKEE.ABM 
  ( parseABM
  , printABM
  , lexABM
  , abmToModel
  , Model(..) )
  where

import Data.Text (Text)
import qualified Data.Text as Text
import Language.ASKEE.ABM.Syntax
import Language.ASKEE.ABM.GenLexer
import qualified Language.ASKEE.ABM.GenParser as Parse
import Language.ASKEE.ABM.Print
import Language.ASKEE.ABM.Translate

parseABM :: Text -> Either String Model
parseABM txt = 
  do  toks <- lexABM (Text.unpack txt) -- XXX: lex Text
      Parse.parseABM toks