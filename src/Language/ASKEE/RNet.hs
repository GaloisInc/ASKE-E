module Language.ASKEE.RNet
  ( module M
  , parseRNet
  , rnetToCore
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Language.ASKEE.RNet.Syntax as M
import Language.ASKEE.RNet.GenLexer(lexRNet)
import qualified Language.ASKEE.RNet.GenParser as Parser
import Language.ASKEE.RNet.Convert(rnetToCore)

parseRNet :: Text -> Either String ReactionNet
parseRNet txt =
  do toks <- lexRNet (Text.unpack txt) -- XXX: lex Text
     Parser.parseRNet toks




