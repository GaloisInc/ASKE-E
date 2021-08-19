module Language.ASKEE.Gromet 
  ( module G
  , grometText
  , PetriNetClassic(..)
  ) where

import Language.ASKEE.Gromet.Syntax as G
import Language.ASKEE.Gromet.FromEasel as G (convertCoreToGromet)
import Language.ASKEE.Gromet.PetriNetClassic
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Text(Text)
import qualified Data.Text.Encoding as Text

grometText :: G.Gromet -> Text
grometText g = Text.decodeUtf8 $ LBS.toStrict $ JSON.encode (JSON.toJSON g)
