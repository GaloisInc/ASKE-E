{-# Language OverloadedStrings #-}
-- | A simple module for generating HTML
module HTML
  ( HTML, Name
  , tag, text
  , toText, toLazyText, toByteString, toLazyByteString
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as LBS

type HTML = Builder -- ^ A piece of HTML
type Name = Builder -- ^ A name of a tag or an attribute

-- | Convert HTML to lazy text
toLazyText :: HTML -> Lazy.Text
toLazyText = Builder.toLazyText

-- | Convert HTML to strict text
toText :: HTML -> Text
toText = Lazy.toStrict . toLazyText

-- | Convert HTML to a lazy byte string, using UTF8
toLazyByteString :: HTML -> LBS.ByteString
toLazyByteString = Lazy.encodeUtf8 . toLazyText

-- | Convert HTML to a lazy byte string, using UTF8
toByteString :: HTML -> ByteString
toByteString = LBS.toStrict . toLazyByteString



-- | Make an HTML text element
text :: Text -> HTML
text = Text.foldr (cons . escInText) mempty

-- | Make an HTML tag
tag :: Name -> [(Name,Text)] -> HTML -> HTML
tag nm attrs body =
  "<" <> nm <> foldr (cons . attr) ">" attrs <> body <> "</" <> nm <> ">"

cons :: Builder -> Builder -> Builder
cons b = (b <>)

attr :: (Name,Text) -> Builder
attr (x,y) = " " <> x <> "=\"" <> Text.foldr (cons . escInAttr) "\"" y

escInText :: Char -> Builder
escInText c =
  case c of
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    _   -> Builder.singleton c

escInAttr :: Char -> Builder
escInAttr c =
  case c of
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    _   -> Builder.singleton c

