{-# Language BlockArguments, OverloadedStrings #-}
module SchemaJS
  ( importJSON
  , docsJSON
  , jsAtom
  , module Config.Schema.Spec
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.List(intersperse)
import Data.Scientific(coefficient,base10Exponent)
import qualified Data.Vector as Vector
import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashMap.Strict as HashMap

import Config
import Config.Number
import Config.Schema.Load(loadValue,ValueSpecMismatch)
import Config.Schema.Types
import Config.Schema.Spec

import qualified HTML
import SchemaGeneric

jsAtom :: Text -> ValueSpec ()
jsAtom lab = customSpec ("=" <> lab) anySpec check
  where
  check r = if r == lab then Right () else Left ("Expected: \"" <> lab <> "\"")

importJSON :: ValueSpec a -> JS.Value -> Either (ValueSpecMismatch ()) a
importJSON spec = loadValue spec . valueFromJSON

valueFromJSON :: JS.Value -> Value ()
valueFromJSON js =
  case js of
    JS.Object o -> Sections () (map toSection (HashMap.toList o))
      where toSection (x,y) = Section { sectionAnn = ()
                                      , sectionName = x
                                      , sectionValue = valueFromJSON y
                                      }
    JS.Array a  -> List () (map valueFromJSON (Vector.toList a))
    JS.String t -> Text () t
    JS.Number n ->
      Number ()
      MkNumber { numberCoefficient = fromInteger (coefficient n)
               , numberRadix       = Radix10 (toInteger (base10Exponent n))
               }
    JS.Bool b -> Atom () (MkAtom (if b then "true" else "false"))
    JS.Null   -> Atom () (MkAtom "null")


docsJSON :: ValueSpec a -> Lazy.ByteString
docsJSON = htmlDoc . generateDocs jsFormat


jsFormat :: DocFormat HTML.HTML
jsFormat = DocFormat
  { docTopDoc  = \ds defs -> mconcat [ divClass "top" d | d <- alts ds : defs ]
  , docDef     = \x d -> spanClass "label" (txt x <> ": ") <>
                                              divClass "defn" (alts d)
  , docSection = \opt name doc ty ->
                    let optDoc = if opt then "[optional] " else ""
                    in lit (str (show name) <> ": ") <>
                        alts ty <> divClass "doc" (optDoc <> txt doc)
  , docLabel   = \a -> lab (txt a)
  , docText    = lab "string"
  , docNumber  = lab "number"
  , docAnyAtom = lab "UNSUPPORTED"
  , docAtom    = \a -> case a of
                         "true"  -> lit "true"
                         "false" -> lit "false"
                         "null"  -> lit "null"
                         _       -> lab "UNSUPPORTED"
  , docList    = \as -> lit "[" <> alts as <> lit "]"

  , docSect =
    \fs -> case fs of
             [] -> lit "{}"
             _ : _ ->
               mconcat [ divClass "field" (lit s <> d)
                                  | (s,d) <- zip ("{" : repeat ",") fs ] <>
               divClass "field" (lit "}")

  , docAssoc  = \as -> lit "{" <> alts as <> lit "}"
  , docCustom = \d t ->
      case Text.uncons d of
        Just ('=',rest) -> lit (str (show rest))
        _               -> palts t <> spanClass "custom" (txt d)
  }

  where
  lit           = spanClass "literal"
  lab           = spanClass "label"

  alts          = mconcat . intersperse " | "
  palts as      = case as of
                    [a] -> a
                    _   -> "(" <> alts as <> ")"

  str           = HTML.text . Text.pack
  txt           = HTML.text
  spanClass c   = HTML.tag "span" [ ("class",c) ]
  divClass c    = HTML.tag "div"  [ ("class",c) ]


htmlDoc :: HTML.HTML -> Lazy.ByteString
htmlDoc xs =
    HTML.toLazyByteString $
    HTML.tag "html" [] $ mconcat
      [ HTML.tag "head" [] $
          HTML.tag "style" [] $
            mconcat
              [ "body { background-color: #eef; }"
              , ".top { padding: 1em; }"
              , ".literal { font-weight: bold; }"
              , ".label { font-style: italic;"
              , "         padding-left: 0.2em; padding-right:0.2em;"
              , "         margin-left: 0.2em; margin-right: 0.2em; }"
              , ".field { margin-bottom: 1ex; }"
              , ".defn { margin-left: 2em;"
              , "        padding: 0.5em;"
              , "        border: 1px solid #999;"
              , "        border-radius: 5px;"
              , "        background-color: white; }"
              , ".custom{ font-size: smaller; color: #999;"
              , "         font-style: italic; margin-right: 0.5em;"
              , "         vertical-align: super; }"
              , ".doc { font-style: italic; padding-left: 2em;"
              , "       color: #393; }"
              ]
      , HTML.tag "body" [] xs
      ]
