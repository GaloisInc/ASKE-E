{-# Language BlockArguments, OverloadedStrings #-}
module SchemaJS where

import qualified Data.Text as Text
import Data.List(intercalate)
import Text.Read(readMaybe)
import Data.Scientific(Scientific,coefficient,base10Exponent)
import qualified Data.Vector as Vector
import qualified Data.Aeson as JS

import Config
import Config.Number
import Config.Schema.Types

import SchemaGeneric

valueToJSON :: Value a -> Maybe JS.Value
valueToJSON value =
  case value of
    Sections _ kvs -> JS.object <$> mapM section kvs
      where section kv =
              do js <- valueToJSON (sectionValue kv)
                 pure (sectionName kv JS..= js)

    List _ values -> JS.toJSON <$> mapM valueToJSON values
    Text _ t      -> pure (JS.toJSON t)

    Number _ num ->
      case numberRadix num of
        Radix10 {} ->
          do sci <- readMaybe (show (pretty value))
             pure (JS.toJSON (sci :: Scientific))
        _ -> Nothing

    Atom _ a ->
      case atomName a of
        "true"  -> pure (JS.toJSON True)
        "false" -> pure (JS.toJSON False)
        "null"  -> pure JS.Null
        _       -> Nothing


valueFromJSON :: JS.Value -> Maybe (Value ())
valueFromJSON js =
  case js of
    JS.Object o -> undefined
    JS.Array a  -> List () <$> mapM valueFromJSON (Vector.toList a)
    JS.String t -> Just (Text () t)
    JS.Number n ->
      Just $
      Number () $
      MkNumber { numberCoefficient = fromInteger (coefficient n)
               , numberRadix       = Radix10 (toInteger (base10Exponent n))
               }
    JS.Bool b -> Just (Atom () (MkAtom (if b then "true" else "false")))
    JS.Null -> Nothing


type HTML = String

generateDocsJS :: ValueSpec a -> HTML
generateDocsJS = generateDocs DocFormat
  { docTopDoc  = \ds defs -> mk (concatMap (divClass "top") (alts ds : defs))
  , docDef     = \x d -> spanClass "label" (txt x) ++ ":" ++
                                            divClass "defn" (alts d)
  , docSection = \opt name doc ty ->
                    let optDoc = if opt then "[optional] " else ""
                    in lit (str (show name)) ++ ":" ++ alts ty ++
                                          divClass "doc" (optDoc ++ txt doc)
  , docLabel   = lab . txt
  , docText    = lab "STRING"
  , docNumber  = lab "NUMBER"
  , docAnyAtom = lab "UNSUPPORTED"
  , docAtom    = \a -> case a of
                         "true"  -> lit "true"
                         "false" -> lit "false"
                         "null"  -> lit "null"
                         _       -> lab "UNSUPPORTED"
  , docList = \as -> lit "[" ++ alts as ++ lit "]"

  , docSect =
    \fs -> case fs of
             [] -> lit "{}"
             _ : _ ->
              concat ([ divClass "field" (lit s ++ d)
                        | (s,d) <- zip ("{" : repeat ",") fs ]
                                             ++ [ divClass "field" (lit "}") ])

  , docAssoc = \as -> lit "{" ++ alts as ++ lit "}"
  , docCustom = \d t -> palts t ++ spanClass "custom" (txt d)

  }
  where
  lit = spanClass "literal"
  lab = spanClass "label"

  alts = intercalate " | "
  palts as = case as of
               [a] -> a
               _   -> "(" ++ alts as ++ ")"

  str = concatMap esc
  txt = str . Text.unpack
  esc c = case c of
            '<' -> "&lt;"
            '>' -> "&gt;"
            '&' -> "&amp;"
            _   -> [c]

  spanClass c h = "<span class=" ++ show (c::String) ++ ">" ++ h ++ "</span>"
  divClass c h = "<div class=" ++ show (c::String) ++ ">" ++ h ++ "</div>"



  mk xs = unlines $
    [ "<html>"
    , "<head>"
    , "<style>"
    , ".top { padding: 1em; }"
    , ".literal { font-weight: bold; }"
    , ".label { font-style: italic;"
    , "         background-color: #eee;"
    , "         border-radius: 3px;"
    , "         padding-left: 0.2em; padding-right:0.2em;"
    , "         margin-left: 0.2em; margin-right: 0.2em; }"
    , ".field { margin-bottom: 1ex; }"
    , ".defn { padding-left: 2em; }"
    , ".custom{ font-size: smaller; color: #999;"
    , "         font-style: italic; margin-right: 0.5em;"
    , "         vertical-align: super; }"
    , ".doc { font-style: italic; padding-left: 2em;"
    , "       color: #393; }"
    , "</style>"
    , "</head>"
    , "<body>"
    , xs
    , "</body>"
    , "</html>"
    ]
