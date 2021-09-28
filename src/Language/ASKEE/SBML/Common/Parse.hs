{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.ASKEE.SBML.Common.Parse where

import Control.Monad            ( unless, when )
import Control.Monad.State.Lazy ( evalState
                                , get
                                , MonadState(..)
                                , State )
import Control.Monad.Except     ( throwError
                                , runExceptT
                                , MonadError(..)
                                , ExceptT(..) )

import Data.Char   ( isSpace, toLower )
import Data.String ( IsString(..) )
import Data.Text   ( Text, pack )

import Language.ASKEE.Expr
import Language.ASKEE.SBML.Common.Syntax

import Text.Printf            ( formatString
                              , printf
                              , PrintfArg(formatArg) )
import Text.Read              ( readEither )
import Text.XML.Light         ( elChildren
                              , filterChild
                              , lookupAttrBy
                              , parseXML
                              , Attr
                              , CData(..)
                              , Content(..)
                              , Element(..)
                              , Line
                              , QName(..), filterChildren )

parse :: String -> (Element -> Parser a) -> Either String a
parse src parser =
  case runParser (parseFromString src parser) of
    Left err -> Left (ppError err)
    Right a -> Right a

parseFromString :: String -> (Element -> Parser a) -> Parser a
parseFromString src parser = xml >>= asElement >>= parser
  where
    xml = stripHeader (concatMap removeWS (parseXML src))
    stripHeader x =
      do  setLinum (Just 0)
          case length x of
            0 -> die "this document was too empty"
            1 -> pure $ head x
            2 -> pure (x !! 1)
            _ -> die "this document was too full"

-------------------------------------------------------------------------------

newtype Parser a = Parser
  { unParser :: ExceptT Error (State Location) a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError Error
    , MonadState Location
    )

data Error = Error
  { eLoc :: Location
  , eMessage :: String
  }
  deriving (Eq, Show)

ppError :: Error -> String
ppError Error{..} = printf "error at line %s: %s" ppLoc eMessage
  where
    ppLoc =
      case eLoc of
        Location Nothing -> "<no location>"
        Location (Just l) -> show l

newtype Location = Location
  { locLinum :: Maybe Line
  }
  deriving (Eq, Show)

runParser :: Parser a -> Either Error a
runParser (Parser i) = evalState (runExceptT i) (Location Nothing)

die :: String -> Parser a
die eMessage =
  do  eLoc <- get
      throwError Error{..}

setLinum :: Maybe Line -> Parser ()
setLinum = put . Location

-------------------------------------------------------------------------------

reqChild :: (Element -> Parser a) -> Element -> QName -> Parser a
reqChild parser e@(Element eName _ _ linum) cName =
  do  setLinum linum
      case findChild' cName e of
        Just e' -> parser e'
        Nothing -> die $ printf "couldn't find child named '%s' in element '%s'" cName eName

optChild :: (Element -> Parser a) -> Element -> QName -> Parser (Maybe a)
optChild parser e@(Element _ _ _ linum) cName =
  do  setLinum linum
      case findChild' cName e of
        Just e' -> Just <$> parser e'
        Nothing -> pure Nothing

findChild' :: QName -> Element -> Maybe Element
findChild' qn = filterChild (\(Element qn' _ _ _) -> qName qn == qName qn')

reqAttr :: (String -> Parser a) -> Element -> QName -> Parser a
reqAttr parser (Element _ attrs _ linum) key =
  do  setLinum linum
      case lookupAttr' key attrs of
        Just val -> parser val
        Nothing -> die $ printf "data error: required key %s not found" (qName key)

optAttr :: (String -> Parser a) -> Element -> QName -> Parser (Maybe a)
optAttr parser (Element _ attrs _ linum) key =
  do  setLinum linum
      case lookupAttr' key attrs of
        Just val -> Just <$> parser val
        Nothing -> pure Nothing

optAttrDef :: a -> (String -> Parser a) -> Element -> QName -> Parser a
optAttrDef def parser (Element _ attrs _ linum) key =
  do  setLinum linum
      case lookupAttr' key attrs of
        Just val -> parser val
        Nothing -> pure def

lookupAttr' :: QName -> [Attr] -> Maybe String
lookupAttr' k = lookupAttrBy (\q -> qName q == qName k)

withChildren :: Element -> (Element -> Parser a) -> Parser [a]
withChildren e@(Element _ _ _ linum) p =
  do  setLinum linum
      traverse p (elChildren e)

appChildren :: (Element -> Parser a) -> Element -> Parser [a]
appChildren = flip withChildren

guardName :: QName -> Element -> Parser ()
guardName s (Element n _ _ linum) =
  do  setLinum linum
      unless (qName s == qName n) (die $ printf "expected element to be named '%s', but it was named '%s'" s n)

asElement :: Content -> Parser Element
asElement c =
  do  setLinum (lineFrom c)
      case c of
        Elem el -> pure el
        _ -> die $ printf "expected an element, but found a %s" (show c)

asText :: Content -> Parser String
asText c =
  do  setLinum (lineFrom c)
      case c of
        Text cd -> pure (cdData cd)
        _ -> die $ printf "expected a text, but found a '%s'" (show c)

-- Use this instead of `traverse`ing to have the location set at the
-- first element, rather than the last
asElements :: [Content] -> Parser [Element]
asElements cons = reverse <$> traverse asElement (reverse cons)

-- Ditto
asTexts :: [Content] -> Parser [String]
asTexts cons = reverse <$> traverse asText (reverse cons)


lineFrom :: Content -> Maybe Line
lineFrom c =
  case c of
    Elem el -> elLine el
    Text cd -> cdLine cd
    CRef _ -> Nothing


parseRead :: Read a => String -> Parser a
parseRead s =
  case readEither s of
    Right a -> pure a
    Left _ -> die $ printf "failed to parse '%s'" s

parseBool :: String -> Parser Bool
parseBool s =
  case map toLower s of
    "true" -> pure True
    "false" -> pure False
    _ -> die $ printf "failed to parse '%s' as boolean" s

parseText :: String -> Parser Text
parseText = pure . pack

-------------------------------------------------------------------------------

parseNotes :: Element -> Parser Notes
parseNotes e = Notes <$> asElements (elContent e)

parseAnnotation :: Element -> Parser Annotation
parseAnnotation e = Annotation <$> asElements (elContent e)

parseMath :: Element -> Parser Math
parseMath e =
  do  guardName "math" e
      kids <- asElements (elContent e)
      case kids of
        [k] -> parseTop k
        _ -> die $ printf "don't know how to interpret top-level math expression '%s' with more than one element" (show e)

-- use on top-level applications and raw values
parseTop :: Element -> Parser Math
parseTop el =
  case qName (elName el) of
    "apply" -> asElements (elContent el) >>= parseApply
    "ci" ->
      do  body <- asTexts (elContent el)
          case body of
            [b] -> pure (Var (pack b))
            _ -> die $ printf "could not interpret perhaps multi-part variable '%s'" (show body)
    "cn" ->
      do  tyM <- optAttr parseText el "type"
          body <- asTexts (elContent el)
          case (tyM, body) of
            (Just "e-notation", _) -> die $ printf "e-notation not yet supported"
            (Just "rational", _) -> die $ printf "rational not yet supported"
            (_, [b]) -> LitD <$> parseRead b
            _ -> die $ printf "could not interpret number '%s'" (show body)
    _ -> die $ printf "could not interpret math expression '%s'" (show el)

-- use on application arguments, including "plus"/"minus" etc.
parseApply :: [Element] -> Parser Math
parseApply elems =
  case elems of
    (el:els) ->
      case qName (elName el) of
        "plus"   -> onEmptyElse els (pure (LitD 0)) Add
        "minus"  -> onSingletonElse els Neg Sub
        "times"  -> onEmptyElse els (pure (LitD 1)) Mul
        "divide" -> onEmptyElse els (die $ printf "no division identity") Div
        "and"    -> onEmptyElse els (pure (LitB (and []))) And
        "or"     -> onEmptyElse els (pure (LitB (or []))) Or
        "power"  -> foldl1 Pow <$> traverse parseTop els
        "piecewise" -> parsePiecewise el


        n -> die $ printf "unknown mathematical operator '%s'" n
    [] -> die $ printf "empty application in math element"

  where
    onEmptyElse es identity op =
      case es of
        [] -> identity
        _ -> foldl1 op <$> traverse parseTop es

    onSingletonElse es singletonOp op =
      case es of
        [] -> die $ printf "expected nonempty list of arguments in application of element '%s'" (show $ head elems)
        [e'] -> singletonOp <$> parseTop e'
        _ -> foldl1 op <$> traverse parseTop es

parsePiecewise :: Element -> Parser Expr
parsePiecewise el =
  do  guardName "piecewise" el
      let pieces = filterChildren (\el' -> qName (elName el') == "piece") el
      let other = filterChildren (\el' -> qName (elName el') == "otherwise") el
      when (length other /= 1) $
        die "can't parse piecewise without an 'otherwise' (yet)"
      piecesEs <- traverse parsePiece pieces
      otherE <- parseTop (head other)
      pure $ Cond piecesEs (Just otherE)

parsePiece :: Element -> Parser (Expr, Expr)
parsePiece el =
  do  guardName "piece" el
      case elChildren el of
        [action, condition] ->
          do  a <- parseTop action
              c <- parseTop condition
              pure (a, c)
        _ -> die $ printf "ill-formed piece '%s'" (show el)

parseFunction :: Element -> Parser Function
parseFunction e =
  do  guardName "functionDefinition" e
      functionID <- reqAttr parseText e "id"
      functionName <- optAttr parseText e "name"
      math <- reqChild pure e "math"
      lambda <- reqChild pure math "lambda"
      functionArgs <- parseArgs lambda
      functionBody <- parseBody lambda
      pure Function{..}

parseArgs :: Element -> Parser [ID]
parseArgs e =
  do  let args = filterChildren (\el -> qName (elName el) == "bvar") e
      traverse parseArg args

  where
    parseArg el =
      do  v <- reqChild pure el "ci"
          case elContent v of
            [Text (CData _ s _)] -> parseText s
            _ -> die ""

parseBody :: Element -> Parser Expr
parseBody e =
  do  let body = filterChildren (\el -> qName (elName el) /= "bvar") e
      when (length body /= 1) $
        die "bad function"
      parseMath (head body)

-------------------------------------------------------------------------------

removeWS :: Content -> [Content]
removeWS content =
  case content of
    Text (CData k s l)
      | all isSpace s -> []
      | otherwise -> [Text (CData k (strip s) l)]
    Elem (Element name attrs contents linum) ->
      [Elem (Element name attrs (concatMap removeWS contents) linum)]
    CRef _ -> undefined
  where
    strip = lStrip . rStrip
    lStrip = dropWhile isSpace
    rStrip = reverse . dropWhile isSpace . reverse

-------------------------------------------------------------------------------

instance IsString QName where
  fromString s = QName s uri prefix
    where
      uri = error "internal error: unexpected attempt to examine a QName's URI"
      prefix = error "internal error: unexpected attempt to examine a QName's prefix"

instance PrintfArg QName where
  formatArg (QName n _ _) = formatString n
