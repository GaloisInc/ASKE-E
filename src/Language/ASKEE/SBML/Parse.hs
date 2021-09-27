{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.ASKEE.SBML.Parse where

import Control.Monad            ( unless )
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
                              , QName(..) )

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