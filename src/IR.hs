{-# Language OverloadedStrings #-}
module IR where

import           Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Text(Text)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Parse
import qualified Data.Attoparsec.Combinator as Atto
import           Control.Applicative(many)

-- NOTES
-- Is there a meaningful distinction between expressions and constants?
-- Do we want to include some verification predicates for the models as part
--   of the model description - e.g. the total population is constant?
--

type Ident = Text

data Model =
  Model { modelState       :: Map Ident ModelState 
        , modelEvents      :: Map Ident ModelEvent
        , modelConstants   :: Map Ident Double
        , modelExpressions :: Map Ident ModelExp
        }

data ModelState =
  ModelState { stateVariable :: Ident
             , stateValue    :: Double
             , stateMetadata :: ModelMetadata
             }

data ModelInput =
  ModelInput { inputEnablingPredicate :: ModelExp
             }

data ModelOutput =
  ModelOutput { outputTransitionFunction :: Map Ident ModelExp
              }

data ModelEvent =
  ModelEvent { eventRate       :: ModelExp
             , eventInput      :: ModelInput
             , eventOuput      :: ModelOutput
             , eventMetadata   :: ModelMetadata
             }

data ModelMetadata =
  ModelMetadata { metadataDescription :: Text
                }

data ModelExp =
    Add ModelExp ModelExp
  | Mul ModelExp ModelExp
  | Sub ModelExp ModelExp
  | Div ModelExp ModelExp
  | Neg ModelExp
  | LitNum Double
  | LitList [ModelExp]
  | Idx ModelExp ModelExp
  | Var Ident
  | LessThan ModelExp ModelExp
  | GreaterThan ModelExp ModelExp
  | And ModelExp ModelExp
  | Or ModelExp ModelExp
  | Not ModelExp

parseExp :: Parse.Parser ModelExp
parseExp =
  tok $ Atto.choice
    [ LitNum <$> tok Parse.double
    , binop "+" Add
    , binop "-" Sub
    , binop "*" Mul
    , binop "/" Div
    , binop "<" LessThan
    , binop ">" GreaterThan
    , binop "<=" (\e1 e2 -> Not (GreaterThan e1 e2))
    , binop ">=" (\e1 e2 -> Not (LessThan e1 e2)) 
    , binop "and" And
    , binop "or" Or
    , unop "not" Not
    , unop "-" Neg
    , Var . Text.pack <$> many identifierChar
    , litArray
    , idx
    ]
  where
    tok prs =
      do  Parse.skipSpace
          res <- prs
          Parse.skipSpace
          pure res

    binop op cns =
      do  e1 <- parseExp
          _ <- tok $ Parse.string op
          e2 <- parseExp
          pure $ cns e1 e2

    unop op cns =
      do  _ <- tok $ Parse.string op
          e <- parseExp
          pure $ cns e

    litArray =
      do  _ <- tok $ Parse.char '['
          arr <- Parse.sepBy parseExp (tok $ Parse.char ',')
          _ <- tok $ Parse.char ']'
          pure $ LitList arr

    idx =
      do  arrExp <- parseExp
          _ <- tok $ Parse.char '['
          idxExp <- parseExp
          _ <- tok $ Parse.char ']'
          pure $ Idx arrExp idxExp

    identifierChar = Parse.satisfy (Parse.inClass "A-Za-z0-9\\_")

