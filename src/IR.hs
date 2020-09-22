{-# Language OverloadedStrings #-}
module IR where

import qualified Data.ByteString.Lazy as B
import           Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Text(Text)
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.:))
import qualified Data.Attoparsec.Text as Parse
import qualified Data.Attoparsec.Combinator as Atto
import           Control.Applicative(some)
import           Control.Monad.Fail(fail)

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
  deriving Show

instance Aeson.FromJSON Model where
  parseJSON = Aeson.withObject "model" $
    \v -> Model <$>
          v .: "states" <*>
          v .: "events" <*>
          v .: "constants" <*>
          v .: "expressions"


data ModelState =
  ModelState { stateVariable :: Ident
             , stateValue    :: ModelExp
             , stateMetadata :: ModelMetadata
             }
  deriving Show

instance Aeson.FromJSON ModelState where
  parseJSON = Aeson.withObject "state" $
    \v -> ModelState <$>
          v .: "state_variable" <*>
          v .: "initial_value" <*>
          v .: "metadata"


data ModelInput =
  ModelInput { inputEnablingPredicate :: ModelExp
             }
  deriving Show

instance Aeson.FromJSON ModelInput where
  parseJSON = Aeson.withObject "input" $
    \v -> ModelInput <$>
          v .: "enabling_predicate"  


data ModelOutput =
  ModelOutput { outputTransitionFunction :: Map Ident ModelExp
              }
  deriving Show

instance Aeson.FromJSON ModelOutput where
  parseJSON = Aeson.withObject "output" $
    \v -> ModelOutput <$>
          v .: "transition_function"
  

data ModelEvent =
  ModelEvent { eventRate       :: ModelExp
             , eventInput      :: ModelInput
             , eventOuput      :: ModelOutput
             , eventMetadata   :: ModelMetadata
             }
  deriving Show

instance Aeson.FromJSON ModelEvent where
  parseJSON = Aeson.withObject "event" $
    \v -> ModelEvent <$>
          v .: "rate" <*>
          v .: "input_predicate" <*>
          v .: "output_predicate" <*>
          v .: "metadata"


data ModelMetadata =
  ModelMetadata { metadataDescription :: Text
                }
  deriving Show

instance Aeson.FromJSON ModelMetadata where
  parseJSON = Aeson.withObject "metadata" $
    \v -> ModelMetadata <$>
          v .: "description"


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
  deriving Show

instance Aeson.FromJSON ModelExp where
  parseJSON = Aeson.withText "exp" $
    \t -> case Parse.parse parseExp t of
            Parse.Fail unconsumed contexts err -> fail err
            Parse.Partial cont -> fail "unexpected EOF"
            Parse.Done unconsumed exp -> pure exp

parseExp :: Parse.Parser ModelExp
parseExp =
  tok $ Atto.choice
    [ binop "+" Add
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
    , parseTerm
    , idx
    ]
  where
    parseTerm = do
      tok $ Atto.choice
        [ LitNum <$> tok Parse.double
        , Var . Text.pack <$> some identifierChar
        , litArray
        , parens
        ]
      
    tok prs =
      do  Parse.skipSpace
          res <- prs
          Parse.skipSpace
          pure res

    binop op cns =
      do  e1 <- parseTerm
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
      do  arrExp <- parseTerm
          _ <- tok $ Parse.char '['
          idxExp <- parseExp
          _ <- tok $ Parse.char ']'
          pure $ Idx arrExp idxExp

    parens =
      do  _ <- tok $ Parse.char '('
          inner <- parseExp
          _ <- tok $ Parse.char ')'
          pure inner
    identifierChar = Parse.satisfy (Parse.inClass "A-Za-z0-9\\_")

parseModelFile :: FilePath -> IO (Either String Model)
parseModelFile f = do
  js <- B.readFile f
  pure (Aeson.eitherDecode js)
