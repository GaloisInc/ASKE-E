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
import           Control.Monad.Combinators.Expr as Expr
import           Control.Monad.Fail(fail)
import           Control.Monad(when)

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


data ModelValue =
    ValueDouble Double
  | ValueList [Double]
  deriving Show

-- kinda gross, maybe we should redo it in alex/happy
parseExp :: Parse.Parser ModelExp
parseExp = makeExprParser parseTerm opTable
  where
    parseTerm =
      tok $ Atto.choice
       [ idx
       , parseAtom
       ]

    parseAtom =
      tok $ Atto.choice
        [ LitNum <$> tok Parse.double
        , LitNum . fromIntegral <$> tok (Parse.signed Parse.decimal)
        , Var . Text.pack <$> Parse.many1 identifierChar
        , litArray
        , parens
        ]

    tok prs =
      do  Parse.skipSpace
          res <- prs
          Parse.skipSpace
          pure res

    nest open prs close =
      do _ <- tok $ Parse.char open
         result <- prs
         _ <- tok $ Parse.char close
         pure result

    litArray =
      LitList <$> nest '[' (Parse.sepBy parseExp (tok $ Parse.char ',')) ']'

    idx =
      do  arrExp <- parseAtom
          idxs <- Parse.many1 $ nest '[' parseExp ']' 
          pure $ foldl Idx arrExp idxs

    parens = nest '(' parseExp ')'

    simpleBinop opStr constructor =
      Expr.InfixL $ (tok $ Parse.string opStr) >> pure constructor

    negateBinop opStr constructor =
      Expr.InfixL $ (tok $ Parse.string opStr) >> pure (\e1 e2 -> Not (constructor e1 e2))

    lt =
      Expr.InfixL . tok $ 
        do  _ <- Parse.char '<'
            _ <- Atto.lookAhead (Parse.notChar '=')
            pure LessThan

    gt =
      Expr.InfixL . tok $
        do _ <- Parse.char '>'
           _ <- Atto.lookAhead (Parse.notChar '=')
           pure GreaterThan

    simpleUnop opStr constructor =
      Expr.Prefix $ (tok $ Parse.string opStr) >> pure constructor  

    identifierChar = Parse.satisfy (Parse.inClass "A-Za-z0-9\\_")
    
    opTable =
      [ [ simpleUnop "-" Neg, simpleUnop "not" Not] 
      , [ simpleBinop "*" Mul, simpleBinop "/" Div] 
      , [ simpleBinop "+" Add, simpleBinop "-" Sub ]
      , [ lt, gt, negateBinop "<=" GreaterThan, negateBinop ">=" LessThan ]
      , [ simpleBinop "and" And, simpleBinop "or" Or ]
      ]


parseModelFile :: FilePath -> IO (Either String Model)
parseModelFile f = do
  js <- B.readFile f
  pure (Aeson.eitherDecode js)

-------------------------------------------------------------------------------

 -- it would be nice if we could locate errors

evalToDouble :: Map Ident ModelValue -> ModelExp -> Either String Double
evalToDouble syms e =
  do  val <- evalExp syms e
      case val of
        ValueDouble d -> pure d
        ValueList _ ->
          Left ("Expecting expression to produce a double, but got a list: " ++ show e)

evalExp :: Map Ident ModelValue -> ModelExp -> Either String ModelValue
evalExp syms e0 =
  case e0 of
    LitNum d     -> pure $ ValueDouble d
    LitList elts ->
      do  lst <- evalToDouble syms `traverse` elts
          pure (ValueList lst)

    Add e1 e2 -> binop e1 e2 (+)
    Sub e1 e2 -> binop e1 e2 (-)
    Mul e1 e2 -> binop e1 e2 (*)
    Div e1 e2 -> binop e1 e2 (/)  -- TODO: catch e2 == 0
    Neg e1 -> unop e1 negate
    Not e1 -> unop e1 (\i -> if i == 0.0 then 1.0 else 0.0)
    And e1 e2 -> binop e1 e2 (logOp (*))
    Or e1 e2  -> binop e1 e2 (logOp (+))
    LessThan e1 e2 -> cmp e1 e2 (<)
    GreaterThan e1 e2 -> cmp e1 e2 (>)
    -- there are several questions i have about lists before implementing this
    Idx lstExp idxExp -> undefined
    Var nm ->
      case Map.lookup nm syms of
        Nothing -> Left ("Could not find symbol " ++ Text.unpack nm)
        Just val -> pure val

  where
    logicalVal 0.0 = 0.0
    logicalVal _ = 1.0

    logOp op e1 e2 = logicalVal $ op (logicalVal e1) (logicalVal e2)

    cmp e1 e2 op =
      do  v1 <- evalToDouble syms e1
          v2 <- evalToDouble syms e2
          pure $ ValueDouble (if op v1 v2 then 1.0 else 0.0)

    unop e1 op =
      do  v1 <- evalToDouble syms e1
          pure $ ValueDouble (op v1)

    binop e1 e2 op =
      do  v1 <- evalToDouble syms e1
          v2 <- evalToDouble syms e2
          pure $ ValueDouble (op v1 v2)

    evalToList le =
      do  val <- evalExp syms le
          case val of
            ValueDouble _ ->
              Left ("Expecting expression to produce a list, but got a double: " ++ show le)
            ValueList l -> pure l 
   
   