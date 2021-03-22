{-# Language OverloadedStrings #-}
module Language.ASKEE.Experiment.Syntax where

import Data.Text(Text)
import qualified Data.Text.IO as TIO
import Data.Functor(($>), void)


import qualified Data.Attoparsec.Text as Atto
import Data.Attoparsec.Text(many', sepBy)

-------------------------------------------------------------------------------
-- Syntax

data Decl =
    DeclMeasure Measure
  | DeclExperiment Experiment
  deriving Show

data Experiment =
  Experiment { experimentName :: Text
             , experimentArgs :: [Text]
             , experimentImpl :: [ExperimentStmt]
             }
  deriving Show

data Measure =
  Measure { measureName :: Text
          , measureArgs :: [Text]
          , measureVars :: [(Text, ExperimentExpr)]
          , measureImpl :: (Text, [ExperimentStmt])
          }
  deriving Show

data Type =
    TypeNumber
  | TypeBool
  | TypeSequence Type
  | TypePoint [(Text, Type)]
  | TypeModel Text
  deriving Show

data Literal =
    LitBool Bool
  | LitNum Double
  deriving Show

data ExperimentExpr =
    Lit Literal
  | Var Text
  | MkArray [ExperimentExpr]
  | Call Text [ExperimentExpr]
  | RunMeasure ExperimentExpr Text [ExperimentExpr]
  | RunModel ExperimentExpr ExperimentExpr ExperimentExpr
  | Dot ExperimentExpr Text
  | Op2 Op2 ExperimentExpr ExperimentExpr
  | Op1 Op1 ExperimentExpr
  | MkPoint [(Text, ExperimentExpr)]
  deriving Show

data Op2 =
    Add
  | Subtract
  | Multiply
  | Divide
  | GreaterThan
  | LessThan
  | GreaterThanEqual
  | LessThanEqual
  | Equal
  | NotEqual
  | Or
  | And
  deriving Show

data Op1 =
    Negate
  | Not
  deriving Show

data ExperimentStmt =
    Set Text ExperimentExpr
  | Let Text ExperimentExpr
  | If [(ExperimentExpr, [ExperimentStmt])] [ExperimentStmt]
  | For Text ExperimentExpr [ExperimentStmt]
  | Yield ExperimentExpr
  deriving Show

-------------------------------------------------------------------------------
-- Parser

type Parser = Atto.Parser

tok :: Parser a -> Parser a
tok pa =
  do  Atto.skipSpace
      pa

tok' :: Parser a -> Parser ()
tok' = void . tok

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op = sc
  where
    sc = p >>= rest
    rest x = Atto.choice [ op <*> pure x <*> chainl p op
                         , pure x
                         ]

parseIdent :: Parser Text
parseIdent = tok ident
  where
    ident =
      do  first  <- Atto.takeWhile1 (Atto.inClass "a-zA-Z_")
          second <- Atto.takeWhile  (Atto.inClass "a-zA-Z0-9_")
          pure (first <> second)

parseArgList :: Parser [Text]
parseArgList = tok "(" *> sepBy parseIdent (tok ",") <* tok ")"

parseExpList :: Parser [ExperimentExpr]
parseExpList = Atto.sepBy parseExpr (tok ",")

parseExpr0 :: Parser ExperimentExpr
parseExpr0 =
  Atto.choice
    [ Lit . LitNum <$> tok Atto.double
    , MkArray <$> (tok "[" *> parseExpList <* tok "]")
    , MkPoint <$> (tok "{" *> pointList <* tok "}")
    , Call <$> parseIdent <*> (tok "(" *> parseExpList <* tok ")")
    , RunModel <$> (tok "sample" *> parseExpr) <*> (tok "until" *> parseExpr) <*> (tok "with" *> tok "seed" *> parseExpr)
    , Var <$> parseIdent
    , tok "(" *> parseExpr <* tok ")"
    ]
  where
    point = (,) <$> parseIdent <*> (tok "=" *> parseExpr)
    pointList = Atto.sepBy point (tok ",")

-- dot
parseExpr1 :: Parser ExperimentExpr
parseExpr1 =
  do  e0 <- parseExpr0
      foldl Dot e0 <$> many' (tok "." *> parseIdent)

parseExpr2 :: Parser ExperimentExpr
parseExpr2 =
  Atto.choice [ Op1 Not <$> (tok "!" *> parseExpr2)
              , Op1 Negate <$> (tok "-" *> parseExpr2)
              , parseExpr1
              ]

-- Multiply/div
parseExpr3 :: Parser ExperimentExpr
parseExpr3 = parseExpr2 `chainl` op
  where
    op =
      Atto.choice [ tok "*" $> Op2 Multiply
                  , tok "/" $> Op2 Divide
                  ]

-- add/sub
parseExpr4 :: Parser ExperimentExpr
parseExpr4 = parseExpr3 `chainl` op
  where
    op =
      Atto.choice [ tok "-" $> Op2 Subtract
                  , tok "+" $> Op2 Add
                  ]

-- comparison
parseExpr5 :: Parser ExperimentExpr
parseExpr5 = parseExpr4 `chainl` op
  where
    op =
      Atto.choice [ tok "==" $> Op2 Equal
                  , tok "!=" $> Op2 NotEqual
                  , tok "<" $> Op2 LessThan
                  , tok ">" $> Op2 GreaterThan
                  , tok "<=" $> Op2 LessThanEqual
                  , tok ">=" $> Op2 GreaterThanEqual
                  ]

parseExpr6 :: Parser ExperimentExpr
parseExpr6 = parseExpr5 `chainl` op
  where
    op =
      Atto.choice [ tok "||" $> Op2 Or
                  , tok "&&" $> Op2 And
                  ]

parseExpr7 :: Parser ExperimentExpr
parseExpr7 =
  do  e0 <- parseExpr6
      Atto.choice [ RunMeasure e0 <$> (tok "measure" *> parseIdent)
                                  <*> (tok "(" *> parseExpList <* tok ")")
                  , pure e0
                  ]

parseExpr :: Parser ExperimentExpr
parseExpr = parseExpr7

parseStmt :: Parser ExperimentStmt
parseStmt = Atto.choice [parseLet, parseIf, parseFor, parseYield, parseSet]
  where
    parseLet = Let <$> (tok "let" *> parseIdent)
                   <*> (tok "=" *> parseExpr <* tok ";")
    parseSet = Set <$> parseIdent
                   <*> (tok "=" *> parseExpr <* tok ";")

    parseFor = For <$> (tok "for" *> tok "(" *> parseIdent)
                   <*> (tok "in" *> parseExpr <* tok ")")
                   <*> parseStmtBlock

    parseYield = Yield <$> (tok "yield" *> parseExpr <* tok ";")
    parseIf =
      do  c1 <- parseIfClause "if"
          cs <- many' (parseIfClause "elif")
          els <- Atto.choice  [ tok "else" *> parseStmtBlock
                              , pure []
                              ]
          pure (If (c1:cs) els)

    parseIfClause kw = (,) <$> (tok kw *> tok "(" *> parseExpr <* tok ")")
                           <*> parseStmtBlock

parseStmtBlock :: Parser [ExperimentStmt]
parseStmtBlock = tok "{" *> many' parseStmt <* tok "}"

parseExperiment :: Parser Experiment
parseExperiment =
  tok "experiment" *>
    (Experiment <$> parseIdent <*> parseArgList <*> parseStmtBlock)

parseMeasure :: Parser Measure
parseMeasure =
  do  tok' "measure"
      name <- parseIdent
      args <- parseArgList
      tok' "{"

      vars <- many' varDecl

      tok' "on"
      varName <- parseIdent
      body <- parseStmtBlock


      tok' "}"
      pure (Measure name args vars (varName, body))
  where
    varDecl = (,) <$> (tok "var" *> parseIdent) <*> (tok "=" *> parseExpr <* tok ";")


parseDecls :: Parser [Decl]
parseDecls =
  many' $ Atto.choice [ DeclMeasure <$> parseMeasure
                      , DeclExperiment <$> parseExperiment
                      ]

-------------------------------------------------------------------------------

testFile :: FilePath -> IO (Either String [Decl])
testFile f =
  do  source <- TIO.readFile f
      pure $ Atto.parseOnly (tok parseDecls <* Atto.endOfInput) source
