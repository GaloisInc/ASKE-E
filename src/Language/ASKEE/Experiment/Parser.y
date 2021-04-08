{
{-# Language BlockArguments ,ViewPatterns, GeneralizedNewtypeDeriving #-}
module Language.ASKEE.Experiment.Parser
  ( ParseError(..)
  , parse
  , parseFromFile
  , declsP
  , blockP
  , exprP
  ) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Exception(Exception(..),throwIO)
import MonadLib

import Language.ASKEE.Experiment.Syntax
import Language.ASKEE.Experiment.Lexer

}

%tokentype      { Lexeme Token }

%token
  IDENT         { (isLexIdent  -> Just $$) }
  NUMBER        { (isLexNumber -> Just $$) }

  '('           { Lexeme { lexemeRange = $$, lexemeToken = OpenParen    } }
  ')'           { Lexeme { lexemeRange = $$, lexemeToken = CloseParen   } }
  ','           { Lexeme { lexemeRange = $$, lexemeToken = Comma        } }
  ':'           { Lexeme { lexemeRange = $$, lexemeToken = Colon        } }

  '+'           { Lexeme { lexemeRange = $$, lexemeToken = OpPlus       } }
  '-'           { Lexeme { lexemeRange = $$, lexemeToken = OpDash       } }


  '*'           { Lexeme { lexemeRange = $$, lexemeToken = OpStar       } }
  '/'           { Lexeme { lexemeRange = $$, lexemeToken = OpSlash      } }
  '='           { Lexeme { lexemeRange = $$, lexemeToken = OpEq         } }
  '!'           { Lexeme { lexemeRange = $$, lexemeToken = OpBang       } }
  '.'           { Lexeme { lexemeRange = $$, lexemeToken = OpDot        } }

  '=='          { Lexeme { lexemeRange = $$, lexemeToken = OpEqEq       } }
  '!='          { Lexeme { lexemeRange = $$, lexemeToken = OpBangEq     } }
  '<='          { Lexeme { lexemeRange = $$, lexemeToken = OpLeq        } }
  '<'           { Lexeme { lexemeRange = $$, lexemeToken = OpLt         } }
  '>='          { Lexeme { lexemeRange = $$, lexemeToken = OpGeq        } }
  '>'           { Lexeme { lexemeRange = $$, lexemeToken = OpGt         } }

  '&&'          { Lexeme { lexemeRange = $$, lexemeToken = OpAmpAmp     } }
  '||'          { Lexeme { lexemeRange = $$, lexemeToken = OpBarBar     } }

  'experiment'  { Lexeme { lexemeRange = $$, lexemeToken = KWexperiment } }
  'measure'     { Lexeme { lexemeRange = $$, lexemeToken = KWmeasure    } }
  'sample'      { Lexeme { lexemeRange = $$, lexemeToken = KWsample     } }
  'with'        { Lexeme { lexemeRange = $$, lexemeToken = KWwith       } }
  'yield'       { Lexeme { lexemeRange = $$, lexemeToken = KWyield      } }
  'let'         { Lexeme { lexemeRange = $$, lexemeToken = KWlet        } }
  'if'          { Lexeme { lexemeRange = $$, lexemeToken = KWif         } }
  'then'        { Lexeme { lexemeRange = $$, lexemeToken = KWthen       } }
  'elif'        { Lexeme { lexemeRange = $$, lexemeToken = KWelif       } }
  'else'        { Lexeme { lexemeRange = $$, lexemeToken = KWelse       } }
  'end'         { Lexeme { lexemeRange = $$, lexemeToken = KWend        } }
  'number'      { Lexeme { lexemeRange = $$, lexemeToken = KWnumber     } }
  'bool'        { Lexeme { lexemeRange = $$, lexemeToken = KWbool       } }
  'false'       { Lexeme { lexemeRange = $$, lexemeToken = KWfalse      } }
  'true'        { Lexeme { lexemeRange = $$, lexemeToken = KWtrue       } }

%monad { Parser }
%lexer { nextToken } { Lexeme { lexemeToken = TokEOF } }

%name declsP decls
%name exprP expr
%name blockP block

%left     '||'
%left     '&&'
%nonassoc '==' '!='
%nonassoc '<' '>' '<=' '>='
%left     '+' '-'
%left     '*' '/'
%left     '.'
%nonassoc '!' NEG

%%

decls                                  :: { [Decl] }
  : listOf(decl)                          { $1 }

decl                                   :: { Decl }
  : experiment                            { DExperiment $1 }
  | measure                               { DMeasure $1 }

experiment                             :: { ExperimentDecl }
  : 'experiment' ident params
      listOf(experimentDecl)              { mkExperiment $2 $3 $4 }

experimentDecl                         :: { ExperimentDecl -> ExperimentDecl }
  : 'let' ident '=' expr                  { mkExperimentLet $2 $4 }
  | ident '=' 'sample' expr ident args    { mkSampleExpr $1 $4 $5 $6 }
  | ident '=' measureExpr                 { $3 $1 }
  | 'yield' ident '=' measureExpr         { mkYield $2 (Var $2) . $4 $2 }
  | 'yield' ident '=' expr                { mkYield $2 $4 }

measureExpr                   :: { Ident -> ExperimentDecl -> ExperimentDecl }
  : 'measure' ident 'with' ident args   { \x -> mkMeasureExpr x $2 $4 $5 }

measure                                :: { MeasureDecl }
  : 'measure' ident params
      listOf(measureVar)
      'with' ident block                  { MeasureDecl
                                              { measureName = $2
                                              , measureTArgs = []
                                              , measureConstraints = []
                                              , measureArgs = $3
                                              , measureVars = $4
                                              , measureDataBinder = $6
                                              , measureImpl = $7
                                              } }

measureVar                             :: { (Ident,Expr) }
  : ident '=' expr                        { ($1,$3) }



params                                 :: { [ Binder ] }
  : '(' sepBy(',',param) ')'              { $2 }

param                                  :: { Binder }
  : ident                                 { $1 }
  | ident ':' type                        { $1 { tnType = Just $3 } }


args                                   :: { [ Expr ] }
  : '(' sepBy(',',expr) ')'               { $2 }

type                                   :: { Type }
  : 'number'                              { TypeNumber }
  | 'bool'                                { TypeBool }


block                                  :: { [Stmt] }
  : listOf1(stmt)                         { $1 }

stmt                                   :: { Stmt }
  : ident '=' expr                        { Set $1 $3 }
  | 'let' ident '=' expr                  { Let $2 $4 }
  | ifStmt 'end'                          { $1 [] }
  | ifStmt 'else' block 'end'             { $1 $3 }


ifStmt                                 :: { [Stmt] -> Stmt }
  : 'if' thenStmt listOf(elseIf)          { If ($2 : $3) }

thenStmt                               :: { (Expr,[Stmt]) }
  : expr 'then' block                     { ($1,$3) }

elseIf                                 :: { (Expr,[Stmt]) }
  : 'elif' thenStmt                       { $2 }



expr                                   :: { Expr }
  : literal                               { Lit $1 }
  | ident                                 { Var $1 }
  | '(' expr ')'                          { $2 }

  | '!' expr                              { Call Not    [$2] }
  | '-' expr %prec NEG                    { Call Negate [$2] }

  | expr '+' expr                         { Call Add      [$1, $3] }
  | expr '-' expr                         { Call Subtract [$1, $3] }
  | expr '*' expr                         { Call Multiply [$1, $3] }
  | expr '/' expr                         { Call Divide   [$1, $3] }

  | expr '==' expr                        { Call Equal            [$1, $3] }
  | expr '!=' expr                        { Call NotEqual         [$1, $3] }
  | expr '<=' expr                        { Call LessThanEqual    [$1, $3] }
  | expr '<'  expr                        { Call LessThan         [$1, $3] }
  | expr '>=' expr                        { Call GreaterThanEqual [$1, $3] }
  | expr '>'  expr                        { Call GreaterThanEqual [$1, $3] }

  | expr '&&' expr                        { Call And [$1, $3] }
  | expr '||' expr                        { Call Or  [$1, $3] }

  | expr '.' IDENT                        { Dot $1 (snd $3) }

literal                                :: { Literal }
  : NUMBER                                { LitNum (snd $1) }
  | 'false'                               { LitBool False }
  | 'true'                                { LitBool True }

ident                                   :: { Ident }
  : IDENT                                  { untypedName (snd $1) }

--------------------------------------------------------------------------------
sepBy(s,p)                              :: { [p] }
  : revSepBy1(s,p)                         { reverse $1 }
  | {- empty -}                            { [] }

revSepBy1(s,p)                          :: { [p] }
  : p                                      { [$1] }
  | revSepBy1(s,p) s p                     { $3 : $1 }

listOf1(p)                              :: { [p] }
  : revListOf1(p)                          { reverse $1 }

listOf(p)                               :: { [p] }
  : revListOf1(p)                          { reverse $1 }
  | {- empty -}                            { [] }

revListOf1(p)                           :: { [p] }
  : p                                      { [$1] }
  | revListOf1(p) p                        { $2 : $1 }

{

data ParseError = ParseError SourceRange
  deriving Show

instance Exception ParseError

newtype Parser a = Parser (StateT [Lexeme Token] (ExceptionT ParseError Id) a)
  deriving (Functor,Applicative,Monad)

nextToken :: (Lexeme Token -> Parser a) -> Parser a
nextToken k = k =<< getNext
  where
  getNext =
    Parser $ sets \toks -> case toks of
                             []       -> error "Missing EOF token?"
                             [ l ]    -> (l, toks)
                             l : more -> (l, more)

happyError :: Parser a
happyError =
  Parser
    do ~(l : _) <- get
       raise (ParseError (lexemeRange l))


--------------------------------------------------------------------------------
mkExperiment ::
  Ident -> [Binder] -> [ExperimentDecl -> ExperimentDecl] -> ExperimentDecl
mkExperiment name params =
  foldr ($)
    ExperimentDecl
      { experimentName      = name
      , experimentArgs      = params
      , experimentInputs    = []
      , experimentYield     = []
      , experimentMeasures  = []
      , experimentLets      = []
      }

mkExperimentLet :: Ident -> Expr -> ExperimentDecl -> ExperimentDecl
mkExperimentLet x e d = d { experimentLets = (x,e) : experimentLets d }

mkSampleExpr ::
  Ident -> Expr -> Ident -> [Expr] -> ExperimentDecl -> ExperimentDecl
mkSampleExpr x r f es d =
  d { experimentInputs = (x,SampleExpr { seName = f, seArgs = es, seRange = r })
                       : experimentInputs d }

mkMeasureExpr ::
  Ident -> Ident -> Ident -> [Expr] -> ExperimentDecl -> ExperimentDecl
mkMeasureExpr x d f es decl =
  decl { experimentMeasures = (x, MeasureExpr { meDataset = d
                                              , meMeasureName = f
                                              , meArgs = es
                                              })
                            : experimentMeasures decl }

mkYield :: Ident -> Expr -> ExperimentDecl -> ExperimentDecl
mkYield x e decl =
  decl { experimentYield = (tnName x,e) : experimentYield decl }

--------------------------------------------------------------------------------

type LexPattern a = Lexeme Token -> Maybe (SourceRange, a)

isLexIdent :: LexPattern Text
isLexIdent x = case lexemeToken x of
                 Ident -> Just (lexemeRange x, lexemeText x)
                 _     -> Nothing

isLexNumber :: LexPattern Double
isLexNumber x = case lexemeToken x of
                  Integer i  -> Just (lexemeRange x, fromInteger i) -- XXX: looses precisiosn
                  Rational i -> Just (lexemeRange x, i)
                  _          -> Nothing


--------------------------------------------------------------------------------

parse ::  Parser a -> Text -> Text -> Either ParseError a
parse (Parser m) srcName thingToParse =
  fst <$> runId (runExceptionT (runStateT tokens m))
  where
  tokens   = lexer srcName thingToParse

-- Throws parse error
parseFromFile :: Parser a -> FilePath -> IO a
parseFromFile m file =
  do txt <- Text.readFile file
     case parse m (Text.pack file) txt of
       Right a  -> pure a
       Left err -> throwIO err

}


