{
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ESL.GenParser where

import           Data.Either ( partitionEithers )
import qualified Data.Map    as Map
import           Data.Maybe  ( mapMaybe )
import           Data.Text   ( Text )

import           Language.ASKEE.Core             ( Ident, substExpr )
import           Language.ASKEE.Core.ImportASKEE ( expAsCore )
import           Language.ASKEE.DEQ.Syntax       ( DiffEqs(..) )
import           Language.ASKEE.Expr
import           Language.ASKEE.ESL.Lexer        ( Located(..) )
import qualified Language.ASKEE.ESL.Lexer        as Lexer
import           Language.ASKEE.ESL.Syntax
import qualified Language.ASKEE.Metadata         as Meta

import Prelude hiding (Ordering(..))
}

%name parseModel Model
%name parseModelMeta ModelMeta

%tokentype {Located Lexer.Token}
%error {parseError}
%monad {Either String}

%token
'and'           { Located _ _ Lexer.And }
'='             { Located _ _ Lexer.Assign }
BCLOSE          { Located _ _ Lexer.CloseBlock }
')'             { Located _ _ Lexer.CloseP }
':'             { Located _ _ Lexer.Colon }
'cond'          { Located _ _ Lexer.Cond }
'/'             { Located _ _ Lexer.Divide }
'/='            { Located _ _ Lexer.DivideAssign }
'effect'        { Located _ _ Lexer.Effect }
'else'          { Located _ _ Lexer.Else }
'=='            { Located _ _ Lexer.EQ }
'event'         { Located _ _ Lexer.Event }
'exp'           { Located _ _ Lexer.Exp }
'false'         { Located _ _ (Lexer.Boolean False)}
'>'             { Located _ _ Lexer.GT }
'>='            { Located _ _ Lexer.GTE }
'if'            { Located _ _ Lexer.If }
'<'             { Located _ _ Lexer.LT }
'<='            { Located _ _ Lexer.LTE }
'log'           { Located _ _ Lexer.Log }
'let'           { Located _ _ Lexer.Let }
meta            { Located _ _ (Lexer.Meta $$) }
'-'             { Located _ _ Lexer.Minus }
'-='            { Located _ _ Lexer.MinusAssign }
'model'         { Located _ _ Lexer.Model }
'not'           { Located _ _ Lexer.Not }
BOPEN           { Located _ _ Lexer.OpenBlock }
'('             { Located _ _ Lexer.OpenP }
'or'            { Located _ _ Lexer.Or }
'otherwise'     { Located _ _ Lexer.Otherwise }
'+'             { Located _ _ Lexer.Plus }
'+='            { Located _ _ Lexer.PlusAssign }
'rate'          { Located _ _ Lexer.Rate }
REAL            { Located _ _ (Lexer.Real $$) }
BSEP            { Located _ _ Lexer.BlockSeparator }
'state'         { Located _ _ Lexer.State }
SYM             { Located _ _ (Lexer.Sym $$) }
'then'          { Located _ _ Lexer.Then }
'true'          { Located _ _ (Lexer.Boolean True)}
'*'             { Located _ _ Lexer.Times }
'*='            { Located _ _ Lexer.TimesAssign }
'when'          { Located _ _ Lexer.When }
'assert'        { Located _ _ Lexer.Assert }
'parameter'     { Located _ _ Lexer.Parameter }

%left 'or'
%left 'and'
%nonassoc '<' '<=' '==' '>=' '>'
%nonassoc 'if' 'then' 'else'
%right 'not'
%left '+' '-'
%left '*' '/'

%%

Model                                 :: { Model }
  : ModelMeta                            { stripMeta $1 }

ModelMeta                             :: { ModelMeta }
  : 'model' SYM ':'
      BOPEN ModelDecls BCLOSE            { mkModel $2 $5 }

ModelDecls                            :: { [Either (Meta.MetaAnn Decl) Event] }
  :                                      { [] }
  | ModelDecls1                          { reverse $1 }

ModelDecls1                           :: { [Either (Meta.MetaAnn Decl) Event] }
  : ModelDecl                            { [$1] }
  | ModelDecls1 BSEP ModelDecl           { $3 : $1 }

ModelDecl                             :: { Either (Meta.MetaAnn Decl) Event }
  : MetaDecl                             { Left $1 }
  | Event                                { Right $1 }

MetaDecl                              :: { Meta.MetaAnn Decl }
  : Decl                                 { Meta.fromValue $1 }
  | meta BSEP MetaDecl                   { (uncurry Meta.withMeta) $1 $3 }

Decl                                  :: { Decl }
  : 'let'    SYM '=' Exp                 { Let $2 $4 }
  | 'state'  SYM '=' Exp                 { State $2 $4 }
  | 'assert' Exp                         { Assert $2 }
  | 'parameter' SYM '=' REAL             { Parameter $2 (Just $4) }

Event                                 :: { Event }
  : 'event' SYM ':' BOPEN
    WhenBlock
    RateBlock
    EffectBlock
    MDBlock
    BCLOSE                               { Event $2 $5 $6 $7 $8 }

WhenBlock                             :: { Maybe Expr }
  :                                      { Nothing }
  | 'when' ':' Exp BSEP                  { Just $3 }

RateBlock                             :: { Expr }
  : 'rate' ':' Exp BSEP                  { $3 }

EffectBlock                           :: { [Statement] }
  : 'effect' ':' BOPEN Statements BCLOSE { $4 }

-- XXX
MDBlock                               :: { Maybe Text }
  :                                      { Nothing }

Statements                            :: { [Statement] }
  :                                      { [] }
  | Statements1                          { reverse $1 }

Statements1
  : Statement                            { [$1] }
  | Statements1 BSEP Statement           { $3 : $1 }

Statement                             :: { Statement }
  : SYM '='  Exp                         { ($1, $3) }
  | SYM OpEq Exp                         { ($1, $2 (Var $1) $3) }

OpEq                                  :: { Expr -> Expr -> Expr }
  : '+='                                 { Add }
  | '-='                                 { Sub }
  | '*='                                 { Mul }
  | '/='                                 { Div }

Exp                                   :: { Expr }
  : Exp '+' Exp                          { Add  $1 $3 }
  | Exp '-' Exp                          { Sub  $1 $3 }
  | Exp '*' Exp                          { Mul  $1 $3 }
  | Exp '/' Exp                          { Div  $1 $3 }
  | '-' Exp                              { Neg  $2 }
  | 'exp' '(' Exp ')'                    { Exp  $3 }
  | 'log' '(' Exp ')'                    { Log  $3 }
  | Exp '>' Exp                          { GT  $1 $3 }
  | Exp '>=' Exp                         { GTE $1 $3 }
  | Exp '==' Exp                         { EQ  $1 $3 }
  | Exp '<=' Exp                         { LTE $1 $3 }
  | Exp '<' Exp                          { LT  $1 $3 }
  | Exp 'and' Exp                        { And $1 $3 }
  | Exp 'or'  Exp                        { Or  $1 $3 } 
  | 'not' Exp                            { Not $2 }
  | '(' Exp ')'                          { $2 }
  | SYM                                  { Var $1 }
  | REAL                                 { LitD $1 }
  | 'true'                               { LitB True }
  | 'false'                              { LitB False }
  | 'if' Exp 'then' Exp 'else' Exp       { If $2 $4 $6 }
  | CondExp                              { $1 }

CondExp                               :: { Expr }
  : 'cond' ':' BOPEN Condition BCLOSE    { $4 }

Condition                             :: { Expr }
  : CondChoices                          { Cond (reverse $1) Nothing }
  | CondChoices BSEP Exp 'otherwise'     { Cond (reverse $1) (Just $3) }

CondChoices
  : CondChoice                           { [$1] }
  | CondChoices BSEP CondChoice          { $3 : $1 }

CondChoice                            :: { (Expr,Expr) }
  : Exp 'if' Exp                         { ($1, $3) }



{
parseError :: [Located Lexer.Token] -> Either String a
parseError []     = Left $ "parse error at end of file"
parseError (t:ts) = Left $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t) ++ " (" ++ show t ++ ")"


mkModel :: Text -> [ Either (Meta.MetaAnn Decl) Event ] -> ModelMeta
mkModel nm ps = ModelMeta { modelMetaName = nm
                          , modelMetaDecls = ds
                          , modelMetaEvents = es
                          }
  where (ds,es) = partitionEithers ps
}



