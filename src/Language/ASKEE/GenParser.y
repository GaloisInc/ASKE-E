{
module Language.ASKEE.GenParser where

import Prelude hiding (Ordering(..))
import Data.Text(Text)

import qualified Language.ASKEE.GenLexer as Lexer
import           Language.ASKEE.Lexer(Located(..))
import qualified Language.ASKEE.Lexer as Lexer
import           Language.ASKEE.Syntax
import           Language.ASKEE.Expr


}

%name parse
%tokentype {Located Lexer.Token}
%error {parseError}

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
'>'             { Located _ _ Lexer.GT }
'>='            { Located _ _ Lexer.GTE }
'if'            { Located _ _ Lexer.If }
'<'             { Located _ _ Lexer.LT }
'<='            { Located _ _ Lexer.LTE }
'let'           { Located _ _ Lexer.Let }
-- metadata        { Located _ _ Lexer.Metadata }
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
';'             { Located _ _ Lexer.BlockSeparator }
'state'         { Located _ _ Lexer.State }
SYM             { Located _ _ (Lexer.Sym $$) }
'then'          { Located _ _ Lexer.Then }
'*'             { Located _ _ Lexer.Times }
'*='            { Located _ _ Lexer.TimesAssign }
'when'          { Located _ _ Lexer.When }
'assert'        { Located _ _ Lexer.Assert }

%left 'or'
%left 'and'
%nonassoc '<' '<=' '==' '>=' '>'
%nonassoc 'if' 'then' 'else'
%right 'not'
%left '+' '-'
%left '*' '/'

%%

Model                                :: { Model }
  : 'model' SYM ':' BOPEN
     Decls
     Events
     BCLOSE                             { Model $2 (reverse $5) (reverse $6) }

Decls                                :: { [Decl] }
  :                                     { [] }
  | Decls Decl                          { $2 : $1 }

Decl                                 :: { Decl }
  : 'let'    SYM '=' Exp ';'            { Let $2 $4 }
  | 'let'    SYM '=' CondExp            { Let $2 $4 }
  | 'state'  SYM '=' Exp ';'            { State $2 $4 }
  | 'state'  SYM '=' CondExp            { State $2 $4 }
  | 'assert' Exp ';'                    { Assert $2 }

Events                               :: { [Event] }
  :                                     { [] }
  | Events Event                        { $2 : $1 }

Event                                :: { Event }
  : 'event' SYM ':' BOPEN
    WhenBlock
    RateBlock
    EffectBlock
    MDBlock
    BCLOSE                              { Event $2 $5 $6 $7 $8 }

WhenBlock                            :: { Maybe Expr }
  :                                     { Nothing }
  | 'when' ':' BOPEN Exp  ';' BCLOSE    { Just $4 }

RateBlock                             :: { Expr }
  : 'rate' ':' BOPEN Exp  ';' BCLOSE     { $4 }

EffectBlock                           :: { [Statement] }
  : 'effect' ':' BOPEN Statements BCLOSE { (reverse $4) }

-- XXX
MDBlock                               :: { Maybe Text }
  :                                      { Nothing }

Statements                            :: { [Statement] }
  :                                      { [] }
  | Statements Statement                 { $2 : $1 }

Statement                             :: { Statement }
  : SYM '='  Exp ';'                     { ($1, $3) }
  | SYM OpEq Exp ';'                     { ($1, $2 (Var $1) $3) }

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
  | 'if' Exp 'then' Exp 'else' Exp       { If $2 $4 $6 }
  | CondExp                              { $1 }

CondExp                               :: { Expr }
  : 'cond' ':' BOPEN Condition BCLOSE    { $4 }

Condition                             :: { Expr }
  : CondChoices CondOtherwise            { Cond (reverse $1) $2 }

CondChoices                           :: { [(Expr,Expr)] }
  :                                      { [] }
  | CondChoices CondChoice               { $2 : $1 }

CondChoice                            :: { (Expr,Expr) }
  : Exp 'if' Exp ';'                     { ($1, $3) }

CondOtherwise                         :: { Maybe Expr }
  :                                      { Nothing }
  | Exp 'otherwise' ';'                  { Just $1 }


{
parseError :: [Located Lexer.Token] -> a
parseError []     = error $ "parse error at end of file"
parseError (t:ts) = error $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t) ++ " (" ++ show t ++ ")"
}
