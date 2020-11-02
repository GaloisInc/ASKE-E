{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.ASKEE.GenParser where
import Language.ASKEE.GenLexer
import Language.ASKEE.Lexer  as Lexer
import Language.ASKEE.Syntax as Syntax
import Language.ASKEE.Expr   as Expr

import Control.Monad.State

import qualified Data.Map as M
}

%name parse
%tokentype {Located Token}
%error {parseError}

%token
and             { Located _ _ Lexer.And }
'='             { Located _ _ Lexer.Assign }
bclose          { Located _ _ Lexer.CloseBlock }
')'             { Located _ _ Lexer.CloseP }
':'             { Located _ _ Lexer.Colon }
cond            { Located _ _ Lexer.Cond }
'/'             { Located _ _ Lexer.Divide }
'/='            { Located _ _ Lexer.DivideAssign }
effect          { Located _ _ Lexer.Effect }
-- elif            { Located _ _ Lexer.Elif }
else            { Located _ _ Lexer.Else }
'=='            { Located _ _ Lexer.EQ }
event           { Located _ _ Lexer.Event }
'>'             { Located _ _ Lexer.GT }
'>='            { Located _ _ Lexer.GTE }
if              { Located _ _ Lexer.If }
'<'             { Located _ _ Lexer.LT }
'<='            { Located _ _ Lexer.LTE }
let             { Located _ _ Lexer.Let }
-- metadata        { Located _ _ Lexer.Metadata }
'-'             { Located _ _ Lexer.Minus }
'-='            { Located _ _ Lexer.MinusAssign }
model           { Located _ _ Lexer.Model }
not             { Located _ _ Lexer.Not }
bopen           { Located _ _ Lexer.OpenBlock }
'('             { Located _ _ Lexer.OpenP }
or              { Located _ _ Lexer.Or }
otherwise       { Located _ _ Lexer.Otherwise }
'+'             { Located _ _ Lexer.Plus }
'+='            { Located _ _ Lexer.PlusAssign }
rate            { Located _ _ Lexer.Rate }
real            { Located _ _ (Lexer.Real $$) }
';'             { Located _ _ Lexer.BlockSeparator }
state           { Located _ _ Lexer.State }
sym             { Located _ _ (Lexer.Sym $$) }
then            { Located _ _ Lexer.Then }
'*'             { Located _ _ Lexer.Times }
'*='            { Located _ _ Lexer.TimesAssign }
when            { Located _ _ Lexer.When }
assert          { Located _ _ Lexer.Assert }

%left and or
%nonassoc '<' '<=' '==' '>=' '>'
%nonassoc if then else
%right not       
%left '+' '-'
%left '*' '/'

%%

Model : model NamedBlockInit Decls Events BlockEnd { Syntax.Model $2 (reverse $3) (reverse $4) }

Decls :            { [] }
      | Decls Decl { $2 : $1 }

Decl : let   sym '=' Exp      ';'  { Syntax.Let $2 $4 }
     | let   sym '=' CondExp       { Syntax.Let $2 $4 }
     | state sym '=' Exp      ';'  { Syntax.State $2 $4 }
     | state sym '=' CondExp       { Syntax.State $2 $4 }
     | assert LExp ';'             { Syntax.Assert $2 }

Events :              { [] }
       | Events Event { $2 : $1 }

Event : event NamedBlockInit WhenBlock RateBlock EffectBlock MDBlock BlockEnd { Syntax.Event $2 $3 $4 $5 Nothing }

WhenBlock   :                                       { Nothing }
            | when   BlockInit LExp ';'    BlockEnd { Just $3 }
RateBlock   : rate   BlockInit Exp  ';'    BlockEnd { $3 }
EffectBlock : effect BlockInit Statements  BlockEnd { (reverse $3) }

MDBlock : {}

Statements :                      { [] }
           | Statements Statement { $2 : $1 }

Statement : sym '='  AExp ';' { ($1, $3) }
          | sym OpEq AExp ';' { ($1, $2 (Expr.Var $1) $3) }

OpEq : '+=' { Expr.Add }
     | '-=' { Expr.Sub }
     | '*=' { Expr.Mul }
     | '/=' { Expr.Div }

NamedBlockInit : sym ':' bopen  { $1 }
BlockInit      :     ':' bopen  {}
BlockEnd       :         bclose {}

Exp : AExp                       { Syntax.ArithExpr $1 }
    | '(' Exp ')'                { $2 }
    | if LExp then Exp else Exp  { Syntax.IfExpr $2 $4 $6 }

AExp : AExp '+' AExp    { Expr.Add  $1 $3 }
     | AExp '-' AExp    { Expr.Sub  $1 $3 }
     | AExp '*' AExp    { Expr.Mul  $1 $3 }
     | AExp '/' AExp    { Expr.Div  $1 $3 }
     | '-' AExp         { Expr.Neg  $2 }
     | '(' AExp ')'     { $2 }
     | sym              { Expr.Var $1 }
     | real             { Expr.ALit $1 }

LExp : AExp '>' AExp    { Expr.GT  $1 $3 }
     | AExp '>=' AExp   { Expr.GTE $1 $3 }
     | AExp '==' AExp   { Expr.EQ  $1 $3 }
     | AExp '<=' AExp   { Expr.LTE $1 $3 }
     | AExp '<' AExp    { Expr.LT  $1 $3 }
     | LExp and LExp    { Expr.And $1 $3 }
     | LExp or  LExp    { Expr.Or  $1 $3 } 
     | not LExp         { Expr.Not $2 }
     | '(' LExp ')'     { $2 }

CondExp : cond BlockInit Condition BlockEnd { Syntax.CondExpr $3 }

Condition : CondChoices CondOtherwise { Syntax.Condition (reverse $1) $2 }

CondChoices :                        { [] }
            | CondChoices CondChoice { $2 : $1 }

CondChoice : AExp if LExp ';' { ($1, $3) }

CondOtherwise :                    { Nothing }
              | AExp otherwise ';' { Just $1 }


{
parseError :: [Located Token] -> a
parseError []     = error $ "parse error at end of file"
parseError (t:ts) = error $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t) ++ " (" ++ show t ++ ")"
}