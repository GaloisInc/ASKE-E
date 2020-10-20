{
{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.DiffEq.GenParser where

import Data.Text ( Text
                 , append
                 )

import Language.ASKEE.DiffEq.GenLexer
import Language.ASKEE.DiffEq.Lexer    as Lexer
import Language.ASKEE.DiffEq.DiffEq   as Syntax
}

%name parse
%tokentype { Located Token }
%error { parseError }

%token
  '+'        { Located _ _ Lexer.Plus }
  '-'        { Located _ _ Lexer.Minus }
  '*'        { Located _ _ Lexer.Times }
  '/'        { Located _ _ Lexer.Divide }
  '='        { Located _ _ Lexer.Eq }
  frac       { Located _ _ Lexer.Frac }
  '{'        { Located _ _ Lexer.OpenB }
  '}'        { Located _ _ Lexer.CloseB }
  '('        { Located _ _ Lexer.OpenP }
  ')'        { Located _ _ Lexer.CloseP }
  var        { Located _ _ (Lexer.Var $$) }
  lit        { Located _ _ (Lexer.Lit $$) }

%left '+' '-'
%left '*' '/'
%right var frac

%%

Eqns :            { [] }
     | Eqns Eqn   { $2 : $1 }

Eqn : frac '{' var '}' '{' var '}' '=' Exp   { Syntax.StateEq ($3 `append` "/" `append` $6) $9 }
    | var '=' Exp                            { Syntax.VarEq   $1                            $3 }

Exp : Exp '+' Exp                    { Syntax.Add $1 $3 }
    | Exp '-' Exp                    { Syntax.Sub $1 $3 }
    | Exp '*' Exp                    { Syntax.Mul $1 $3 }
    | Exp '/' Exp                    { Syntax.Div $1 $3 }
    | frac '{' Exp '}' '{' Exp '}'   { Syntax.Div $3 $6 }
    | '-' Exp                        { Syntax.Neg $2 }
    | '(' Exp ')'                    { $2 }
    | var Exp                        { Syntax.Mul (Syntax.Var $1) $2 }
    | var                            { Syntax.Var $1 }
    | lit                            { Syntax.Lit $1 }

{
textFromVar :: Located Token -> Text
textFromVar (Located _ _ (Lexer.Var t)) = t
textFromVar _ = error "not a Var"

parseError :: [Located Token] -> a
parseError [] = error "parse error at end of file"
parseError ((Located lin col tok):_) = 
  error $ "parse error at line "++show lin++", column "++show col++" (token: "++show tok++")"
}