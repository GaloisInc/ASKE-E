{
{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.DiffEq.GenParser where

import Data.Text ( Text )
import qualified Data.Text as T

import Language.ASKEE.DiffEq.GenLexer
import Language.ASKEE.DiffEq.Lexer    as Lexer
import Language.ASKEE.DiffEq.DiffEq   as Syntax
import Language.ASKEE.Expr            as Expr
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
%left var frac

%%

Eqns :            { [] }
     | Eqns Eqn   { $2 : $1 }

Eqn : frac '{' var '}' '{' var '}' '=' Exp   { Syntax.StateEq (fromD $3) $9 }
    | var '=' Exp                            { Syntax.VarEq $1 $3 }

Exp : Exp '+' Exp                    { Expr.Add $1 $3 }
    | Exp '-' Exp                    { Expr.Sub $1 $3 }
    | Exp '*' Exp                    { Expr.Mul $1 $3 }
    | Exp '/' Exp                    { Expr.Div $1 $3 }
    | frac '{' Exp '}' '{' Exp '}'   { Expr.Div $3 $6 }
    | '-' Exp                        { Expr.Neg $2 }
    | '(' Exp ')'                    { $2 }
    | var Exp                        { Expr.Mul (Expr.Var $1) $2 }
    | var                            { Expr.Var $1 }
    | lit                            { Expr.ALit $1 }

{
textFromVar :: Located Token -> Text
textFromVar (Located _ _ (Lexer.Var t)) = t
textFromVar _ = error "not a Var"

fromD :: Text -> Text
fromD t = 
  if T.length t > 1
  then T.tail t
  else error $ "numerator "<>T.unpack t<>" is not a differential"

parseError :: [Located Token] -> a
parseError [] = error "parse error at end of file"
parseError ((Located lin col tok):_) = 
  error $ "parse error at line "++show lin++", column "++show col++" (token: "++show tok++")"
}