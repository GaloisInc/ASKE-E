{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.Latex.GenParser where

import           Data.Text  ( Text )
import qualified Data.Text  as Text
import qualified Data.Map   as Map
import           Data.Maybe ( mapMaybe )

import Language.ASKEE.Core             ( Ident )
import Language.ASKEE.Core.ImportASKEE ( expAsCore )
import Language.ASKEE.DEQ.Syntax       as Syntax
import Language.ASKEE.Expr             as Expr
import Language.ASKEE.Lexer            ( Located(..) )
import Language.ASKEE.Latex.GenLexer  
import Language.ASKEE.Latex.Lexer      as Lexer
}

%name        parseLatex
%tokentype { Located Token}
%error     { parseError }
%monad     { Either String }

%token
  '+'        { Located _ _ Lexer.Plus }
  '-'        { Located _ _ Lexer.Minus }
  '*'        { Located _ _ Lexer.Times }
  '/'        { Located _ _ Lexer.Divide }
  '='        { Located _ _ Lexer.Eq }
  frac       { Located _ _ Lexer.Frac }
  '{'        { Located _ _ Lexer.OpenBrace }
  '}'        { Located _ _ Lexer.CloseBrace }
  '('        { Located _ _ Lexer.OpenParen }
  ')'        { Located _ _ Lexer.CloseParen }
  'd'        { Located _ _ (Lexer.Sym "d") }
  dt         { Located _ _ (Lexer.Sym "dt") }
  SYM        { Located _ _ (Lexer.Sym $$) }
  REAL       { Located _ _ (Lexer.Lit $$) }

%left '+' '-'
%left '*' '/'
%left SYM frac

%%

Eqns                                      :: { Syntax.DiffEqs }
  : Eqns1                                    { mkDiffEqs $1 }

Eqns1                                     :: { [(String, Ident, Expr)]}
  : Eqn                                      { [$1] }
  | Eqns1 Eqn                                { $2 : $1 }

Eqn                                       :: { (String, Ident, Expr) }
  : frac '{' Diff '}' '{' dt '}' '=' Exp     { ("d/dt", $3, $9) }
  | SYM '=' Exp                              { ("let", $1, $3) }

Diff                                      :: { Ident }       
  : 'd' SYM                                  { $2 }

Exp                                       :: { Expr }
  : Exp '+' Exp                              { Expr.Add $1 $3 }
  | Exp '-' Exp                              { Expr.Sub $1 $3 }
  | Exp '*' Exp                              { Expr.Mul $1 $3 }
  | Exp '/' Exp                              { Expr.Div $1 $3 }
  | frac '{' Exp '}' '{' Exp '}'             { Expr.Div (Expr.Paren $3) (Expr.Paren $6) }
  | '-' Exp                                  { Expr.Neg $2 }
  | '(' Exp ')'                              { Expr.Paren $2 }
  | SYM Exp                                  { Expr.Mul (Expr.Var $1) $2 }
  | SYM                                      { Expr.Var $1 }
  | REAL                                     { Expr.LitD $1 }

{
-- textFromVar :: Located Token -> Text
-- textFromVar (Located _ _ (Lexer.Var t)) = t
-- textFromVar _ = error "not a Var"

fromD :: Text -> Text
fromD t
  | Text.length t > 1 && Text.head t == 'd' = Text.tail t
  | otherwise = error $ "numerator "<>Text.unpack t<>" is not a differential"

parseError :: [Located Token] -> Either String a
parseError [] = 
  fail $ "parse error at end of file"
parseError ((Located lin col t):ts) = 
  fail $ "parse error at line "++show lin++", col "++show col++" ("++show t++")"

mkDiffEqs :: [(String, Ident, Expr)] -> DiffEqs
mkDiffEqs decls =
  let lets       = Map.fromList $ mapMaybe (match "let") decls
      deqRates   = Map.fromList $ mapMaybe (match "d/dt") decls
      deqInitial = Map.intersection lets deqRates 
      deqLets    = Map.difference lets deqRates 
      deqParams  = []
  in  DiffEqs{..}

  where
    match s (s',i,e) = if s == s' then Just (i,expAsCore e) else Nothing
}