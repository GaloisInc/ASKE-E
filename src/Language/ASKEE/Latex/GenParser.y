{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.Latex.GenParser where

import           Data.Char  ( isSpace )
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

%name        parseLatex Eqns
%name        parseExpr  Exp
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
  SYMT       { Located _ _ (Lexer.SymTime $$) }
  SYMI       { Located _ _ (Lexer.SymInit $$) }
  SYM        { Located _ _ (Lexer.Sym $$) }
  REAL       { Located _ _ (Lexer.Lit $$) }
  LF         { Located _ _ Lexer.Newline }

%left '+' '-'
%left '*' '/'
%left NEGPREC

%%

Eqns                                      :: { Syntax.DiffEqs }
  : Eqns1                                    { mkDiffEqs $1 }

Eqns1                                     :: { [(EqnType, Ident, Expr)]}
  : Eqn                                      { [$1] }
  | Eqns1 LF Eqn                             { $3 : $1 }

Eqn                                       :: { (EqnType, Ident, Expr) }
  : frac '{' Diff '}' '{' dt '}' '=' Exp     { (Rate, $3, $9) }
  | SYM '=' Exp                              { (Let, $1, $3) }
  | SYMI '=' Exp                             { (Init, $1, $3) }

Diff                                      :: { Ident }       
  : 'd' SYM                                  { $2 }
  | SYM                                      {% fromDiff $1 }

Exp                                       :: { Expr }
  : Exp '+' Exp                              { Expr.Add $1 $3 }
  | Exp '-' Exp                              { Expr.Sub $1 $3 }
  | Exp '*' Exp                              { Expr.Mul $1 $3 }
  | Exp '/' Exp                              { Expr.Div $1 $3 }
  | '-' Exp       %prec NEGPREC              { Expr.Neg $2 }
  | WSMultiply                                     { $1 }
  | SYMI                                     { Expr.Mul (Expr.Var $1) (Expr.LitD 0) }
  | '(' Exp ')'                              { Expr.Paren $2 }
  | frac '{' Exp '}' '{' Exp '}'             { Expr.Div (Expr.Paren $3) (Expr.Paren $6) }

WSMultiply                                        :: { Expr }
  : Sym                                        { Expr.Var $1 }
  | REAL                                       { Expr.LitD $1 }
  | WSMultiply Sym                             { Expr.Mul $1 (Expr.Var $2) }

Sym                                       :: { Text }
  : SYM                                      { $1 }
  | SYMT                                     { $1 }

{
data EqnType = Let | Init | Rate
  deriving Eq

fromDiff :: Text -> Either String Text
fromDiff t
  | Text.length t > 1 && Text.head t == 'd' = pure $ Text.dropWhile isSpace $ Text.tail t
  | otherwise = fail $ "numerator "<>Text.unpack t<>" is not a differential"

mkInit :: Double -> Ident -> Expr -> Either String (EqnType, Ident, Expr)
mkInit d i t
  | d == 0 = pure (Init, i, t) 
  | otherwise = fail "only initial values at time = 0 are supported"

parseError :: [Located Token] -> Either String a
parseError [] = 
  fail $ "parse error at end of file"
parseError ((Located lin col t):ts) = 
  fail $ "parse error at line "++show lin++", col "++show col++" ("++show t++")"

mkDiffEqs :: [(EqnType, Ident, Expr)] -> DiffEqs
mkDiffEqs decls =
  let deqLets    = Map.fromList $ mapMaybe (match Let) decls
      deqRates   = Map.fromList $ mapMaybe (match Rate) decls
      deqInitial = Map.fromList $ mapMaybe (match Init) decls
      -- deqLets    = Map.difference lets deqRates 
      deqParams  = []
  in  DiffEqs{..}

  where
    match s (s',i,e) = if s == s' then Just (i,expAsCore e) else Nothing
}