{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.DEQ.GenParser where

import qualified Data.Map as Map
import           Data.Maybe ( mapMaybe )
import           Data.Text ( Text )
import qualified Data.Text as T

import Language.ASKEE.Core             ( Ident )
import Language.ASKEE.Core.ImportASKEE ( expAsCore )
import Language.ASKEE.DEQ.Lexer        as Lexer
import Language.ASKEE.DEQ.Syntax       as Syntax
import Language.ASKEE.Expr             as Expr
import Language.ASKEE.Lexer            ( Located(..) )
}

%name        parseDEQs
%tokentype { Located Token}
%error     { parseError }
%monad     { Either String }

%token
  '+'        { Located _ _ Plus }
  '-'        { Located _ _ Minus }
  '*'        { Located _ _ Times }
  '/'        { Located _ _ Divide }
  '='        { Located _ _ Assign }
  '('        { Located _ _ OpenP }
  ')'        { Located _ _ CloseP }
  let        { Located _ _ Let }
  var        { Located _ _ Lexer.Var }
  ddt        { Located _ _ DDt }
  REAL       { Located _ _ (Lit $$) }
  SYM        { Located _ _ (Sym $$) }

%left '+' '-'
%left '*' '/'
-- %left var

%%

Eqns                :: { DiffEqs }
  : Eqns1              { mkDiffEqs $1 }

Eqns1               :: { [(String, Ident, Expr)] }
  : Eqn                { [$1] }
  | Eqns1 Eqn          { $2 : $1}

Eqn                 :: { (String, Ident, Expr) }
  : let SYM '=' Exp    { ("let", $2, $4) }
  | var SYM '=' Exp    { ("var", $2, $4) }
  | ddt SYM '=' Exp    { ("d/dt", $2, $4) }

Exp                 :: { Expr }
  : Exp '+' Exp        { Expr.Add $1 $3 }
  | Exp '-' Exp        { Expr.Sub $1 $3 }
  | Exp '*' Exp        { Expr.Mul $1 $3 }
  | Exp '/' Exp        { Expr.Div $1 $3 }
  | '-' Exp            { Expr.Neg $2 }
  | '(' Exp ')'        { $2 }
  | SYM                { Expr.Var $1 }
  | REAL               { Expr.LitD $1 }

{
parseError :: [Located Token] -> Either String a
parseError [] = 
  fail $ "parse error at end of file"
parseError ((Located lin col t):ts) = 
  fail $ "parse error at line "++show lin++", col "++show col++" ("++show t++")"

mkDiffEqs :: [(String, Ident, Expr)] -> DiffEqs
mkDiffEqs decls =
  let deqLets    = Map.fromList $ mapMaybe (match "let")   decls
      deqInitial = Map.fromList $ mapMaybe (match "var") decls
      deqRates   = Map.fromList $ mapMaybe (match "d/dt")  decls
      deqParams  = []
  in  DiffEqs{..}

  where
    match s (s',i,e) = if s == s' then Just (i,expAsCore e) else Nothing

}