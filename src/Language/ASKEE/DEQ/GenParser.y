{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.DEQ.GenParser where

import qualified Data.Map as Map
import           Data.Maybe ( mapMaybe )
import           Data.Text ( Text )
import qualified Data.Text as T

import Language.ASKEE.Core.Syntax          ( Ident )
import Language.ASKEE.ESL.Convert          ( expAsCore )
import qualified Language.ASKEE.DEQ.Lexer  as Lexer
import Language.ASKEE.DEQ.Syntax           as Syntax
import Language.ASKEE.Expr                 as Expr
import Language.ASKEE.ESL.Lexer            ( Located(..) )
import Language.ASKEE.Panic                ( panic )
}

%name        parseDiffEqs
%tokentype { Located Lexer.Token }
%error     { parseError }
%monad     { Either String }

%token
  '+'        { Located _ _ Lexer.Plus }
  '-'        { Located _ _ Lexer.Minus }
  '*'        { Located _ _ Lexer.Times }
  '/'        { Located _ _ Lexer.Divide }
  '='        { Located _ _ Lexer.Assign }
  '('        { Located _ _ Lexer.OpenP }
  ')'        { Located _ _ Lexer.CloseP }
  if         { Located _ _ Lexer.If }
  then       { Located _ _ Lexer.Then }
  else       { Located _ _ Lexer.Else }
  '<'        { Located _ _ Lexer.LT }
  '<='       { Located _ _ Lexer.LTE }
  '=='       { Located _ _ Lexer.EQ }
  '>='       { Located _ _ Lexer.GTE }
  '>'        { Located _ _ Lexer.GT }
  and        { Located _ _ Lexer.And }
  or         { Located _ _ Lexer.Or }
  not        { Located _ _ Lexer.Not }
  true       { Located _ _ (Lexer.LitB $$) }
  false      { Located _ _ (Lexer.LitB $$) }
  let        { Located _ _ Lexer.Let }
--  var        { Located _ _ Lexer.Var }
  ddt        { Located _ _ Lexer.DDt }
  REAL       { Located _ _ (Lexer.LitD $$) }
  SYM        { Located _ _ (Lexer.Sym $$) }

%left or
%left and
%nonassoc '<' '<=' '==' '>=' '>'
%nonassoc if then else
%right not
%left '+' '-'
%left '*' '/'

%%

Eqns                :: { DiffEqs }
  : Eqns1              { mkDiffEqs $1 }

Eqns1               :: { [(EqnType, Ident, Expr)] }
  : Eqn                { [$1] }
  | Eqns1 Eqn          { $2 : $1}

Eqn                 :: { (EqnType, Ident, Expr) }
  : let SYM '=' Exp    { (Let, $2, $4) }
  | SYM '(' REAL ')' '=' Exp    {% mkVar $1 $3 $6 }
  | ddt SYM '=' Exp    { (Rate, $2, $4) }

Exp                 :: { Expr }
  : Exp '+' Exp        { Expr.Add $1 $3 }
  | Exp '-' Exp        { Expr.Sub $1 $3 }
  | Exp '*' Exp        { Expr.Mul $1 $3 }
  | Exp '/' Exp        { Expr.Div $1 $3 }
  | '-' Exp            { Expr.Neg $2 }
  | '(' Exp ')'        { $2 }
  | if Exp then Exp else Exp { Expr.If $2 $4 $6 }
  | Exp '<' Exp        { Expr.LT $1 $3 }
  | Exp '<=' Exp       { Expr.LTE $1 $3 }
  | Exp '==' Exp       { Expr.EQ $1 $3 }
  | Exp '>=' Exp       { Expr.GTE $1 $3 }
  | Exp '>' Exp        { Expr.GT $1 $3 }
  | Exp and Exp        { Expr.And $1 $3 }
  | Exp or Exp         { Expr.Or $1 $3 }
  | not Exp            { Expr.Not $2 }
  | true               { Expr.LitB $1 }
  | false              { Expr.LitB $1 }
  | SYM                { Expr.Var $1 }
  | REAL               { Expr.LitD $1 }

{
data EqnType = Let | Init | Rate
  deriving Eq

parseError :: [Located Lexer.Token] -> Either String a
parseError [] = 
  Left $ "parse error at end of file"
parseError ((Located lin col t):ts) = 
  Left $ "parse error at line "++show lin++", col "++show col++" ("++show t++")"

mkVar :: Ident -> Double -> Expr -> Either String (EqnType, Ident, Expr)
mkVar i d e
  | d == 0 = pure (Init, i, e)
  | otherwise = Left $ "initial value for "++i'++" not declared as "++i'++"(0)"
  
  where
    i' = T.unpack i

mkDiffEqs :: [(EqnType, Ident, Expr)] -> DiffEqs
mkDiffEqs decls =
  let deqLets    = Map.fromList $ mapMaybe (match Let)   decls
      deqInitial = Map.fromList $ mapMaybe (match Init) decls
      deqRates   = Map.fromList $ mapMaybe (match Rate)  decls
      deqParams  = []
  in  DiffEqs{..}

  where
    match s (s',i,e) = if s == s' then Just (i,expAsCore e) else Nothing

}