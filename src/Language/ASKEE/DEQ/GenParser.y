{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.DEQ.GenParser where

import           Data.Bifunctor ( Bifunctor(..) )
import qualified Data.Map as Map
import           Data.Maybe ( mapMaybe )
import           Data.Text ( Text )
import qualified Data.Text as T

import Language.ASKEE.Core.Expr            ( Ident )
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
  parameter  { Located _ _ Lexer.Parameter }
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

Eqns1               :: { [EqnDecl EqnType] }
  : Eqn                { [$1] }
  | Eqns1 Eqn          { $2 : $1}

Eqn                       :: { EqnDecl EqnType }
  : let SYM '=' Exp          { ($2, Let $4) }
  | parameter SYM '=' Exp    { ($2, Parameter (Just $4)) }
  | parameter SYM            { ($2, Parameter Nothing) }
  | SYM '(' REAL ')' '=' Exp {% mkVar $1 $3 $6 }
  | ddt SYM '=' Exp          { ($2, Rate $4) }

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
data EqnType
  = Let Expr
  | Init Expr
  | Rate Expr
  | Parameter (Maybe Expr)
  deriving Eq

type EqnDecl rhs = (Ident, rhs)

partitionEqnDecls :: [EqnDecl EqnType]
                  -> ( [EqnDecl Expr]         -- let
                     , [EqnDecl Expr]         -- init
                     , [EqnDecl Expr]         -- rate
                     , [EqnDecl (Maybe Expr)] -- parameter
                     )
partitionEqnDecls [] = ([], [], [], [])
partitionEqnDecls ((i, eqnType):decls) =
  let (lets, inits, rates, parameters) = partitionEqnDecls decls in
  case eqnType of
    Let e         -> ((i, e):lets,        inits,        rates,          parameters)
    Init e        -> (       lets, (i, e):inits,        rates,          parameters)
    Rate e        -> (       lets,        inits, (i, e):rates,          parameters)
    Parameter mbE -> (       lets,        inits,        rates, (i, mbE):parameters)

parseError :: [Located Lexer.Token] -> Either String a
parseError [] =
  Left $ "parse error at end of file"
parseError ((Located lin col t):ts) =
  Left $ "parse error at line "++show lin++", col "++show col++" ("++show t++")"

mkVar :: Ident -> Double -> Expr -> Either String (EqnDecl EqnType)
mkVar i d e
  | d == 0 = pure (i, Init e)
  | otherwise = Left $ "initial value for "++i'++" not declared as "++i'++"(0)"

  where
    i' = T.unpack i

mkDiffEqs :: [EqnDecl EqnType] -> DiffEqs
mkDiffEqs decls =
  let (lets, inits, rates, parameters) = partitionEqnDecls decls
      deqLets    = Map.fromList $ map (second expAsCore)        lets
      deqInitial = Map.fromList $ map (second expAsCore)        inits
      deqRates   = Map.fromList $ map (second expAsCore)        rates
      deqParams  = Map.fromList $ map (second (fmap expAsCore)) parameters
  in  DiffEqs{..}
}
