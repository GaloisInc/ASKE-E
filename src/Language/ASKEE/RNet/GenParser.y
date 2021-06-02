{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.RNet.GenParser where

import           Data.Text ( Text )
import           Data.Map ( Map )
import qualified Data.Map as Map

import Language.ASKEE.Expr ( Expr(..) )
import Language.ASKEE.ESL.Lexer ( Located(..) )
import Language.ASKEE.RNet.Lexer as Lexer
import Language.ASKEE.RNet.Syntax as Syntax

}

%name        parseRNet
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
  ','        { Located _ _ Comma }
  '-->'      { Located _ _ Arrow }
  let        { Located _ _ Let }
  when       { Located _ _ When }
  nil        { Located _ _ Lexer.Nil }
  REAL       { Located _ _ (Lit $$) }
  INT        { Located _ _ (Int $$) }
  SYM        { Located _ _ (Sym $$) }

%left '+' '-'
%left '*' '/'


%%

ReactionNet                              :: { ReactionNet }
  : Bindings Reactions1                     { ReactionNet (Map.fromList $1) (reverse $2) }
        
Reactions1                               :: { [Reaction] }
  : Reaction                                { [$1] }
  | Reactions1 Reaction                     { $2 : $1 }
        
Reaction                                           :: { Reaction }
  : Exp          ',' ReactionExp '-->' ReactionExp    { Reaction $1 MassAction Nothing $3 $5 }
  | Exp when Exp ',' ReactionExp '-->' ReactionExp    { Reaction $1 MassAction (Just $3) $5 $7 }

ReactionExp                              :: { ReactionExp }
  : nil                                     { Syntax.Nil }
  | ReactionTerms1                          { ReactionTerms (reverse $1) }

ReactionTerms1                           :: { [ReactionTerm] }
  : ReactionTerm                            { [mkRTerm $1] }
  | ReactionTerms1 '+' ReactionTerm         { mkRTerm $3 : $1 }

ReactionTerm                             :: { (Int, Text) }
  : INT SYM                                 { ($1, $2) }
  | SYM                                     { (1, $1) }

Bindings                                 :: { [(Text, Expr)] }
  :                                         { [] }
  | Bindings Binding                        { $2 : $1 }
        
Binding                                  :: { (Text, Expr) }
  : let SYM '=' Exp                         { ($2, $4) }

Exp                                      :: { Expr }
  : Exp '+' Exp                             { Add  $1 $3 }
  | Exp '-' Exp                             { Sub  $1 $3 }
  | Exp '*' Exp                             { Mul  $1 $3 }
  | Exp '/' Exp                             { Div  $1 $3 }
  | '-' Exp                                 { Neg  $2 }
  | '(' Exp ')'                             { $2 }
  | SYM                                     { Var $1 }
  | REAL                                    { LitD $1 }

{
parseError :: [Located Token] -> Either String a
parseError [] = 
  Left $ "parse error at end of file"
parseError ((Located lin col t):ts) = 
  Left $ "parse error at line "++show lin++", col "++show col++" ("++show t++")"

mkRTerm :: (Int, Text) -> ReactionTerm
mkRTerm = uncurry ReactionTerm
}