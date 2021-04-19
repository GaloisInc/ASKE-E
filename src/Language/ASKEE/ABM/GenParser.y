{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ABM.GenParser where

import           Data.Text ( Text )
import           Data.Map  ( Map )
import qualified Data.Map  as Map

-- import           Language.ASKEE.Core             ( Ident, substExpr )
-- import           Language.ASKEE.Core.ImportASKEE ( expAsCore )
-- import           Language.ASKEE.DEQ.Syntax       ( DiffEqs(..) )
import           Language.ASKEE.Expr         as Expr
import           Language.ASKEE.ABM.Lexer    ( Located(..) )
import qualified Language.ASKEE.ABM.Lexer    as Lexer
import           Language.ASKEE.ABM.Syntax   as Syntax

import Prelude hiding (Ordering(..))

import Debug.Trace
}

%name      parseABM Model
%name      parseExpr Exp
%tokentype { Located Lexer.Token }
%error     { parseError }
%monad     { Either String }

%token
agent           { Located _ _ Lexer.Agent }
effect          { Located _ _ Lexer.Effect }
event           { Located _ _ Lexer.Event }
initt            { Located _ _ Lexer.Init }
let             { Located _ _ Lexer.Let }
model           { Located _ _ Lexer.Model }
rate            { Located _ _ Lexer.Rate }
status          { Located _ _ Lexer.Status }
when            { Located _ _ Lexer.When }

'+'             { Located _ _ Lexer.Add }
'-'             { Located _ _ Lexer.Sub }
'*'             { Located _ _ Lexer.Mul }
'/'             { Located _ _ Lexer.Div }

and             { Located _ _ Lexer.And }
or              { Located _ _ Lexer.Or }
not             { Located _ _ Lexer.Not }
'>'             { Located _ _ Lexer.GT }
'>='            { Located _ _ Lexer.GTE }
'=='            { Located _ _ Lexer.EQ }
'<='            { Located _ _ Lexer.LTE }
'<'             { Located _ _ Lexer.LT }
if              { Located _ _ Lexer.If }
then            { Located _ _ Lexer.Then }
else            { Located _ _ Lexer.Else }
true            { Located _ _ (Lexer.Boolean True)}
false           { Located _ _ (Lexer.Boolean False)}
REAL            { Located _ _ (Lexer.Real $$) }
-- size            { Located _ _ Lexer.Size }
SYM             { Located _ _ (Lexer.Sym $$) }

'='             { Located _ _ Lexer.Assign }
'<-'            { Located _ _ Lexer.Bind }

'.'             { Located _ _ Lexer.Dot }
','             { Located _ _ Lexer.Comma }
':'             { Located _ _ Lexer.Colon }
'::'            { Located _ _ Lexer.DoubleColon }
'('             { Located _ _ Lexer.OpenParen }
')'             { Located _ _ Lexer.CloseParen }
BOPEN           { Located _ _ Lexer.OpenBlock }
BSEP            { Located _ _ Lexer.BlockSeparator }
BCLOSE          { Located _ _ Lexer.CloseBlock }

%left or
%left and
%nonassoc '<' '<=' '==' '>=' '>'
%nonassoc if then else
%right not
%left '+' '-'
%left '*' '/'

%%

REV1(p)
   : p                 { [$1] }
   | REV1(p) p         { $2 : $1 }

MANY1(p)
   : REV1(p)           { reverse $1 }

REV1SEP(p,s)
  : p                  { [$1] }
  | REV1SEP(p,s) s p   { $3 : $1 }

MANY1SEP(p,s)
  : REV1SEP(p,s)       { reverse $1 }

Model                              
  : StatusDecls AgentDecl ModelDecl     { mkModel ($1, $2, $3) }

StatusDecls                          :: { [StatusDecl] }
  : MANY1(StatusDecl)                   { $1 }

StatusDecl                           :: { StatusDecl }
  : status SYM ':' BOPEN
      MANY1SEP(SYM,BSEP)
    BCLOSE                              { StatusDecl $2 $5 }

AgentDecl                            :: { AgentDecl }
  : agent SYM ':' BOPEN
      MANY1SEP(AgentAttr,BSEP)
    BCLOSE                              { AgentDecl $2 $5 }

AgentAttr                            :: { (Text, Text) }
  : SYM '::' SYM                        { ($1, $3) }

ModelDecl                            :: { (Text, [(Text, Expr)], [(Text, Expr)], [Event]) }
  : model SYM ':' BOPEN
      LetDecls
      InitDecl
      EventDecls
    BCLOSE                              { ($2, $5, $6, $7) }

LetDecls                             :: { [(Text, Expr)] }
  : MANY1(LetDecl)                      { $1 }

LetDecl                              :: { (Text, Expr) }
  : let SYM '=' Exp BSEP                { ($2, $4) }

InitDecl                             :: { [(Text, Expr)] }
  : initt ':' BOPEN
      SYM '<-' Exp
    BCLOSE BSEP                         { [($4, $6)] }

BindStatement                        :: { (Text, Expr) }
  : SYM '<-' Exp                        { ($1, $3) }

EventDecls                           :: { [Event] }
  : MANY1SEP(EventDecl,BSEP)            { $1 }

EventDecl                            :: { Event }
  : event SYM '(' EventArgs ')' ':' BOPEN
      when ':' AExp 
      BSEP
      rate ':' Exp 
      BSEP
      effect ':' BOPEN
        Assigns
      BCLOSE
    BCLOSE                              { Event $2 $4 $10 $14 $19 }

EventArgs                            :: { [Text] }
  : MANY1SEP(SYM,',')                   { $1 }

AExp                                 :: { AgentExpr }
  : AttrRef '==' AttrRef                { Eq $1 $3 }
--  | size '(' AttrRef ')'                { Size $3 }
  | AExp and AExp                       { Syntax.And $1 $3 }
  | AExp or AExp                        { Syntax.Or $1 $3 }

AttrRef                              :: { AttributeRef }
  : SYM                                 { Status $1 }
  | SYM '.' SYM                         { Attribute $1 $3 }

Assigns                              :: { [AgentAssign] }
  : MANY1SEP(Assign,BSEP)               { $1 }

Assign                               :: { AgentAssign }
  : AttrRef '=' AttrRef                 { AgentAssign $1 $3 }

Exp                                  :: { Expr }
  : Exp '+' Exp                         { Add $1 $3 }
  | Exp '-' Exp                         { Sub $1 $3 }
  | Exp '*' Exp                         { Mul $1 $3 }
  | Exp '/' Exp                         { Div $1 $3 }
  | '-' Exp                             { Neg $2 }
  | Exp '>' Exp                         { GT  $1 $3 }
  | Exp '>=' Exp                        { GTE $1 $3 }
  | Exp '==' Exp                        { EQ  $1 $3 }
  | Exp '<=' Exp                        { LTE $1 $3 }
  | Exp '<' Exp                         { LT  $1 $3 }
  | Exp and Exp                         { Expr.And $1 $3 }
  | Exp or  Exp                         { Expr.Or  $1 $3 } 
  | not Exp                             { Not $2 }
  | '(' Exp ')'                         { $2 }
  | SYM '(' MANY1SEP(Exp,',') ')'       { Fn $1 $3 }
  | SYM                                 { Var $1 }
  | SYM '.' SYM                         { Var ($1<>"."<>$3) }
  | REAL                                { LitD $1 }
  | true                                { LitB True }
  | false                               { LitB False }
  | if Exp then Exp else Exp            { If $2 $4 $6 }
--  | CondExp                              { $1 }
-- 
-- CondExp                               :: { Expr }
--   : 'cond' ':' BOPEN Condition BCLOSE    { $4 }
-- 
-- Condition                             :: { Expr }
--   : CondChoices                          { Cond (reverse $1) Nothing }
--   | CondChoices BSEP Exp 'otherwise'     { Cond (reverse $1) (Just $3) }
-- 
-- CondChoices
--   : CondChoice                           { [$1] }
--   | CondChoices BSEP CondChoice          { $3 : $1 }
-- 
-- CondChoice                            :: { (Expr,Expr) }
--   : Exp 'if' Exp                         { ($1, $3) }


{
parseError :: [Located Lexer.Token] -> Either String a
parseError []     = Left $ "parse error at end of file"
parseError (t:ts) = Left $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t) ++ " (" ++ show t ++ ")"


data StatusDecl = StatusDecl
  { statusName :: Text
  , statusValues :: [Text] 
  }
  deriving (Eq, Show)

data AgentDecl = AgentDecl
  { agentName :: Text
  , agentAttributes :: [(Text, Text)]
  }
  deriving (Eq, Show)

mkModel :: ([StatusDecl], AgentDecl, (Text, [(Text, Expr)], [(Text, Expr)], [Event])) -> Model
mkModel (statuses, AgentDecl{..}, (modelName, modelLets, modelInit, modelEvents)) = Model{..}
  where
    statusMap = foldr (uncurry Map.insert) Map.empty 
      [ (n, vs) 
      | StatusDecl n vs <- statuses ]
    modelAgent =
      [ AgentAttribute attrName (statusMap Map.! attrType) 
      | (attrName, attrType) <- agentAttributes ]
}



