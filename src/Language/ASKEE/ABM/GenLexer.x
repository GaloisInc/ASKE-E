{
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Language.ASKEE.ABM.GenLexer ( lexABM, Token(..) ) where


import qualified Data.Text as Text

import Prelude hiding (LT,GT,EQ)
import Language.ASKEE.ABM.Lexer

}

%wrapper "monad"

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]
$ws    = [\ ]

@exp     = e [\+\-]? $digit+
@real    = $digit+ (\. $digit+)? @exp?
@ident   = [A-Za-z0-9_]
@lowerID = $lower @ident*
@upperID = $upper @ident*
@qID     = @ident+ (\. @ident+)*

tokens :-
"agent"                 { atomic Agent }
"effect"                { atomic Effect }
"event"                 { atomic Event }
"init"                  { atomic Init }
"let"                   { atomic Let }
"model"                 { atomic Model }
"rate"                  { atomic Rate }
"status"                { atomic Status }
"when"                  { atomic When }
"mingling"              { atomic Mingling }
"nonmingling"           { atomic Nonmingling }
"+"                     { atomic Add }
"-"                     { atomic Sub }
"*"                     { atomic Mul }
"/"                     { atomic Div }
"and"                   { atomic And }
"or"                    { atomic Or }
"not"                   { atomic Not }
">"                     { atomic GT }
">="                    { atomic GTE }
"=="                    { atomic EQ }
"<="                    { atomic LTE }
"<"                     { atomic LT }
"if"                    { atomic If }
"then"                  { atomic Then }
"else"                  { atomic Else }
"true"                  { atomic (Boolean True) }
"false"                 { atomic (Boolean False) }
@real                   { real }
-- "size"                  { atomic Size }
"="                     { atomic Assign }
"<-"                    { atomic Bind }
@upperID                { ident }
@lowerID                { ident }
-- @qID                    { qident }
"."                     { atomic Dot }
","                     { atomic Comma }
":"                     { atomic Colon }
"::"                    { atomic DoubleColon }
"("                     { atomic OpenParen }
")"                     { atomic CloseParen }

-- "elif"      { atomic Elif }
-- "otherwise" { atomic Otherwise }
-- "metadata"  { atomic Metadata }
-- "cond"      { atomic Cond }
-- "log"       { atomic Log }
-- "exp"       { atomic Exp }


-- We can't seem to skip these as the locations of the other tokens
-- get messed up.
$white+     { atomic Whitespace }
"#".*       { atomic Whitespace }


{
type Action = AlexInput -> Int -> Alex Token

real :: Action
real (_,_,_,s) len = 
  (pure . Real . read . take len) s

ident :: Action
ident (_,_,_,s) len = 
  (pure . Sym . Text.pack . take len) s

qident :: Action
qident (_,_,_,s) len = 
  (pure . QSym . Text.split (== '.') . Text.pack . take len) s

isIdentChar c = 
  c `elem` (['a'..'z']++['A'..'Z']++['0'..'9']++['_'])

atomic :: Token -> Action
atomic tok = \_ _ -> pure tok

getState :: Alex AlexState
getState = Alex (\s -> Right (s,s))

lexABM :: String -> Either String [Located Token]
lexABM s = addLayout <$> runAlex s go
  where
  go :: Alex [Located Token]
  go =
    do AlexPn _ line col <- alex_pos <$> getState
       tok <- alexMonadScan
       case tok of
         EOF -> pure []
         t ->
           do rest <- go
              pure (Located { locLine = line, locCol = col, locVal = t } : rest)

alexEOF = pure EOF
}
