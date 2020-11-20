{
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Language.ASKEE.GenLexer where


import qualified Data.Text as Text

import Prelude hiding (LT,GT,EQ)
import Language.ASKEE.Lexer

}

%wrapper "monad"

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]
$ws    = [\ ]

@real    = $digit+ (\. $digit+)?
@ident   = [A-Za-z0-9_]
@lowerID = $lower @ident*
@upperID = $upper @ident*

tokens :-



"model"     { atomic Model }
"state"     { atomic State }
"let"       { atomic Let }
"event"     { atomic Event }
@real       { real }
"+="        { atomic PlusAssign }
"-="        { atomic MinusAssign }
"*="        { atomic TimesAssign }
"/="        { atomic DivideAssign }
"+"         { atomic Plus }
"-"         { atomic Minus }
"*"         { atomic Times }
"/"         { atomic Divide }
"="         { atomic Assign }
"<="        { atomic LTE }
">="        { atomic GTE }
"<"         { atomic LT }
">"         { atomic GT }
"=="        { atomic EQ }
"("         { atomic OpenP }
")"         { atomic CloseP }
":"         { atomic Colon }
"not"       { atomic Not }
"and"       { atomic And }
"or"        { atomic Or }
"if"        { atomic If }
"then"      { atomic Then }
"else"      { atomic Else }
"elif"      { atomic Elif }
"otherwise" { atomic Otherwise }
"metadata"  { atomic Metadata }
"cond"      { atomic Cond }
"when"      { atomic When }
"rate"      { atomic Rate }
"effect"    { atomic Effect }
"assert"    { atomic Assert }

@upperID    { ident }
@lowerID    { ident }

-- We can't seem to skip these as the locations of the other tokens
-- get messed up.
$white+     { atomic Whitespace }
"#".*       { atomic Whitespace }


{
-- TODO:
-- metadata
-- use codes for expressions to promote more errors to lex time?

type Action = AlexInput -> Int -> Alex Token

real :: Action
real (_,_,_,s) len = (pure . Real . read . take len) s

ident :: Action
ident (_,_,_,s) len = (pure . Sym . Text.pack . take len) s

isIdentChar c = c `elem` (['a'..'z']++['A'..'Z']++['0'..'9']++['_'])

atomic :: Token -> Action
atomic tok = \_ _ -> pure tok

getState :: Alex AlexState
getState = Alex (\s -> Right (s,s))

lexModel :: String -> Either String [Located Token]
lexModel s = addLayout <$> runAlex s go
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
