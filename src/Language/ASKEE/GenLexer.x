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

@exp     = e [\+\-]? $digit+
@real    = $digit+ (\. $digit+)? @exp?
@ident   = [A-Za-z0-9_]
@lowerID = $lower @ident*
@upperID = $upper @ident*
@metakeychar = [^\-\:]|("-"[^\-])|("--"[^\]])
@metavaluechar =  [^\-]|("-"[^\-])|("--"[^\]])
-- non empty/whitespace metadata strings
@metakeystring = @metakeychar*[^\ ]@metakeychar*
@metavaluestring = @metavaluechar*[^\ ]@metavaluechar*
@meta    = "[--" @metakeystring ":" @metavaluestring "--]"

tokens :-



"model"     { atomic Model }
"state"     { atomic State }
"let"       { atomic Let }
"event"     { atomic Event }
@real       { real }
@meta       { meta }
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
"true"      { bool True }
"false"     { bool False }
"log"       { atomic Log }
"exp"       { atomic Exp }
"parameter" { atomic Parameter }

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

meta :: Action
meta (_,_,_,s) len = pure $ Meta (Text.pack $ strip key, Text.pack $ strip val)
  where
    mdTok = take len s
    mdTokStripped = bothEnds (drop 3) mdTok
    (key, _:val) = break ((==) ':') mdTokStripped
    strip =    bothEnds (dropWhile ((==) ' '))
    bothEnds f = reverse . f . reverse . f

real :: Action
real (_,_,_,s) len = (pure . Real . read . take len) s

ident :: Action
ident (_,_,_,s) len = (pure . Sym . Text.pack . take len) s

bool :: Bool -> Action
bool b = \_ _ -> pure $ Boolean b

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
