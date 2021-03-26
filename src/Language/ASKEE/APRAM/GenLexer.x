{
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Language.ASKEE.APRAM.GenLexer where

import Prelude hiding (LT,GT,EQ)
import Language.ASKEE.APRAM.Lexer

}

%wrapper "monad"

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]
$ws    = [\ ]
$graphic = $printable # $white
-- $quote = \" | \'


@exp     = e [\+\-]? $digit+
@real    = $digit+ (\. $digit+)? @exp?
@ident   = [A-Za-z0-9_]+
@lowerID = $lower @ident*
@upperID = $upper @ident*
@string  = (\" ($graphic # \")* \") | (\' ($graphic # \')* \')

tokens :-

@real               { real }
"+"                 { atomic Plus }
"-"                 { atomic Minus }
"*"                 { atomic Times }
"/"                 { atomic Divide }
"="                 { atomic Assign }
"<="                { atomic LTE }
">="                { atomic GTE }
"<"                 { atomic LT }
">"                 { atomic GT }
"=="                { atomic EQ }

"("                 { atomic OpenP }
")"                 { atomic CloseP }
"["                 { atomic OpenB }
"]"                 { atomic CloseB }

":"                 { atomic Colon }
","                 { atomic Comma }

"lambda"            { atomic Lambda }
"not"               { atomic Not }
"and"               { atomic And }
"or"                { atomic Or }
"if"                { atomic If }
"then"              { atomic Then }
"else"              { atomic Else }
"True"              { bool True }
"False"             { bool False }
"for"               { atomic For }
"in"                { atomic In }
"return"            { atomic Return }

"math.log"          { atomic Log }
"math.exp"          { atomic Exp }
"pop.size"          { atomic Size }
"pop.reset()"       ;
"sim.reset()"       ;

-- "pop.make_column"   { atomic MakeColumn }
-- "pop.make_param"    { atomic MakeParam }
-- "pop.make_cohort"   { atomic MakeCohort }
-- "pop." @ident ".eq" { cohortRef Eq }

"pop" { atomic Pop }
"."   { atomic Dot }
"sim" { atomic Sim }

"make_column"   { atomic MakeColumn }
"make_param"    { atomic MakeParam }
"make_cohort"   { atomic MakeCohort }
"**" .* ")"     { atomic CloseP }

@string    { str }        

@ident  { ident }

("from" .*)? "import" .* ("as" .*)? ;

$white+     ;



{
type Action = AlexInput -> Int -> Alex Token

real :: Action
real (_,_,_,s) len = (pure . LitD . read . take len) s

ident :: Action
ident (_,_,_,s) len = (pure . Sym . take len) s

bool :: Bool -> Action
bool b = \_ _ -> pure $ LitB b

str :: Action
str (_,_,_,s) len = (pure . LitS . init . tail . take len) s

cohortRef :: LogOp -> Action
cohortRef op (_,_,_,s) len =
  let res =
        do  ("pop", a) <- lex (take len s)
            (".", b) <- lex a
            (cohort, _) <- lex b
            pure cohort
  in  case res of
        [c] -> pure $ CohortExpr c op
        r -> alexError $ "couldn't lex cohort reference \""<>s<>"\" ("<>show r<>")"

atomic :: Token -> Action
atomic tok = \_ _ -> pure tok

getState :: Alex AlexState
getState = Alex (\s -> Right (s,s))

lexAPRAM :: String -> Either String [Located Token]
lexAPRAM s = runAlex s go
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
