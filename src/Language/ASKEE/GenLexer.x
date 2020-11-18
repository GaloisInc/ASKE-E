{
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Language.ASKEE.GenLexer where

import Control.Monad (when)

import Data.Char (isSpace)

import qualified Data.Text as Text

import Prelude hiding (LT,GT,EQ)

import System.Environment (getArgs)
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

-- \n $ws*    { atomic Newline }
$white+    { atomic Whitespace }
"#".*      ;


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
lexModel s = doLayout <$> runAlex s go
  where
    go :: Alex [Located Token]
    go = do
      AlexPn _ line col <- alex_pos <$> getState
      tok <- alexMonadScan
      case tok of
        EOF -> pure []
        t ->
          do  let located = Located { locLine = line
                                    , locCol = col
                                    , locVal = t
                                    }
              ((<$>) . (:)) located go

alexEOF = pure EOF

-- main = do
--   args <- getArgs
--   when (length args /= 1) $
--     error "usage: ./Lexer <file>"
--   f <- readFile (args !! 0)
--   case test f of
--     Left s -> putStrLn s
--     Right ts -> mapM print ts >> pure ()

{-
<0>      "model" $ws+       { atomic Model      `andBegin` modelC }
<modelC> @upperID           { ident             `andBegin` 0 }

<0>      "state" $ws+       { atomic State      `andBegin` stateC }
<stateC> @upperID           { ident             `andBegin` 0 }

<0>      "let derived" $ws+ { atomic LetDerived `andBegin` letC }
<letC>   @lowerID           { ident             `andBegin` 0 }

<0>      "let" $ws+         { atomic Let        `andBegin` letC }
<letC>   @lowerID           { ident             `andBegin` 0 }

<0>      "event" $ws+       { atomic Event      `andBegin` eventC }
<eventC> @upperID           { ident             `andBegin` 0 }
-}
}
