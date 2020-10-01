{
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Language.ASKEE.GenLexer where

import Control.Monad (when)

import Data.Char (isSpace)

import qualified Data.Text as Text

import Prelude hiding (LT,GT)

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



<0> "model"     { atomic Model }
<0> "state"     { atomic State }
<0> "let"       { atomic Let }
<0> "event"     { atomic Event }
<0> @real       { real }
<0> "+="        { atomic PlusAssign }
<0> "-="        { atomic MinusAssign }
<0> "*="        { atomic TimesAssign }
<0> "/="        { atomic DivideAssign }
<0> "+"         { atomic Plus }
<0> "-"         { atomic Minus }
<0> "*"         { atomic Times }
<0> "/"         { atomic Divide }
<0> "="         { atomic Assign }
<0> "<="        { atomic LTE }
<0> ">="        { atomic GTE }
<0> "<"         { atomic LT }
<0> ">"         { atomic GT }
<0> "("         { atomic OpenP }
<0> ")"         { atomic CloseP }
<0> ":"         { atomic Colon }
<0> "not"       { atomic Not }
<0> "and"       { atomic And }
<0> "or"        { atomic Or }
<0> "if"        { atomic If }
<0> "then"      { atomic Then }
<0> "else"      { atomic Else }
<0> "elif"      { atomic Elif }
<0> "otherwise" { atomic Otherwise }
<0> "metadata"  { atomic Metadata }
<0> "cond"      { atomic Cond }
<0> "when"      { atomic When }
<0> "rate"      { atomic Rate }
<0> "effect"    { atomic Effect }

<0> @upperID    { ident }
<0> @lowerID    { ident }

<0> \n $ws*    { atomic Newline }
<0> $ws+       ;
<0> "#".*      ;


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
lexModel s = runAlex s go >>= insertLayoutTokens
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
