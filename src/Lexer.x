{
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Lexer where

import Control.Monad (when)

import Data.Char (isSpace)

import Prelude hiding (LT,GT)

import System.Environment (getArgs)
}

%wrapper "monad"

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]
$ws    = [\ ]

@real    = $digit+ (\. $digit+)?
@ident   = [$upper $lower $digit _]
@lowerID = $lower @ident*
@upperID = $upper @ident*

tokens :-

<0>      "model" $ws+       { atomic Model      `andBegin` modelC }
<modelC> @upperID ":"       { identColon        `andBegin` 0 }

<0>      "state" $ws+       { atomic State      `andBegin` stateC }
<stateC> @upperID           { ident             `andBegin` 0 }

<0>      "let derived" $ws+ { atomic LetDerived `andBegin` letC }
<letC>   @lowerID           { ident             `andBegin` 0 }

<0>      "let" $ws+         { atomic Let        `andBegin` letC }
<letC>   @lowerID           { ident             `andBegin` 0 }

<0>      "event" $ws+       { atomic Event      `andBegin` eventC }
<eventC> @upperID ":"       { identColon        `andBegin` 0 }

<0> @upperID   { ident }
<0> @lowerID   { ident }
<0> @real      { real }
<0> "+="       { atomic PlusAssign }
<0> "-="       { atomic MinusAssign }
<0> "*="       { atomic TimesAssign }
<0> "/="       { atomic DivideAssign }
<0> "+"        { atomic Plus }
<0> "-"        { atomic Minus }
<0> "*"        { atomic Times }
<0> "/"        { atomic Divide }
<0> "="        { atomic Assign }
<0> "<="       { atomic LTE }
<0> ">="       { atomic GTE }
<0> "<"        { atomic LT }
<0> ">"        { atomic GT }
<0> "("        { atomic OpenP }
<0> ")"        { atomic CloseP }
<0> "not"      { atomic Not }
<0> "and"      { atomic And }
<0> "or"       { atomic Or }
<0> "if"       { atomic If }
<0> "then"     { atomic Then }
<0> "else"     { atomic Else }
<0> "elif"     { atomic Elif }
<0> "cond:"    { atomic Cond }
<0> "when:"    { atomic When }
<0> "rate:"    { atomic Rate }
<0> "effect:"  { atomic Effect }

<0> \n " "+    { indent }
<0> \n         { atomic Newline   -- better to skip this? }
<0> $ws+       ;
<0> "#".*      ;


{
-- TODO:
-- metadata
-- use codes for expressions to promote more errors to lex time?

type Action = AlexInput -> Int -> Alex Token

real :: Action
real (_,_,_,s) _ = (pure . Real . fst . head . reads) s

ident :: Action
ident (_,_,_,s) _ = (pure . Sym . takeWhile (not . isSpace)) s

identColon :: Action
identColon (_,_,_,s) _ = (pure . Sym . takeWhile (not . isSpace)) (init s)

atomic :: Token -> Action
atomic tok = \_ _ -> pure tok

indent :: Action
indent (_,_,_,('\n':s)) _ = let
  spaces = (length . takeWhile isSpace) s
  in pure $ Indent spaces
indent _ _ = alexError "impossible?"


test :: String -> Either String [Token]
test s = runAlex s go
  where
    go :: Alex [Token]
    go = do
      tok <- alexMonadScan
      case tok of
        EOF -> pure []
        _ -> ((<$>) . (:)) tok go

data Token = EOF
  | And
  | Assign
  | CloseP
  | Cond
  | Divide
  | DivideAssign
  | Effect
  | Elif
  | Else
  | Event
  | GT
  | GTE
  | If
  | Indent Int
  | LT
  | LTE
  | Let
  | LetDerived
  | Minus
  | MinusAssign
  | Model
  | Newline
  | Not
  | OpenP
  | Or
  | Plus
  | PlusAssign
  | Rate
  | Real Double
  | State
  | Sym String
  | Test AlexInput
  | Then
  | Times
  | TimesAssign
  | When
  deriving (Eq, Show)

alexEOF = pure EOF

main = do
  args <- getArgs
  when (length args /= 1) $
    error "usage: ./Lexer <file>"
  f <- readFile (args !! 0)
  case test f of
    Left s -> putStrLn s
    Right ts -> mapM print ts >> pure ()
}
