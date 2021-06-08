{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Language.ASKEE.Latex.GenLexer ( lexLatex, Token(..) ) where

import Data.Text ( Text, pack )

import Language.ASKEE.ESL.Lexer    ( Located(..) )
import Language.ASKEE.Latex.Lexer  ( Token(..) )
}

%wrapper "monad"

$bs = \\
$nl = \n
$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]

@identhead = [$upper $lower]
@identbody = [$upper $lower $digit _]
@ident     = @identhead @identbody*
@real      = $digit+ (\. $digit+)?
@ws        = $white # $nl

tokens :-

"{"              { atomic OpenBrace }
"}"              { atomic CloseBrace }
"("              { atomic OpenParen }
")"              { atomic CloseParen }
"["              { atomic OpenParen }
"]"              { atomic CloseParen }
"+"              { atomic Plus }
"-"              { atomic Minus }
-- "*"              { atomic Times }
"/"              { atomic Divide }  -- dubiously necessary...
"="              { atomic Eq }
$bs "frac"       { atomic Frac }
$bs @ident       { escaped Sym }
@real            { real }
@ident           { ident }

@ws+             ;
"#".*            ;
"&"              ;
$bs              ;
$bs "begin" .*   ;
$bs "end" .*     ;
$bs "label" .*   ;

$nl+             { atomic Newline }

{
type Action = AlexInput -> Int -> Alex Token

real :: Action
real (_,_,_,s) len = (pure . Lit . read . take len) s

ident :: Action
ident (_,_,_,s) len = (pure . Sym . pack . take len) s

atomic :: Token -> Action
atomic tok = \_ _ -> pure tok

escaped :: (Text -> Token) -> Action
escaped t (_,_,_,s) len = (pure . t . pack . tail . take len) s


getState :: Alex AlexState
getState = Alex (\s -> Right (s,s))

lexLatex :: String -> Either String [Located Token]
lexLatex s = simplifyTimes <$> stripLeading Newline <$> stripTrailing Newline <$> runAlex s go
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

  simplifyTimes :: [Located Token] -> [Located Token]
  simplifyTimes [] = []
  simplifyTimes (Located l c (Sym sym) : Located _ _ OpenParen : Located _ _ (Sym "t") : Located _ _ CloseParen : ts) =
    Located l c (SymTime sym) : simplifyTimes ts
  simplifyTimes (Located l c (Sym sym) : Located _ _ OpenParen : Located _ _ (Lit 0) : Located _ _ CloseParen : ts) =
    Located l c (SymInit sym) : simplifyTimes ts
  simplifyTimes (t:ts) = t : simplifyTimes ts

  stripLeading :: Token -> [Located Token] -> [Located Token]
  stripLeading x = dropWhile (\t -> locVal t == x)

  stripTrailing :: Token -> [Located Token] -> [Located Token]
  stripTrailing x = (reverse . stripLeading x . reverse)

alexEOF = pure EOF
}