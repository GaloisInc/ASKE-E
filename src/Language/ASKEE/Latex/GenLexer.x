{
{-# LANGUAGE TypeApplications #-}

module Language.ASKEE.Latex.GenLexer ( lexLatex, Token(..) ) where

import Data.Text ( Text, pack )

import Language.ASKEE.Lexer        ( Located(..) )
import Language.ASKEE.Latex.Lexer  ( Token(..) )
}

%wrapper "monad"

$bs = \\
$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]

@identhead = [$upper $lower]
@identbody = [$upper $lower $digit _]
@ident     = @identhead @identbody*
@real      = $digit+ (\. $digit+)?

tokens :-

"{"              { atomic OpenBrace }
"}"              { atomic CloseBrace }
"("              { atomic OpenParen }
")"              { atomic CloseParen }
"["              { atomic OpenParen }
"]"              { atomic CloseParen }
"+"              { atomic Plus }
"-"              { atomic Minus }
"*"              { atomic Times }
"/"              { atomic Divide }  -- probably unnecessary?
"="              { atomic Eq }
$bs "frac"       { atomic Frac }
$bs @ident       { escaped Sym }
@real            { real }
@ident           { ident }

$white+          ;
"#".*            ;


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
lexLatex s = runAlex s go
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