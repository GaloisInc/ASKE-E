{
{-# LANGUAGE TypeApplications #-}

module Language.ASKEE.RNet.GenLexer ( lexRNet, Token(..) ) where

import Data.Text ( pack )

import Language.ASKEE.ESL.Lexer   ( Located(..) )
import Language.ASKEE.RNet.Lexer  ( Token(..) )
}

%wrapper "monad"

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]

@exp       = e [\+\-]? $digit+
@real      = $digit+ (\. $digit+)? @exp?
@int       = $digit+
@identhead = [$upper $lower]
@identbody = [$upper $lower $digit _]
@ident     = @identhead @identbody*

tokens :-

"let"    { atomic Let }
"when"   { atomic When }
"nil"    { atomic Nil }
"+"      { atomic Plus }
"-"      { atomic Minus }
"*"      { atomic Times }
"/"      { atomic Divide }
"="      { atomic Assign }
"("      { atomic OpenP }
")"      { atomic CloseP }
","      { atomic Comma }
"-->"    { atomic Arrow }
@int     { int }
@real    { real }
@ident   { ident }

$white+  ;

{
{-
type Action = AlexPosn -> String -> Located Token
atomic :: Token -> Action
atomic t (AlexPn offset lin col) _ = Located lin col t
ident :: Action
ident (AlexPn offset lin col) str = (Located lin col . Sym . pack) str
real :: Action
real (AlexPn offset lin col) str = (Located lin col . Lit . read @Double) str
-}

type Action = AlexInput -> Int -> Alex Token

real :: Action
real (_,_,_,s) len = (pure . Lit . read . take len) s

int :: Action
int (_,_,_,s) len = (pure . Int . read . take len) s

ident :: Action
ident (_,_,_,s) len = (pure . Sym . pack . take len) s

atomic :: Token -> Action
atomic tok = \_ _ -> pure tok

getState :: Alex AlexState
getState = Alex (\s -> Right (s,s))

lexRNet :: String -> Either String [Located Token]
lexRNet s = runAlex s go
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