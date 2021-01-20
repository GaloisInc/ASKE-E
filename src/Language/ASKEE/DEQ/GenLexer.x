{
{-# LANGUAGE TypeApplications #-}

module Language.ASKEE.DEQ.GenLexer ( lexDEQs, Token(..) ) where

import Data.Text ( pack )

import Language.ASKEE.Lexer      ( Located(..) )
import Language.ASKEE.DEQ.Lexer  ( Token(..) )

import Prelude hiding (LT, EQ, GT)
}

%wrapper "monad"

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]
-- $ws    = [\ ]

@exp       = e [\+\-]? $digit+
@real      = $digit+ (\. $digit+)? @exp?
@identhead = [$upper $lower]
@identbody = [$upper $lower $digit _]
@ident     = @identhead @identbody*
-- @lowerID = $lower @ident*
-- @upperID = $upper @ident*

tokens :-

"let"    { atomic Let }
-- "var"    { atomic Var }
"d/dt"   { atomic DDt }
"+"      { atomic Plus }
"-"      { atomic Minus }
"*"      { atomic Times }
"/"      { atomic Divide }
"="      { atomic Assign }
"<="     { atomic LTE }
">="     { atomic GTE }
"<"      { atomic LT }
">"      { atomic GT }
"=="     { atomic EQ }
"("      { atomic OpenP }
")"      { atomic CloseP }
"not"    { atomic Not }
"and"    { atomic And }
"or"     { atomic Or }
"if"     { atomic If }
"then"   { atomic Then }
"else"   { atomic Else }
"true"   { bool True }
"false"  { bool False }
@real    { real          }
@ident   { ident         }

$white+  ;
"#".*    ;

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
real (_,_,_,s) len = (pure . LitD . read . take len) s

ident :: Action
ident (_,_,_,s) len = (pure . Sym . pack . take len) s

bool :: Bool -> Action
bool b _ _ = pure $ LitB b

atomic :: Token -> Action
atomic tok = \_ _ -> pure tok

getState :: Alex AlexState
getState = Alex (\s -> Right (s,s))

lexDEQs :: String -> Either String [Located Token]
lexDEQs s = runAlex s go
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