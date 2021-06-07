{
module Language.ASKEE.Jupyter.GenLexer where

import qualified Data.Text as T

import Language.ASKEE.ESL.Lexer (Located(..))
import Language.ASKEE.Jupyter.Lexer
}

%wrapper "monad"

$upper   = [A-Z]
$lower   = [a-z]
$digit   = [0-9]
$graphic = $printable # $white

@string    = \" ($graphic # \")* \"
@exp       = e [\+\-]? $digit+
@real      = $digit+ (\. $digit+)? @exp?
@identhead = [$upper $lower _]
@identbody = [$upper $lower $digit _]
@ident     = @identhead @identbody*

tokens :-

"="      { atomic Assign }
"("      { atomic OpenP  }
")"      { atomic CloseP }
","      { atomic Comma  }

@string  { str }
@real    { real          }
@ident   { ident         }

$white+	 ;

{
type Action = AlexInput -> Int -> Alex Token

real :: Action
real (_,_,_,s) len = (pure . LitD . read . take len) s

ident :: Action
ident (_,_,_,s) len = (pure . Sym . T.pack . take len) s

str :: Action
str (_,_,_,s) len = (pure . LitS . T.pack . init . tail . take len) s

atomic :: Token -> Action
atomic tok = \_ _ -> pure tok

getState :: Alex AlexState
getState = Alex (\s -> Right (s,s))

lexJupyter :: String -> Either String [Located Token]
lexJupyter s = runAlex s go
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
