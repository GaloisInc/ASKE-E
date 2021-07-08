{
module Language.ASKEE.Exposure.GenLexer where

import qualified Data.Text as T

import Language.ASKEE.ESL.Lexer (Located(..))
import Language.ASKEE.Exposure.Lexer
}

%wrapper "monad"

$upper   = [A-Z]
$lower   = [a-z]
$digit   = [0-9]
$graphic = $printable # $white

@string    = \" ($graphic # \")* \"
@exp       = e [\+\-]? $digit+
@real      = $digit+ (\. $digit+)? @exp?
@identHead = [$upper $lower _]
@identBody = [$upper $lower $digit _]
@ident     = @identHead @identBody*
@body      = (. | \n)*
@def       = "define" @body "end" "define"

tokens :-

<0> "="         { atomic Assign    }
<0> "("         { atomic OpenP     }
<0> ")"         { atomic CloseP    }
<0> ","         { atomic Comma     }
<0> "+"         { atomic InfixAdd  }
<0> "-"         { atomic InfixSub  }
<0> "*"         { atomic InfixMul  }
<0> "/"         { atomic InfixDiv  }
<0> "<="        { atomic InfixLTE  }
<0> ">="        { atomic InfixGTE  }
<0> "<"         { atomic InfixLT   }
<0> ">"         { atomic InfixGT   }
<0> "=="        { atomic InfixEQ   }
<0> "!="        { atomic InfixNEQ  }
<0> "."         { atomic Dot       }
<0> "and"       { atomic InfixAnd  }
<0> "or"        { atomic InfixOr   }
<0> "not"       { atomic InfixNot  }
<0> "false"     { atomic BoolFalse }
<0> "true"      { atomic BoolTrue  }
<0> "at"        { atomic At        }

<0> "define"   { defineModel }
<defNm> @ident { modelIdent }
<defSC> "end"  { endBody }
<defSC> .      { defChar }
<defSC> \n     { defChar }

<0> @string   { str   }
<0> @real     { real  }
<0> @ident    { ident }

<0,defNm> $white+	 ;

{
type Action = AlexInput -> Int -> Alex Token

real :: Action
real (_,_,_,s) len = (pure . LitD . read . take len) s

ident :: Action
ident (_,_,_,s) len = (pure . Ident . T.pack . take len) s

str :: Action
str (_,_,_,s) len = (pure . LitS . T.pack . init . tail . take len) s

defChar :: Action
defChar (_,_,_,s) len = (pure . DefChar . T.pack . take len) s

defineModel :: Action
defineModel (_,_,_,s) len =
  do alexSetStartCode defNm
     pure Define

modelIdent :: Action
modelIdent inp len =
  do alexSetStartCode defSC
     ident inp len

endBody :: Action
endBody _ _ =
  do alexSetStartCode 0
     return End

atomic :: Token -> Action
atomic tok = \_ _ -> pure tok

getState :: Alex AlexState
getState = Alex (\s -> Right (s,s))

lexExposure :: String -> Either String [Located Token]
lexExposure s = runAlex s go
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
