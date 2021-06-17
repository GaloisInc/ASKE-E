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
$symbol  = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$graphic = $printable # $white

@string          = \" ($graphic # \")* \"
@exp             = e [\+\-]? $digit+
@real            = $digit+ (\. $digit+)? @exp?
@prefixIdentHead = [$upper $lower _]
@prefixIdentBody = [$upper $lower $digit _]
@prefixIdent     = @prefixIdentHead @prefixIdentBody*
@infixIdent      = $symbol+

tokens :-

"="         { atomic Assign    }
"("         { atomic OpenP     }
")"         { atomic CloseP    }
","         { atomic Comma     }

@string      { str         }
@real        { real        }
@prefixIdent { prefixIdent }
@infixIdent  { infixIdent  }

$white+	 ;

{
type Action = AlexInput -> Int -> Alex Token

real :: Action
real (_,_,_,s) len = (pure . LitD . read . take len) s

prefixIdent :: Action
prefixIdent (_,_,_,s) len = (pure . PrefixIdent . T.pack . take len) s

infixIdent :: Action
infixIdent (_,_,_,s) len = (pure . InfixIdent . T.pack . take len) s

str :: Action
str (_,_,_,s) len = (pure . LitS . T.pack . init . tail . take len) s

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
