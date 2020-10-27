{
{-# LANGUAGE TypeApplications #-}

module Language.ASKEE.DiffEq.GenLexer where

import Data.Text ( pack )

import Language.ASKEE.DiffEq.Lexer
}

%wrapper "posn"

$bs = \\
$digit = [0-9]
$ident = [A-Za-z0-9_]

@identifier = $ident+
@real       = $digit+ (\. $digit+)?

tokens :-

"{"              { atomic OpenB  }
"}"              { atomic CloseB }
"\left("         { atomic OpenP  }
"\right)"        { atomic CloseP }
"+"              { atomic Plus   }
"-"              { atomic Minus  }
"*"              { atomic Times  }
"/"              { atomic Divide }  -- probably unnecessary
"="              { atomic Eq     }
$bs "frac"       { atomic Frac   }
$bs @identifier  { reserved      }
@real            { real          }
@identifier      { ident         }

$white+          ;


{
type Action = AlexPosn -> String -> Located Token

atomic :: Token -> Action
atomic t (AlexPn offset lin col) _ = Located lin col t

reserved :: Action
reserved (AlexPn offset lin col) str = (Located lin col . Var . pack . tail) str

ident :: Action
ident (AlexPn offset lin col) str = (Located lin col . Var . pack) str

real :: Action
real (AlexPn offset lin col) str = (Located lin col . Lit . read @Double) str
}