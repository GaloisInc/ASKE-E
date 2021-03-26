{
module Language.ASKEE.APRAM.GenParser where

import Language.ASKEE.APRAM.GenLexer
import Language.ASKEE.APRAM.Lexer as Lexer
import Language.ASKEE.APRAM.Syntax

import Language.ASKEE.Expr as Expr

import qualified Data.Map as Map
import Data.Map ( Map )

import Data.Text ( pack )
}

%name        parseAPRAM APRAM
%name        parseCohort Cohort
%tokentype   { Located Token}
%error       { parseError }
%monad       { Either String }

%token

BOOL                { Located _ _ (Lexer.LitB $$)  }
REAL                { Located _ _ (Lexer.LitD $$)  }
STRING              { Located _ _ (LitS $$)  }
SYM                 { Located _ _ (Sym $$)   }
'+'                 { Located _ _ Plus       }
'-'                 { Located _ _ Minus      }
'*'                 { Located _ _ Times      }
'/'                 { Located _ _ Divide     }
'='                 { Located _ _ Lexer.Assign }
'<='                { Located _ _ Lexer.LTE  }
'>='                { Located _ _ Lexer.GTE  }
'<'                 { Located _ _ Lexer.LT   }
'>'                 { Located _ _ Lexer.GT   }
'=='                { Located _ _ Lexer.EQ   }

'('                 { Located _ _ OpenP      }
')'                 { Located _ _ CloseP     }
'['                 { Located _ _ OpenB      }
']'                 { Located _ _ CloseB     }

':'                 { Located _ _ Colon      }
','                 { Located _ _ Comma      }
'.'                 { Located _ _ Dot        }

ceq                 { Located _ _ (Sym "eq") }

lambda              { Located _ _ Lambda     }
not                 { Located _ _ Lexer.Not  }
and                 { Located _ _ Lexer.And  }
or                  { Located _ _ Lexer.Or   }
if                  { Located _ _ Lexer.If   }
then                { Located _ _ Then       }
else                { Located _ _ Else       }
for                 { Located _ _ For        }
in                  { Located _ _ In         }
return              { Located _ _ Return     }
log                 { Located _ _ Lexer.Log  }
exp                 { Located _ _ Lexer.Exp  }
pop                 { Located _ _ Pop        }
sim                 { Located _ _ Sim        }
size                { Located _ _ Size       }
make_column         { Located _ _ MakeColumn }
make_param          { Located _ _ MakeParam  }
make_cohort         { Located _ _ MakeCohort }


%left 'or'
%left 'and'
%nonassoc '<' '<=' '==' '>=' '>'
%nonassoc 'if' 'then' 'else'
%right 'not'
%left '+' '-'
%left '*' '/'

%%

APRAM                              :: { APRAM }
  : size Columns Params Cohorts       { mkAPRAM $1 $2 $3 $4 undefined }


Columns                            :: { Map String [(String, Double)] }
  : Column                            { uncurry Map.singleton $1 }
  | Columns Column                    { uncurry Map.insert $2 $1 }

Column                             :: { (String, [(String, Double)]) }
  : pop '.' make_column '(' STRING ',' Exp ')' Statuses { ($5, $9) }


Statuses                           :: { [(String, Double)] }
  : Status                            { [$1] }
  | Statuses Status                   { $2 : $1 }

Status                             :: { (String, Double) }
  : SYM '=' REAL                      { ($1, $3) }


Params                             :: { Map String Expr }
  : Param                             { uncurry Map.singleton $1 }
  | Params Param                      { uncurry Map.insert $2 $1 }
  
Param                              :: { (String, Expr) }
  : make_param '(' STRING ',' Exp ')' { ($3, $5) }


Cohorts                            :: { [Cohort] }
  : Cohort                            { [$1] }
  | Cohorts Cohort                    { $2 : $1 }

Cohort                             :: { Cohort }
  : pop '.' make_cohort '(' STRING ',' lambda ':' pop '.' SYM '.' ceq '(' SYM ')' ')' { Cohort $5 (Is $11 $15) }


Exp                                :: { Expr }
  : REAL                              { Expr.LitD $1 }
  | SYM                               { Expr.Var (pack $1) }
  | SYM '.' Exp                       { undefined }
  | SYM '(' Exp ')'                   { undefined }


{
-- mkAPRAM :: Double -> Map String (String, Expr) -> Map String Expr -> [Cohort] -> [Mod] -> APRAM
mkAPRAM = undefined

parseError :: [Located Lexer.Token] -> Either String a
parseError []     = Left $ "parse error at end of file"
parseError (t:ts) = Left $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t) ++ " (" ++ show t ++ ")"

}