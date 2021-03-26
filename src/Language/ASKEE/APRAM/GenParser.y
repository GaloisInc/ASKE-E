{
module Language.ASKEE.APRAM.GenParser where

import Language.ASKEE.APRAM.GenLexer
import Language.ASKEE.APRAM.Lexer as Lexer
import Language.ASKEE.APRAM.Syntax as Syntax

import Language.ASKEE.Expr as Expr

import qualified Data.Map as Map
import Data.Map ( Map )

import Data.Text ( pack )
}

%name        parseAPRAM APRAM
%tokentype   { Located Token}
%error       { parseError }
%monad       { Either String }

%token

eq                  { Located _ _ (Sym "eq") }
ne                  { Located _ _ (Sym "ne") }
np                  { Located _ _ (Sym "np") }
repeat              { Located _ _ (Sym "repeat") }
bool                { Located _ _ (Sym "bool") }
zeros               { Located _ _ (Sym "zeros") }
ones                { Located _ _ (Sym "ones") }
astype              { Located _ _ (Sym "astype") }


true                { Located _ _ (Lexer.LitB True)  }
false               { Located _ _ (Lexer.LitB False)  }
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

APRAM                              
  : Size Columns Params Cohorts       { ($1, $2, $3, $4) }

Size                               :: { Double }
  : size '=' REAL                     { $3 }

Columns                            :: { Map String [(String, Double)] }
  : Column                            { uncurry Map.singleton $1 }
  | Columns Column                    { uncurry Map.insert $2 $1 }

Column                             :: { (String, [(String, Double)]) }
  : pop '.' make_column '(' 
      STRING ',' np '.' zeros '(' 
        size 
      ')' 
    ')' Statuses                      { ($5, $14) }


Statuses                           :: { [(String, Double)] }
  : Status                            { [$1] }
  | Statuses Status                   { $2 : $1 }

Status                             :: { (String, Double) }
  : SYM '=' REAL                      { ($1, $3) }


Params                             :: { Map String Expr }
  : Param                             { uncurry Map.singleton $1 }
  | Params Param                      { uncurry Map.insert $2 $1 }
  
Param                              :: { (String, Expr) }
  : pop '.' make_param '(' 
      STRING ',' Exp 
    ')'                               { ($5, $7) }


Cohorts                            :: { [Cohort] }
  : Cohort                            { [$1] }
  | Cohorts Cohort                    { $2 : $1 }

Cohort                             :: { Cohort }
  : pop '.' make_cohort '(' 
      STRING ',' lambda ':' pop '.' SYM '.' eq '(' 
        SYM 
      ')' 
    ')'                               { Cohort $5 (Is $11 $15) }
  | pop '.' make_cohort '(' 
      STRING ',' lambda ':' pop '.' SYM '.' ne '(' 
        SYM 
      ')' 
    ')'                               { Cohort $5 (Syntax.Not $11 $15) }
  | pop '.' make_cohort '(' 
      STRING ',' lambda ':' np '.' repeat '(' 
        true ',' size
      ')' 
    ')'                               { Cohort $5 All }
  | pop '.' make_cohort '(' 
      STRING ',' lambda ':' np '.' ones '(' 
        size 
      ')' '.' astype '(' 
        bool 
      ')' 
    ')'                               { Cohort $5 All }


Exp                                :: { Expr }
  : REAL                              { Expr.LitD $1 }
  | SYM                               { Expr.Var (pack $1) }
  | SYM '.' Exp                       { undefined }
  | size                              { undefined }
  | SYM '(' 
      Exp 
    ')'                               { undefined }


{
-- mkAPRAM :: Double -> Map String (String, Expr) -> Map String Expr -> [Cohort] -> [Mod] -> APRAM
mkAPRAM = undefined

parseError :: [Located Lexer.Token] -> Either String a
parseError []     = Left $ "parse error at end of file"
parseError (t:ts) = Left $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t) ++ " (" ++ show t ++ ")"

}