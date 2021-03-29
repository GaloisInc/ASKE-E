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
%name        parseMod Mod
%name        parseActionSequence ActionSequence
%name        parseAction Action
%tokentype   { Located Token}
%error       { parseError }
%monad       { Either String }

%token

eq                  { Located _ _ (Sym "eq")           }
ne                  { Located _ _ (Sym "ne")           }
np                  { Located _ _ (Sym "np")           }
repeat              { Located _ _ (Sym "repeat")       }
bool                { Located _ _ (Sym "bool")         }
zeros               { Located _ _ (Sym "zeros")        }
ones                { Located _ _ (Sym "ones")         }
astype              { Located _ _ (Sym "astype")       }
make_column         { Located _ _ (Sym "make_column")  }
make_param          { Located _ _ (Sym "make_param")   }
make_cohort         { Located _ _ (Sym "make_cohort")  }
make_mod            { Located _ _ (Sym "make_mod")     }
name                { Located _ _ (Sym "name")         }
cohort              { Located _ _ (Sym "cohort")       }
mods                { Located _ _ (Sym "mods")         }
prob_spec           { Located _ _ (Sym "prob_spec")    }
sim_phase           { Located _ _ (Sym "sim_phase")    }
assign              { Located _ _ (Sym "assign")       }
delta               { Located _ _ (Sym "delta") }
reset               { Located _ _ (Sym "reset") }


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
star                { Located _ _ Star       }
starstar            { Located _ _ StarStar   }
pop                 { Located _ _ Pop        }
sim                 { Located _ _ Sim        }
size                { Located _ _ Size       }


%left 'or'
%left 'and'
%nonassoc '<' '<=' '==' '>=' '>'
%nonassoc 'if' 'then' 'else'
%right 'not'
%left '+' '-'
%left '*' '/'

%%

opt(p)
   :    { Nothing }
   | p  { Just $1 }

APRAM                              
  : Size Columns Params Cohorts Mods       { ($1, $2, $3, $4, $5) }

Size                               :: { (Double, Double) }
  : pop '.' size '=' REAL
    delta '=' REAL                    { ($5, $8) }

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
      STRING ',' LambdaDecl pop '.' SYM '.' eq '(' 
        SYM 
      ')' 
    ')'                               { Cohort $5 (Is $10 $14) }
  | pop '.' make_cohort '(' 
      STRING ',' LambdaDecl pop '.' SYM '.' ne '(' 
        SYM 
      ')' 
    ')'                               { Cohort $5 (Syntax.Not $10 $14) }
  | pop '.' make_cohort '(' 
      STRING ',' LambdaDecl np '.' repeat '(' 
        true ',' size
      ')' 
    ')'                               { Cohort $5 All }
  | pop '.' make_cohort '(' 
      STRING ',' LambdaDecl np '.' ones '(' 
        size 
      ')' '.' astype '(' 
        bool 
      ')' 
    ')'                               { Cohort $5 All }


Mods                               :: { [(String, String, [ActionSequence], [ProbSpec], String)] }
  : Mod                               { [$1] }
  | Mods Mod                          { $2 : $1 }

Mod                                :: { (String, String, [ActionSequence], [ProbSpec], String) }
  : sim '.' make_mod '('
      name '=' STRING ','
      cohort '=' pop '.' SYM ','
      mods '=' '['
        ActionSequences
      ']' ','
      prob_spec '=' LambdaDecl '['
        ProbSpecs
      ']' ','
      sim_phase '=' STRING ','
    ')'                               { ($7, $13, $18, $25, $30) }

ActionSequences                    :: { [ActionSequence] }
  :                                   { [] }
  | ActionSequences ActionSequence    { $2 : $1 }

ActionSequence                     :: { ActionSequence }
  : '[' Actions ']' opt(',')          { Actions $2 }
  | '[' ']' opt(',')                  { Pass }

Actions                            :: { [Syntax.Action] }
  : Action                            { [$1] }
  | Actions Action                    { $2 : $1 }

Action                             :: { Syntax.Action }
  : LambdaDecl pop '.' SYM '.' assign '(' 
      SYM ',' starstar SYM 
    ')' opt(',')                      { Syntax.Assign $4 $8 }

LambdaDecl                         :: { () }
  : lambda LambdaArgs ':'             { () }

LambdaArgs                         :: { () }
  :                                   { () }
  | LambdaArgs LambdaArg              { () }
  | LambdaArgs LambdaArg ','          { () }

LambdaArg                          :: { () }
  : SYM                               { () }
  | '*' SYM                           { () }
  | starstar SYM                      { () }

ProbSpecs                          :: { [ProbSpec] }
  :                                   { [] }
  | ProbSpecs Exp                     { Probability $2 : $1 }
  | ProbSpecs Exp ','                 { Probability $2 : $1 }


Exp                                :: { Expr }
  : REAL                              { Expr.LitD $1 }
  | SYM                               { Expr.Var (pack $1) }
  | Exp '/' Exp                       { Expr.Div $1 $3 }

Exp                                   :: { Expr }
  : Exp '+' Exp                          { Add  $1 $3 }
  | Exp '-' Exp                          { Sub  $1 $3 }
  | Exp '*' Exp                          { Mul  $1 $3 }
  | Exp '/' Exp                          { Div  $1 $3 }
  | '-' Exp                              { Neg  $2 }
  | 'exp' '(' Exp ')'                    { Exp  $3 }
  | 'log' '(' Exp ')'                    { Log  $3 }
  | Exp '>' Exp                          { GT  $1 $3 }
  | Exp '>=' Exp                         { GTE $1 $3 }
  | Exp '==' Exp                         { EQ  $1 $3 }
  | Exp '<=' Exp                         { LTE $1 $3 }
  | Exp '<' Exp                          { LT  $1 $3 }
  | Exp 'and' Exp                        { And $1 $3 }
  | Exp 'or'  Exp                        { Or  $1 $3 } 
  | 'not' Exp                            { Not $2 }
  | '(' Exp ')'                          { $2 }
  | Var                                  { Var $1 }
  | REAL                                 { LitD $1 }
  | 'True'                               { LitB True }
  | 'False'                              { LitB False }
  | Exp 'if' Exp 'else' Exp              { If $3 $1 $5 }

Var :: { Expr }
  : SYM { Var $1 }
  | pop '.' SYM '.' val { Var $3 }
  | pop '.' SYM '.' size { Var $3 }

{
-- mkAPRAM :: Double -> Map String (String, Expr) -> Map String Expr -> [Cohort] -> [Mod] -> APRAM
mkAPRAM = undefined

mkMod :: String -> Cohort -> [ActionSequence] -> [ProbSpec] -> String -> Either String Mod
mkMod name cohort actions probs phase
  | length actions == length probs = Right $ Mod name cohort (zip actions probs) phase
  | otherwise = Left $ "mod action and probability lists differ in length"

parseError :: [Located Lexer.Token] -> Either String a
parseError []     = Left $ "parse error at end of file"
parseError (t:ts) = Left $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t) ++ " (" ++ show t ++ ")"

}