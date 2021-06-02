{
{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.APRAM.GenParser where

import Language.ASKEE.APRAM.GenLexer
import Language.ASKEE.APRAM.Lexer as Lexer
import Language.ASKEE.APRAM.Syntax as Syntax

import Language.ASKEE.Expr as Expr
import Language.ASKEE.ESL.Lexer ( Located(..) )

import qualified Data.Map as Map
import Data.Map ( Map )

import Data.Text ( Text, pack, unpack )
}

%name        parseAPRAM APRAM
%tokentype   { Located Token}
%error       { parseError }
%monad       { Either String }

%token

eq                  { Located _ _ (Sym "eq")           }
ne                  { Located _ _ (Sym "ne")           }
logical_and         { Located _ _ (Sym "logical_and")  }
logical_or          { Located _ _ (Sym "logical_or")   }
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
delta               { Located _ _ (Sym "delta")        }
size                { Located _ _ (Sym "size")         }
val                 { Located _ _ (Sym "val")          }


true                { Located _ _ (Lexer.LitB True)    }
false               { Located _ _ (Lexer.LitB False)   }
REAL                { Located _ _ (Lexer.LitD $$)      }
STRING              { Located _ _ (LitS $$)            }
SYM                 { Located _ _ (Sym $$)             }
'+'                 { Located _ _ Plus                 }
'-'                 { Located _ _ Minus                }
'*'                 { Located _ _ Times                }
'/'                 { Located _ _ Divide               }
'='                 { Located _ _ Lexer.Assign         }
'<='                { Located _ _ Lexer.LTE            }
'>='                { Located _ _ Lexer.GTE            }
'<'                 { Located _ _ Lexer.LT             }
'>'                 { Located _ _ Lexer.GT             }
'=='                { Located _ _ Lexer.EQ             }

'('                 { Located _ _ OpenP                }
')'                 { Located _ _ CloseP               }
'['                 { Located _ _ OpenB                }
']'                 { Located _ _ CloseB               }

':'                 { Located _ _ Colon                }
','                 { Located _ _ Comma                }
'.'                 { Located _ _ Dot                  }

lambda              { Located _ _ Lambda               }
not                 { Located _ _ Lexer.Not            }
and                 { Located _ _ Lexer.And            }
or                  { Located _ _ Lexer.Or             }
if                  { Located _ _ Lexer.If             }
else                { Located _ _ Else                 }
log                 { Located _ _ Lexer.Log            }
exp                 { Located _ _ Lexer.Exp            }
starstar            { Located _ _ StarStar             }
pop                 { Located _ _ Pop                  }
sim                 { Located _ _ Sim                  }


%left or
%left and
%nonassoc '<' '<=' '==' '>=' '>'
%nonassoc if else
%right not
%left '+' '-'
%left '*' '/'

%%

REV1(p)
  : p                { [$1] }
  | REV1(p) p        { $2 : $1 }

REV(p)
  :                  { [] }
  | REV(p) p         { $2 : $1 }

OPT(p)
   :                 { Nothing }
   | p               { Just $1 }

MANY1(p) 
  : REV1(p)          { reverse $1 }

MANY(p) 
  : REV(p)           { reverse $1 }

MANY1MAP(p)
  : p                { uncurry Map.singleton $1 }
  | MANY1MAP(p) p    { uncurry Map.insert $2 $1 }



APRAM                              
  : Size 
    MANY1MAP(Column) 
    MANY1MAP(Param) 
    MANY1(Cohort)
    MANY1(Mod)                        {% mkAPRAM $1 $2 $3 $4 $5 }

Size                               :: { (Double, Double) }
  : pop '.' size '=' REAL
    delta '=' REAL                    { ($5, $8) }


Column                             :: { (Text, [(Text, Double)]) }
  : pop '.' make_column '(' 
      STRING ',' np '.' zeros '(' 
        pop '.' size 
      ')' 
    ')' MANY1(Status)                 { ($5, $16) }

Status                             :: { (Text, Double) }
  : SYM '=' REAL                      { ($1, $3) }

  
Param                              :: { (Text, Expr) }
  : pop '.' make_param '(' 
      STRING ',' Exp 
    ')'                               { ($5, $7) }


Cohort                             :: { Cohort }
  : pop '.' make_cohort '(' 
      STRING ',' LambdaDecl CohortExpr
    ')'                               { Cohort $5 $8 }

CohortExpr 
  : pop '.' SYM '.' eq '(' 
      SYM 
    ')'                               { Is $3 $7 }
  | pop '.' SYM '.' ne '(' 
      SYM 
    ')'                               { Syntax.Not $3 $7 }
  | np '.' repeat '(' 
      true ',' pop '.' size
    ')'                               { All }
  | np '.' ones '(' 
      pop '.' size 
    ')' '.' astype '(' 
      bool 
    ')'                               { All }
  | np '.' logical_and '(' 
      CohortExpr ',' CohortExpr 
    ')'                               { Syntax.And $5 $7 }
  | np '.' logical_or '(' 
      CohortExpr ',' CohortExpr 
    ')'                               { Syntax.Or $5 $7 }


Mod                                :: { (Text, Text, [ActionSequence], [ProbSpec], Text) }
  : sim '.' make_mod '('
      name '=' STRING ','
      cohort '=' pop '.' SYM ','
      mods '=' '['
        MANY(ActionSequence)
      ']' ','
      prob_spec '=' OPT(LambdaDecl) '['
        ProbSpecs
      ']' ','
      sim_phase '=' STRING OPT(',')
    ')'                               { ($7, $13, $18, $25, $30) }

ActionSequence                     :: { ActionSequence }
  : '[' MANY1(Action) ']' OPT(',')    { Actions $2 }
  | '[' ']' OPT(',')                  { Pass }

Action                             :: { Syntax.Action }
  : LambdaDecl pop '.' SYM '.' assign '(' 
      SYM ',' starstar SYM 
    ')' OPT(',')                      { Syntax.Assign $4 $8 }

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
  : Exp '+' Exp                       { Add  $1 $3 }
  | Exp '-' Exp                       { Sub  $1 $3 }
  | Exp '*' Exp                       { Mul  $1 $3 }
  | Exp '/' Exp                       { Div  $1 $3 }
  | '-' Exp                           { Neg  $2 }
  | exp '(' Exp ')'                   { Expr.Exp  $3 }
  | log '(' Exp ')'                   { Expr.Log  $3 }
  | Exp '>' Exp                       { Expr.GT  $1 $3 }
  | Exp '>=' Exp                      { Expr.GTE $1 $3 }
  | Exp '==' Exp                      { Expr.EQ  $1 $3 }
  | Exp '<=' Exp                      { Expr.LTE $1 $3 }
  | Exp '<' Exp                       { Expr.LT  $1 $3 }
  | Exp and Exp                       { Expr.And $1 $3 }
  | Exp or  Exp                       { Expr.Or  $1 $3 } 
  | not Exp                           { Expr.Not $2 }
  | '(' Exp ')'                       { $2 }
  | Var                               { Var ($1) }
  | REAL                              { Expr.LitD $1 }
  | true                              { Expr.LitB True }
  | false                             { Expr.LitB False }
  | Exp if Exp else Exp               { Expr.If $3 $1 $5 }

Var                                :: { Text }
  : SYM                               { $1 }
  | delta                             { "delta" }
  | pop '.' size                      { "size" }
  | pop '.' SYM '.' val               { $3 }
  | pop '.' SYM '.' size              { $3 }

{
mkAPRAM :: (Double, Double) 
        -> Map Text [(Text, Double)] 
        -> Map Text Expr 
        -> [Cohort] 
        -> [(Text, Text, [ActionSequence], [ProbSpec], Text)] 
        -> Either String APRAM
mkAPRAM (size, delta) columns params cohorts mods =
  APRAM (floor size) params (Map.map (map fst) columns) cohorts <$> (mapM mkMod mods)
  where
    mkMod :: (Text, Text, [ActionSequence], [ProbSpec], Text) -> Either String Mod
    mkMod (name, cohortName, actions, probs, phase)
      | length actions == length probs = 
        Mod name <$> 
          (findOne ("is "<>unpack cohortName) (\(Cohort name _) -> name == cohortName) cohorts) <*> 
          (pure $ zip actions probs) <*>
          (pure phase)
      | otherwise = Left $ "mod action and probability lists differ in length"

    findOne :: String -> (a -> Bool) -> [a] -> Either String a
    findOne why p xs =
      case filter p xs of
        [x] -> Right x
        _ -> Left why

mkMod :: Text -> Cohort -> [ActionSequence] -> [ProbSpec] -> Text -> Either String Mod
mkMod name cohort actions probs phase
  | length actions == length probs = Right $ Mod name cohort (zip actions probs) phase
  | otherwise = Left $ "mod action and probability lists differ in length"

parseError :: [Located Lexer.Token] -> Either String a
parseError []     = Left $ "parse error at end of file"
parseError (t:ts) = Left $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t) ++ " (" ++ show t ++ ")"

}