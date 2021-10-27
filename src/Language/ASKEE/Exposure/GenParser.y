{
module Language.ASKEE.Exposure.GenParser where

import Data.Text (Text)
import Language.ASKEE.Core.Syntax (Model(..))
import Language.ASKEE.Model (parseModel, toCore)
import Language.ASKEE.Model.Basics (ModelType(..))
import Language.ASKEE.ESL.Lexer ( Located(..) )
import Language.ASKEE.Exposure.GenLexer
import qualified Language.ASKEE.Exposure.Lexer as Lexer
import Language.ASKEE.Exposure.Syntax as Syntax
}

%name        parseExposureStmts stmts
%name        parseExposureStmt stmt
%name        parseExposureExpr expr
%tokentype   { Located Lexer.Token }
%error       { parseError          }
%monad       { Either String       }

%token
'='      { Located _ _ Lexer.Assign     }
','      { Located _ _ Lexer.Comma      }
'('      { Located _ _ Lexer.OpenP      }
')'      { Located _ _ Lexer.CloseP     }
'['      { Located _ _ Lexer.OpenB      }
']'      { Located _ _ Lexer.CloseB     }
'{'      { Located _ _ Lexer.OpenC      }
'}'      { Located _ _ Lexer.CloseC     }
'{{'     { Located _ _ Lexer.OpenCC     }
'}}'     { Located _ _ Lexer.CloseCC    }
'=>'     { Located _ _ Lexer.LambdaArr  }
'..'     { Located _ _ Lexer.DotDot     }
INT      { Located _ _ (Lexer.LitI $$)  }
REAL     { Located _ _ (Lexer.LitD $$)  }
STRING   { Located _ _ (Lexer.LitS $$)  }
IDENT    { Located _ _ (Lexer.Ident $$) }
'+'      { Located _ _ Lexer.InfixAdd   }
'-'      { Located _ _ Lexer.InfixSub   }
'*'      { Located _ _ Lexer.InfixMul   }
'/'      { Located _ _ Lexer.InfixDiv   }
'>'      { Located _ _ Lexer.InfixGT    }
'>='     { Located _ _ Lexer.InfixGTE   }
'<'      { Located _ _ Lexer.InfixLT    }
'<='     { Located _ _ Lexer.InfixLTE   }
'=='     { Located _ _ Lexer.InfixEQ    }
'!='     { Located _ _ Lexer.InfixNEQ   }
'and'    { Located _ _ Lexer.InfixAnd   }
'or'     { Located _ _ Lexer.InfixOr    }
'not'    { Located _ _ Lexer.InfixNot   }
'false'  { Located _ _ Lexer.BoolFalse  }
'true'   { Located _ _ Lexer.BoolTrue   }
'.'      { Located _ _ Lexer.Dot        }
'at'     { Located _ _ Lexer.At         }
'peak'   { Located _ _ Lexer.Peak       }
'over'   { Located _ _ Lexer.Over       }
'by'     { Located _ _ Lexer.By         }
'@'      { Located _ _ Lexer.AtSymbol   }
'define' { Located _ _ Lexer.Define     }
'end'    { Located _ _ Lexer.End        }
DCHR     { Located _ _ (Lexer.DefChar $$) }

%nonassoc 'at'
%left 'or'
%left 'and'
%nonassoc '<' '<=' '==' '!=' '>=' '>'
%right 'not'
%left '+' '-'
%left '*' '/'
%left '.'

%%

stmts :: { [Stmt] }
stmts : rev_stmts { reverse $1 }

rev_stmts :: { [Stmt] }
rev_stmts : stmt       { [$1] }
          | rev_stmts stmt { $2 : $1 }

stmt :: { Stmt }
stmt  : IDENT '=' expr         { StmtLet $1 $3  }
      | modelDef               {% do { m <- toCore =<< parseModel RNetType (snd $1)
                                     ; let nm = fst $1
                                     ; let e = EVal (VModelExpr (EVal (VModel m { modelName = nm })))
                                     ; pure (StmtLet nm e) }}
      | dispExpr               { StmtDisplay $1 }

modelDef :: { (Text, Text) }
modelDef : 'define' IDENT defContents 'end' { ($2, $3) }

defContents :: { Text }
defContents : rev_defContents { mconcat (reverse $1) }

rev_defContents :: { [Text] }
rev_defContents : DCHR { [$1] }
                | rev_defContents DCHR { $2 : $1}

expr :: { Expr }
expr : IDENT                            { EVar $1 }
     | lit                              { EVal $1 }
     | bool                             { EVal $1 }
     | '@' IDENT '(' commaSepExprs0 ')' { ECall FPython (EVal (VString $2) : $4) }
     | IDENT '(' commaSepExprs0 ')'     {% do { funName <- prefixFunctionName $1
                                              ; pure (ECall funName $3) }}
     | IDENT '(' commaSepExprs0 ')'
             '{' IDENT '=>' expr '}'    {% do { funName <- functionWithLambdaName $1
                                              ; pure (ECallWithLambda funName $3 $6 $8) }}
     | infixExpr                        { $1 }
     | expr '.' IDENT                   { EMember $1 $3 }
     | expr '[' expr ']'                { EIndex $1 $3 }
     | '[' commaSepExprs0 ']'           { EList $2 }
     | '[' expr '..' expr 'by' expr ']' { EListRange $2 $4 $6 }
     | '{{' pointBinds0 '}}'            { EPoint $2 }

infixExpr :: { Expr }
infixExpr : expr '+'   expr { ECall FAdd [$1, $3] }
          | expr '-'   expr { ECall FSub [$1, $3] }
          | expr '*'   expr { ECall FMul [$1, $3] }
          | expr '/'   expr { ECall FDiv [$1, $3] }
          | expr '>'   expr { ECall FGT  [$1, $3] }
          | expr '>='  expr { ECall FGTE [$1, $3] }
          | expr '<'   expr { ECall FLT  [$1, $3] }
          | expr '<='  expr { ECall FLTE [$1, $3] }
          | expr '=='  expr { ECall FEQ  [$1, $3] }
          | expr '!='  expr { ECall FNEQ [$1, $3] }
          | expr 'and' expr { ECall FAnd [$1, $3] }
          | expr 'or'  expr { ECall FOr  [$1, $3] }
          | 'not' expr      { ECall FNot [$2]     }
          | expr 'at' expr  { ECall FAt  [$1, $3] }
          | expr 'at' 'peak' IDENT 'over' expr { ECall FAt [$1, EVal (VString $4), $6] }

dispExpr :: { DisplayExpr }
dispExpr : expr { DisplayScalar $1 }

lit :: { Value }
lit : INT    { VInt $1    }
    | REAL   { VDouble $1 }
    | STRING { VString $1 }

bool :: { Value }
bool : 'false' { VBool False }
     | 'true'  { VBool True }

commaSepExprs0 :: { [Expr] }
commaSepExprs0 : {- empty -}    { [] }
               | commaSepExprs1 { $1 }

commaSepExprs1 :: { [Expr] }
commaSepExprs1 : expr                    { [$1]    }
               | expr ',' commaSepExprs1 { ($1:$3) }

pointBinds0 :: { [(Ident, Expr)] }
pointBinds0 : {- empty -} { [] }
            | pointBinds1 { $1 }

pointBinds1 :: { [(Ident, Expr)] }
pointBinds1 : pointBind                 { [$1]    }
            | pointBind ',' pointBinds1 { ($1:$3) }

pointBind :: { (Ident, Expr) }
pointBind : IDENT '=' expr { ($1, $3) }

{
parseError :: [Located Lexer.Token] -> Either String a
parseError []     = Left $ "parse error at end of file"
parseError (t:ts) = Left $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t) ++ " (" ++ show t ++ ")"
}
