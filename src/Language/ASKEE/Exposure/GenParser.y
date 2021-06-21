{
module Language.ASKEE.Exposure.GenParser where

import Language.ASKEE.ESL.Lexer ( Located(..) )
import Language.ASKEE.Exposure.GenLexer
import qualified Language.ASKEE.Exposure.Lexer as Lexer
import Language.ASKEE.Exposure.Syntax as Syntax
}

%name        parseExposureStmt stmt
%name        parseExposureExpr expr
%tokentype   { Located Lexer.Token }
%error       { parseError          }
%monad       { Either String       }

%token
'='    { Located _ _ Lexer.Assign     }
','    { Located _ _ Lexer.Comma      }
'('    { Located _ _ Lexer.OpenP      }
')'    { Located _ _ Lexer.CloseP     }
REAL   { Located _ _ (Lexer.LitD $$)  }
STRING { Located _ _ (Lexer.LitS $$)  }
IDENT  { Located _ _ (Lexer.Ident $$) }
'+'    { Located _ _ Lexer.InfixAdd   }
'-'    { Located _ _ Lexer.InfixSub   }
'*'    { Located _ _ Lexer.InfixMul   }
'/'    { Located _ _ Lexer.InfixDiv   }
'>'    { Located _ _ Lexer.InfixGT    }
'>='   { Located _ _ Lexer.InfixGTE   }
'<'    { Located _ _ Lexer.InfixLT    }
'<='   { Located _ _ Lexer.InfixLTE   }
'=='   { Located _ _ Lexer.InfixEQ    }
'!='   { Located _ _ Lexer.InfixNEQ   }
'&&'   { Located _ _ Lexer.InfixAnd   }
'||'   { Located _ _ Lexer.InfixOr    }

%left '||'
%left '&&'
%nonassoc '<' '<=' '==' '!=' '>=' '>'
%left '+' '-'
%left '*' '/'

%%

stmt :: { Stmt }
stmt  : IDENT '=' expr { StmtLet $1 $3  }
      | dispExpr       { StmtDisplay $1 }

expr :: { Expr }
expr : IDENT                        { EVar $1 }
     | lit                          { EVal $1 }
     | IDENT '(' commaSepExprs0 ')' {% do { funName <- prefixFunctionName $1
                                          ; pure (ECall funName $3) }}
     | infixExpr                    { $1 }

infixExpr :: { Expr }
infixExpr : expr '+'  expr { ECall FAdd [$1, $3] }
          | expr '-'  expr { ECall FSub [$1, $3] }
          | expr '*'  expr { ECall FMul [$1, $3] }
          | expr '/'  expr { ECall FDiv [$1, $3] }
          | expr '>'  expr { ECall FGT  [$1, $3] }
          | expr '>=' expr { ECall FGTE [$1, $3] }
          | expr '<'  expr { ECall FLT  [$1, $3] }
          | expr '<=' expr { ECall FLTE [$1, $3] }
          | expr '==' expr { ECall FEQ  [$1, $3] }
          | expr '!=' expr { ECall FNEQ [$1, $3] }
          | expr '&&' expr { ECall FAnd [$1, $3] }
          | expr '||' expr { ECall FOr  [$1, $3] }

dispExpr :: { DisplayExpr }
disExpr : expr { DisplayScalar $1 }

lit :: { Value }
lit : REAL   { VDouble $1 }
    | STRING { VString $1 }

commaSepExprs0 :: { [Expr] }
commaSepExprs0 : {- empty -}    { [] }
               | commaSepExprs1 { $1 }

commaSepExprs1 :: { [Expr] }
commaSepExprs1 : expr                    { [$1]    }
               | expr ',' commaSepExprs1 { ($1:$3) }

{
parseError :: [Located Lexer.Token] -> Either String a
parseError []     = Left $ "parse error at end of file"
parseError (t:ts) = Left $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t) ++ " (" ++ show t ++ ")"
}
