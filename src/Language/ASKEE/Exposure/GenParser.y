{
module Language.ASKEE.Exposure.GenParser where

import Language.ASKEE.ESL.Lexer ( Located(..) )
import Language.ASKEE.Exposure.GenLexer
import qualified Language.ASKEE.Exposure.Lexer as Lexer
import Language.ASKEE.Exposure.Syntax as Syntax
}

%name        parseExposureStmt stmt
%tokentype   { Located Lexer.Token }
%error       { parseError          }
%monad       { Either String       }

%token
'='          { Located _ _ Lexer.Assign           }
','          { Located _ _ Lexer.Comma            }
'('          { Located _ _ Lexer.OpenP            }
')'          { Located _ _ Lexer.CloseP           }
REAL         { Located _ _ (Lexer.LitD $$)        }
STRING       { Located _ _ (Lexer.LitS $$)        }
PREFIX_IDENT { Located _ _ (Lexer.PrefixIdent $$) }
INFIX_IDENT  { Located _ _ (Lexer.InfixIdent $$)  }

%%

stmt :: { Stmt }
stmt  : PREFIX_IDENT '=' expr { StmtLet $1 $3  }
      | dispExpr              { StmtDisplay $1 }

expr :: { Expr }
expr : infixExpr    { $1 }
     | nonInfixExpr { $1 }

nonInfixExpr :: { Expr }
nonInfixExpr : PREFIX_IDENT                        { EVar $1 }
             | lit                                 { ELit $1 }
             | PREFIX_IDENT '(' commaSepExprs0 ')' {% do { funName <- prefixFunctionName $1
                                                        ; pure (ECall funName $3) }}

infixExpr :: { Expr }
infixExpr : nonInfixExpr INFIX_IDENT nonInfixExpr {% do { funName <- infixFunctionName $2
                                                        ; pure (ECall funName [$1, $3]) }}

dispExpr :: { DisplayExpr }
disExpr : expr { DisplayScalar $1 }

lit :: { Literal }
lit : REAL   { LitNum $1    }
    | STRING { LitString $1 }

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
