{
module Language.ASKEE.Jupyter.GenParser where

import Language.ASKEE.ESL.Lexer ( Located(..) )
import Language.ASKEE.Jupyter.GenLexer
import Language.ASKEE.Jupyter.Lexer as Lexer
import Language.ASKEE.Jupyter.Syntax as Syntax
}

%name        parseJupyterStmt stmt
%tokentype   { Located Token }
%error       { parseError    }
%monad       { Either String }

%token
'='          { Located _ _ Lexer.Assign    }
','          { Located _ _ Comma           }
'('          { Located _ _ OpenP           }
')'          { Located _ _ CloseP          }
REAL         { Located _ _ (Lexer.LitD $$) }
STRING       { Located _ _ (Lexer.LitS $$) }
SYM          { Located _ _ (Lexer.Sym $$)  }

%%

stmt :: { Stmt }
stmt  : SYM '=' expr { StmtAssign $1 $3 }
      | expr         { StmtEval $1      }

expr :: { Expr }
expr : SYM '(' commaSepExprs0 ')' { ExprCall $1 $3 }
     | SYM                        { ExprVar $1     }
     | STRING                     { ExprString $1  }
     | REAL                       { ExprNumber $1  }

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
