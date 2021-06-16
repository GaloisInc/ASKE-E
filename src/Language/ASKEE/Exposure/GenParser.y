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
'='          { Located _ _ Lexer.Assign    }
','          { Located _ _ Lexer.Comma     }
'('          { Located _ _ Lexer.OpenP     }
')'          { Located _ _ Lexer.CloseP    }
'loadEasel'  { Located _ _ Lexer.LoadEasel }
REAL         { Located _ _ (Lexer.LitD $$) }
STRING       { Located _ _ (Lexer.LitS $$) }
SYM          { Located _ _ (Lexer.Sym $$)  }

%%

stmt :: { Stmt }
stmt  : SYM '=' expr { StmtLet $1 $3  }
      | dispExpr     { StmtDisplay $1 }

expr :: { Expr }
expr : SYM                                 { EVar $1     }
     | lit                                 { ELit $1     }
     | functionName '(' commaSepExprs0 ')' { ECall $1 $3 }

dispExpr :: { DisplayExpr }
disExpr : expr { DisplayScalar $1 }

lit :: { Literal }
lit : REAL   { LitNum $1    }
    | STRING { LitString $1 }

functionName :: { FunctionName }
functionName : 'loadEasel' { FLoadEasel }
             -- TODO: Add all the others

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
