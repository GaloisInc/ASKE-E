{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.ASKEE.GenParser where
import Language.ASKEE.GenLexer
import Language.ASKEE.Lexer as Lexer
import Language.ASKEE.SurfaceSyntax as Syntax

import Control.Monad.State

import Data.Map as M
}

%name parse
%tokentype {Located Token}
%error {parseError}

%token

model       { Located _ _ Lexer.Model }
sym         { Located _ _ (Lexer.Sym $$) }
let         { Located _ _ Lexer.Let }
state       { Located _ _ Lexer.State }
event       { Located _ _ Lexer.Event }
':'         { Located _ _ Lexer.Colon }
'='         { Located _ _ Lexer.Assign }
'+'         { Located _ _ Lexer.Plus }
'-'         { Located _ _ Lexer.Minus }
real        { Located _ _ (Lexer.Real $$) }
tab         { Located _ _ Lexer.Indent }
untab       { Located _ _ Lexer.Dedent }
nl          { Located _ _ Lexer.Newline }
when        { Located _ _ Lexer.When }
rate        { Located _ _ Lexer.Rate }
effect      { Located _ _ Lexer.Effect }

%left '+' '-'

%%

Model : model NamedBlockInit Decls Events BlockEnd { Syntax.Model $2 $3 $4 }

Decls :            { [] }
      | Decls Decl { $2 : $1 }

Decl : let   sym '=' Exp Break { Syntax.Let $2 $4 }
     | state sym '=' Exp Break { Syntax.State $2 $4 }

Events :              { [] }
       | Events Event { $2 : $1 }

Event : event NamedBlockInit WhenBlock RateBlock EffectBlock MDBlock BlockEnd { Syntax.Event $2 $3 $4 $5 Nothing }

WhenBlock   :                                            { Nothing }
            | when   BlockInit Exp        Break BlockEnd { Just $3 }
RateBlock   : rate   BlockInit Exp        Break BlockEnd { $3 }
EffectBlock : effect BlockInit Statements       BlockEnd { $3 }

MDBlock : {}

Statements : { [] }
           | Statements Statement { $2 : $1 }

Statement : sym '=' Exp Break { ($1, $3) }

NamedBlockInit : sym ':' Break tab { $1 }
BlockInit      :     ':' Break tab {}
BlockEnd       : untab {}

Break0 :       {}
       | Break {}

Break : nl       {}
      | nl Break {}
      

Exp : Exp '+' Exp { Syntax.Add $1 $3 }
    | Exp '-' Exp { Syntax.Sub $1 $3 }
    | real        { Syntax.Real $1 }
    | sym         { Syntax.Var $1 }

{

parseError :: [Located Token] -> a
parseError []     = error $ "parse error at end of file"
parseError (t:ts) = error $ "parse error at line " ++ show (locLine t) ++ ", col " ++ show (locCol t)
}