{-# Language OverloadedStrings #-}
module Language.ASKEE.C where

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint
          (Doc,nest,vcat,(<+>),($$),comma,hsep,punctuate,parens,semi,colon)

infixl 6 <.>
(<.>) :: Doc -> Doc -> Doc
(<.>) = (PP.<>)

function :: Doc -> Doc -> [Doc] -> [Doc] -> Doc
function retTy name args xs =
  vcat [ retTy <+> name <.> parens (hsep (punctuate comma args)) <+> "{"
       , nested xs
       , "}"
       ]

stmt :: Doc -> Doc
stmt s = s <.> semi

decl :: Doc -> Doc -> Doc
decl ty nm = ty <+> nm

void :: Doc
void = "void"

int :: Doc
int = "int"

double :: Doc
double = "double"

bool :: Doc
bool = "bool"


switch :: Doc -> [Doc] -> Doc
switch e cs = "switch" <+> parens e <+> "{" $$ nested cs $$ "}"

case' :: Doc -> Doc
case' c = "case" <+> c <.> colon

ifThen :: Doc -> [Doc] -> Doc
ifThen c t =
  vcat [ "if" <+> parens c <+> "{"
       , nested t
       , "}"
       ]

ifThenElse :: Doc -> [Doc] -> [Doc] -> Doc
ifThenElse c t e =
  vcat [ "if" <+> parens c <+> "{"
       , nested t
       , "} else {"
       , nested e
       , "}"
       ]

break :: Doc
break = "break;"

return :: Doc
return = "return;"

nested :: [Doc] -> Doc
nested = nest 2 . stmts

stmts :: [Doc] -> Doc
stmts = vcat

