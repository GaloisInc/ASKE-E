{-# Language OverloadedStrings #-}
module Language.ASKEE.C where

import Data.Text(Text)
import qualified Data.Text as Text
import Text.PrettyPrint
          ( empty
          , nest,vcat,hsep,(<+>),($$)
          , punctuate, comma, semi, colon
          , braces
          )
import qualified Text.PrettyPrint as PP

type Doc = PP.Doc

infixl 6 <.>
(<.>) :: Doc -> Doc -> Doc
(<.>) = (PP.<>)

nested :: [Doc] -> Doc
nested = nest 2 . stmts


function :: Doc -> Doc -> [Doc] -> [Doc] -> Doc
function retTy name args xs =
  vcat [ retTy <+> name <.> parens (hsep (punctuate comma args)) <+> "{"
       , nested xs
       , "}"
       ]

arg :: Doc -> Doc -> Doc
arg ty name = ty <+> name

refArg :: Doc -> Doc -> Doc
refArg ty name = ty <+> "&" <.> name

ptrArg :: Doc -> Doc -> Doc
ptrArg ty name = ty <+> "*" <.> name

--------------------------------------------------------------------------------
-- Expressions (kind of)

call :: Doc -> [Doc] -> Doc
call f args = f <.> parens (hsep (punctuate comma args))

callCon :: Doc -> [Doc] -> Doc
callCon f args = f <.> braces (hsep (punctuate comma args))

callInfix :: Doc -> Doc -> Doc -> Doc
callInfix op x y = x <+> op <+> y

callPrefix :: Doc -> Doc -> Doc
callPrefix op x = op <+> x

(+) :: Doc -> Doc -> Doc
(+) = callInfix "+"

(-) :: Doc -> Doc -> Doc
(-) = callInfix "-"

(<=) :: Doc -> Doc -> Doc
(<=) = callInfix "<="

(<) :: Doc -> Doc -> Doc
(<) = callInfix "<"

(>) :: Doc -> Doc -> Doc
(>) = callInfix ">"

(==) :: Doc -> Doc -> Doc
(==) = callInfix "=="

(&&) :: Doc -> Doc -> Doc
(&&) = callInfix "&&"

(||) :: Doc -> Doc -> Doc
(||) = callInfix "&&"

lineComment :: Doc -> Doc
lineComment x = "//" <+> x


member :: Doc -> Doc -> Doc
member obj x = obj <> "." <> x

-- XXX: escape bad charcarcters and avoid keywords, etc.
ident :: Text -> Doc
ident = PP.text . Text.unpack

intLit :: Int -> Doc
intLit = PP.int

doubleLit :: Double -> Doc
doubleLit = PP.double

boolLit :: Bool -> Doc
boolLit b = if b then "true" else "false"

parens :: Doc -> Doc
parens = PP.parens

cond :: Doc -> Doc -> Doc -> Doc
cond x y z = x <+> "?" <+> y <+> ":" <+> z


--------------------------------------------------------------------------------
-- Types

auto :: Doc
auto = "auto"

void :: Doc
void = "void"

int :: Doc
int = "int"

double :: Doc
double = "double"

bool :: Doc
bool = "bool"


--------------------------------------------------------------------------------
-- Statements

stmt :: Doc -> Doc
stmt s = s <.> semi

stmts :: [Doc] -> Doc
stmts = vcat

include :: Doc -> Doc
include x = "#include <" <.>  x <.> ">"

declare :: Doc -> Doc -> Doc
declare t v = stmt (t <+> v)

declareInit :: Doc -> Doc -> Doc -> Doc
declareInit ty n v = stmt (ty <+> n <+> "=" <+> v)

assign :: Doc -> Doc -> Doc
assign n v = stmt (n <+> "=" <+> v)

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

while :: Doc -> [Doc] -> Doc
while p xs = "while" <+> parens p <+> "{" $$ nested xs $$ "}"

break :: Doc
break = stmt "break"

return :: Doc
return = stmt "return"

callStmt :: Doc -> [Doc] -> Doc
callStmt f xs = stmt (call f xs)

returnWith :: Doc -> Doc
returnWith e = stmt ("return" <+> e)


(-=) :: Doc -> Doc ->  Doc
x -= y = stmt (x <+> "-=" <+> y)

struct :: Doc -> [Doc] -> Doc
struct x ys = stmt ("struct" <+> x <+> "{" $$ nested ys $$ "}")

nop :: Doc
nop = empty
