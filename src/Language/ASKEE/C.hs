{-# Language OverloadedStrings #-}
module Language.ASKEE.C where

import Data.Text(Text)

import Prettyprinter
          ( emptyDoc
          , indent,vcat,hsep,(<+>)
          , punctuate, comma, semi, colon
          , pretty
          )
import qualified Prettyprinter as PP


type Doc = PP.Doc ()

nested :: [Doc] -> Doc
nested = indent 2 . stmts


function :: Doc -> Doc -> [Doc] -> [Doc] -> Doc
function retTy name args xs =
  vcat [ retTy <+> name <> parens (hsep (punctuate comma args)) <+> "{"
       , nested xs
       , "}"
       ]

arg :: Doc -> Doc -> Doc
arg ty name = ty <+> name

refArg :: Doc -> Doc -> Doc
refArg ty name = ty <+> "&" <> name

ptrArg :: Doc -> Doc -> Doc
ptrArg ty name = ty <+> "*" <> name

--------------------------------------------------------------------------------
-- Expressions (kind of)

call :: Doc -> [Doc] -> Doc
call f args = f <> parens (hsep (punctuate comma args))

callCon :: Doc -> [Doc] -> Doc
callCon f args = f <> braces args

braces :: [Doc] -> Doc
braces = PP.braces . hsep . punctuate comma

callInfix :: Doc -> Doc -> Doc -> Doc
callInfix op x y = x <+> op <+> y

callPrefix :: Doc -> Doc -> Doc
callPrefix op x = op <+> x

not :: Doc -> Doc
not = callPrefix "!"


(+) :: Doc -> Doc -> Doc
(+) = callInfix "+"

(-) :: Doc -> Doc -> Doc
(-) = callInfix "-"

(*) :: Doc -> Doc -> Doc
(*) = callInfix "*"

(/) :: Doc -> Doc -> Doc
(/) = callInfix "/"

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
ident = pretty

intLit :: Int -> Doc
intLit = pretty

doubleLit :: Double -> Doc
doubleLit = pretty

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
stmt s = s <> semi

stmts :: [Doc] -> Doc
stmts = vcat

include :: Doc -> Doc
include x = "#include <" <>  x <> ">"

declare :: Doc -> Doc -> Doc
declare t v = stmt (t <+> v)

declareInit :: Doc -> Doc -> Doc -> Doc
declareInit ty n v = stmt (ty <+> n <+> "=" <+> v)

assign :: Doc -> Doc -> Doc
assign n v = stmt (n <+> "=" <+> v)

switch :: Doc -> [Doc] -> Doc
switch e cs = block ("switch" <+> parens e) cs

case' :: Doc -> Doc
case' c = "case" <+> c <> colon

ifThen :: Doc -> [Doc] -> Doc
ifThen c t = block ("if" <+> parens c) t

ifThenElse :: Doc -> [Doc] -> [Doc] -> Doc
ifThenElse c t e =
  vcat [ "if" <+> parens c <+> "{"
       , nested t
       , "} else {"
       , nested e
       , "}"
       ]

while :: Doc -> [Doc] -> Doc
while p xs = block ("while" <+> parens p) xs

doWhile :: [Doc] -> Doc -> Doc
doWhile xs p =
  stmt (block "do" xs <+> "while" <+> parens p)

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
struct x ys = stmt (block ("struct" <+> x) ys)

nop :: Doc
nop = emptyDoc

scope :: [Doc] -> Doc
scope ds = vcat [ "{", nested ds, "}" ]

block :: Doc -> [Doc] -> Doc
block pref ds = vcat [ pref <+> "{", nested ds, "}" ]

