{-# Language OverloadedStrings, BlockArguments #-}
module Language.ASKEE.Experiment.CodeGen where

import Data.Text(Text)
import Prettyprinter(pretty,(<+>),vcat,hsep,punctuate,comma)

import Language.ASKEE.Panic(panic)
import qualified Language.ASKEE.C as C
import Language.ASKEE.Experiment.Syntax



compileMeasure :: MeasureDecl -> C.Doc
compileMeasure mdecl =
  vcat
  [ template
  , "class" <+> name <+> "{"
  , C.nested $ map attr         (measureArgs mdecl) ++
               map (attr . fst) (measureVars mdecl)
  , "public:"
  , C.nested [ con, addPoint ]
  ,"};"
  ]
  where
  name = measureCName (tnName (measureName mdecl))

  template =
    case measureTArgs mdecl of
      [] -> mempty
      as -> "template" <+> C.angles [ "typename" <+> tvarName a | a <- as ]

  attr b = C.declare (compileType (getType b)) (compileVar b)

  declareArg x  = C.arg (compileType (getType x)) (compileVar x)
  initArg x     = C.call (compileVar x) [compileVar x]
  initVar (x,e) = C.call (compileVar x) [compileExpr 15 e]

  con =
    vcat [ name <> C.argList (map declareArg (measureArgs mdecl))
         , ":" <+> hsep (punctuate comma (map initArg (measureArgs mdecl) ++
                                          map initVar (measureVars mdecl)))
         , "{}"
         ]

  addPoint =
    let vDB = measureDataBinder mdecl
    in C.function C.void "addPoint"
          [ C.refArg (compileType (getType vDB)) (compileVar vDB) ]
          (map compileStmt (measureImpl mdecl))

--------------------------------------------------------------------------------
-- Statements

compileStmt :: Stmt -> C.Doc
compileStmt stmt =
  case stmt of
    Set x e -> C.assign (compileVar x) (compileExpr 15 e)
    Let x e -> C.declareInit (compileType (getType x)) (compileVar x)
                                                       (compileExpr 15 e)
    If sThens sElse -> head $ foldr doThen (doStmts sElse) sThens
      where
      doThen (b,stmts) es = [C.ifThenElse (compileExpr 0 b) (doStmts stmts) es]
      doStmts             = map compileStmt
 


--------------------------------------------------------------------------------
-- Expressions
 
compileExpr :: Int -> Expr -> C.Doc
compileExpr prec expr =
  case expr of
    Lit l     -> compileLiteral l
    Var x     -> compileVar x
    Call f es -> compileCall prec f es
    Dot e l   -> C.member (compileExpr 1 e) (labelName l)
    -- Point [(Text, Expr)]

compileVar :: TypedName -> C.Doc
compileVar = varName . tnName

compileLiteral :: Literal -> C.Doc
compileLiteral lit =
  case lit of
    LitBool b -> C.boolLit b
    LitNum d  -> C.doubleLit d

compileCall :: Int -> FunctionName -> [Expr] -> C.Doc
compileCall prec f es =
  wrap
  case (f,es) of
    (Add, [e1,e2]) -> infOp "+" e1 e2
    (Multiply, [e1,e2]) -> infOp "*" e1 e2
    (Divide, [e1,e2]) -> infOp "/" e1 e2
    (Subtract, [e1,e2]) -> infOp "-" e1 e2
    (Negate, [e1]) -> uniOp "-" e1
    (Not, [e1]) -> uniOp "!" e1
    (LessThan, [e1,e2]) -> infOp "<" e1 e2
    (GreaterThan, [e1,e2]) -> infOp ">" e1 e2
    (LessThanEqual, [e1,e2]) -> infOp "<=" e1 e2
    (GreaterThanEqual, [e1,e2]) -> infOp ">=" e1 e2
    (Equal, [e1,e2]) -> infOp "==" e1 e2
    (NotEqual, [e1,e2]) -> infOp "!=" e1 e2
    (Or, [e1,e2]) -> infOp "||" e1 e2
    (And, [e1,e2]) -> infOp "&&" e1 e2
    (Range,_) -> C.callCon "Range" [ compileExpr 15 e | e <- es ]
    -- XXX: `Range` should be in a separate namespace?

    _ -> panic "compileCall" [ "Unexpected call", show f, show es ]

  where
  wrap        = if thisPrec < prec then id else C.parens
  infOp x a b = compileExpr thisPrec a <+> x <+> compileExpr thisPrec b
  uniOp x a   = x <> compileExpr thisPrec a

  -- lower number is higher precedence!
  thisPrec =
    case f of
      Add               -> 4
      Multiply          -> 3
      Divide            -> 3
      Subtract          -> 4
      Negate            -> 2
      Not               -> 2
      LessThan          -> 6
      GreaterThan       -> 6
      LessThanEqual     -> 6
      GreaterThanEqual  -> 6
      Equal             -> 7
      NotEqual          -> 7
      Or                -> 12
      And               -> 11
      Range             -> 0


------------------------------------------------------------------------------
-- Types

compileTVar :: TypeVar -> C.Doc
compileTVar tv =
  case tv of
    TVFree {} -> panic "compileTVar" ["Unexpteced free type variable"]
    TVBound n -> tvarName n

compileType :: Type -> C.Doc
compileType ty =
  case ty of
    TypeNumber -> C.double
    TypeBool   -> C.bool
    TypeVar tv -> compileTVar tv
{-
  | TypeStream Type -- stream/dataset
  | TypePoint (Map Text Type)
  -- | TypeCallable [Type] Type
-}
 
--------------------------------------------------------------------------------
-- Names

tvarName :: Int -> C.Doc
tvarName n = "T" <> C.intLit n

-- XXX: escape
varName :: Text -> C.Doc
varName = pretty

-- XXX: escape
labelName :: Text -> C.Doc
labelName = pretty

-- XXX: escape
measureCName :: Text -> C.Doc
measureCName = pretty


