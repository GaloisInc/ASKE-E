module Language.ASKEE.Exposure.Syntax where

import qualified Data.Text as T
import Data.Text (Text)

type Binder = Text
type Ident  = Text
type Label  = Int

data Stmt
  = StmtLet Binder Expr
  | StmtDisplay DisplayExpr
  deriving Show

-- TODO: Labels?
newtype DisplayExpr
  = DisplayScalar Expr
  deriving Show

data Literal
  = LitNum Double
  | LitString Text
  deriving Show

data Expr
  = EVar Text
  | ELit Literal
  | ECall FunctionName [Expr]
  | EMember Expr Label
  deriving Show

data FunctionName
  = FAdd
  | FSub
  | FMul
  | FDiv
  | FGT
  | FGTE
  | FLT
  | FLTE
  | FEQ
  | FNEQ
  | FNot
  | FAnd
  | FOr
  | FProb
  | FSample
  | FAt
  | FMkArray
  | FMkIntervals
  | FLoadEasel
  deriving Show

prefixFunctionName :: Ident -> Either String FunctionName
prefixFunctionName ident =
  case T.unpack ident of
    "loadEasel" -> Right FLoadEasel
    strIdent    -> Left $ "Unsupported prefix function name: " ++ strIdent

infixFunctionName :: Ident -> Either String FunctionName
infixFunctionName ident =
  case T.unpack ident of
    "+"      -> Right FAdd
    "-"      -> Right FSub
    "*"      -> Right FMul
    "/"      -> Right FDiv
    ">"      -> Right FGT
    ">="     -> Right FGTE
    "<"      -> Right FLT
    "<="     -> Right FLTE
    "=="     -> Right FEQ
    "!="     -> Right FNEQ
    "&&"     -> Right FAnd
    "||"     -> Right FOr
    strIdent -> Left $ "Unsupported infix function name: " ++ strIdent
