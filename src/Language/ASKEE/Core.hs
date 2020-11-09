{-# Language FlexibleContexts #-}

module Language.ASKEE.Core where
import Data.Text(Text)

type Ident = Text

data Expr =
    ExprAdd    Expr Expr
  | ExprMul    Expr Expr
  | ExprSub    Expr Expr
  | ExprDiv    Expr Expr
  | ExprLT     Expr Expr
  | ExprEQ     Expr Expr
  | ExprGT     Expr Expr
  | ExprNot    Expr
  | ExprNeg    Expr
  | ExprIf     Expr Expr Expr
  | ExprAnd    Expr Expr
  | ExprOr     Expr Expr
  | ExprNumLit Double
  | ExprBoolLit Bool
  | ExprVar    Ident
  | ExprFail   String
  deriving (Show, Eq)

data Event =
  Event { eventName   :: Ident
        , eventRate   :: Expr
        , eventWhen   :: Expr
        , eventEffect :: [(Ident, Expr)]
        }
  deriving (Show, Eq)

data Model =
  Model { modelName :: Text
        , modelInitState :: [(Ident, Double)]
        , modelEvents    :: [Event]
        }
  deriving (Show, Eq)



