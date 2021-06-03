{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.Core.Print where

import qualified Data.Text as Text

import Language.ASKEE.Core.Expr
import Language.ASKEE.Panic       ( panic )

import qualified Text.PrettyPrint as PP
import           Text.Printf      ( printf )

ppExpr :: Expr -> PP.Doc
ppExpr expr =
  case expr of
    NumLit d -> PP.text $ printf "%f" d
    BoolLit b -> if b then "true" else "false"
    Op1 Neg e' -> "-"PP.<>pp e'
    Op1 Not e' -> "not"PP.<+>pp e'
    e1 :+: e2 -> PP.hsep [pp e1, "+", pp e2]
    e1 :-: e2 -> PP.hsep [pp e1, "-", pp e2]
    e1 :*: e2 -> PP.hsep [pp e1, "*", pp e2]
    e1 :/: e2 -> PP.hsep [pp e1, "/", pp e2]
    e1 :<: e2 -> PP.hsep [pp e1, "<", pp e2]
    e1 :<=: e2 -> PP.hsep [pp e1, "<=", pp e2]
    e1 :==: e2 -> PP.hsep [pp e1, "==", pp e2]
    e1 :&&: e2 -> PP.hsep [pp e1, "and", pp e2]
    e1 :||: e2 -> PP.hsep [pp e1, "or", pp e2]
    Var v -> PP.text $ Text.unpack v
    If e1 e2 e3 -> PP.hsep ["if", pp e1, "then", pp e2, "else", pp e3]
    Fail s -> error s -- XXX
    _ -> 
      panic 
        "encountered unknown Core expression when pretty-printing" 
        [ show expr ]

  where
    pp :: Expr -> PP.Doc
    pp e = 
      if prec e < prec expr
        then PP.parens (ppExpr e)
        else            ppExpr e

    prec :: Expr -> Int
    prec e =
      case e of
        NumLit  _ -> 10
        BoolLit _ -> 10
        Op1 Neg _ -> 10
        Op1 Not _ -> 10
        _ :+:   _ -> 6
        _ :-:   _ -> 6
        _ :*:   _ -> 7
        _ :/:   _ -> 7
        _ :<:   _ -> 4
        _ :<=:  _ -> 4
        _ :==:  _ -> 4
        _ :&&:  _ -> 3
        _ :||:  _ -> 3
        Var     _ -> 10
        If     {} -> 0
        Fail s    -> error s -- XXX
        _ -> 
          panic 
            "encountered unknown Core expression when pretty-printing" 
            [ "while determining precedence", show e ]
