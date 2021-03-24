{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.Print where

import Data.Text (unpack,  Text )

import Language.ASKEE.Syntax as Syntax
import Language.ASKEE.Expr as Expr

import Prelude hiding (GT, EQ, LT, (<>))

import Text.PrettyPrint

-- Notes:
-- More line spacing could be nice?

pyPrintExpr :: Expr -> Doc
pyPrintExpr expr = 
  case expr of
    (Add e1 e2) -> binop e1 "+"   e2
    (Sub e1 e2) -> binop e1 "-"   e2
    (Mul e1 e2) -> binop e1 "*"   e2
    (Div e1 e2) -> binop e1 "/"   e2
    (Exp e1) -> text "math.exp" <> parens (pyPrintExpr e1)
    (Log e1) -> text "math.log" <> parens (pyPrintExpr e1)
    (Neg e1) -> char '-' <> pp e1
    (LitD d) -> double d
    (Var i) -> text (unpack i)
    (GT e1 e2)  -> aBinop e1 ">"   e2
    (GTE e1 e2) -> aBinop e1 ">="  e2
    (EQ e1 e2)  -> aBinop e1 "=="  e2
    (LTE e1 e2) -> aBinop e1 "<="  e2
    (LT e1 e2)  -> aBinop e1 "<"   e2
    (And e1 e2) -> lBinop e1 "and" e2
    (Or e1 e2)  -> lBinop e1 "or"  e2
    (Not e1) -> 
      hsep  [ text "not"
            , pp e1]
    If e1 e2 e3 -> 
      hsep  [ pp e2 
            , text "if"
            , pp e1
            , text "else"
            , pp e3
            ]
    Cond branches other -> undefined
      -- let decl = text "cond:"
      --     branches' = vcat $ case other of
      --       Just e'  -> map (uncurry condBranch) branches ++ [condOther e']
      --       Nothing -> map (uncurry condBranch) branches
      -- in  nest 2 decl $$ nest 4 branches' 
    LitB True -> text "True"
    LitB False -> text "False"
  
  where
    binop = expBinop pp
    aBinop = expBinop pp
    lBinop = expBinop pp

    pp :: Expr -> Doc
    pp e = 
      if prec e <= prec expr
        then parens (pyPrintExpr e)
        else         pyPrintExpr e
        
    prec :: Expr -> Int
    prec e =
      case e of
        LitD _ -> 10
        LitB _ -> 10
        Neg _ -> 1
        Not _ -> 1
        Exp _ -> 1
        Log _ -> 1
        Add _ _ -> 6
        Sub _ _ -> 6
        Mul _ _ -> 7
        Div _ _ -> 7
        LT _ _ -> 4
        LTE _ _ -> 4
        EQ _ _ -> 4
        GTE _ _ -> 4
        GT _ _ -> 4
        And _ _ -> 3
        Or _ _ -> 3
        Var _ -> 10
        If {} -> 0
        Cond {} -> 0

    condBranch :: Expr -> Expr -> Doc
    condBranch e1 e2 = 
      pyPrintExpr e1 <+>
      text "if" <+>
      pyPrintExpr e2

    condOther :: Expr -> Doc
    condOther e =
      pyPrintExpr e <+>
      text "otherwise"


printExpr :: Expr -> Doc
printExpr expr = 
  case expr of
    (Add e1 e2) -> binop e1 "+"   e2
    (Sub e1 e2) -> binop e1 "-"   e2
    (Mul e1 e2) -> binop e1 "*"   e2
    (Div e1 e2) -> binop e1 "/"   e2
    (Exp e1) -> text "exp" <> pp e1
    (Log e1) -> text "log" <> pp e1
    (Neg e1) -> char '-' <> pp e1
    (LitD d) -> double d
    (Var i) -> text (unpack i)
    (GT e1 e2)  -> aBinop e1 ">"   e2
    (GTE e1 e2) -> aBinop e1 ">="  e2
    (EQ e1 e2)  -> aBinop e1 "=="  e2
    (LTE e1 e2) -> aBinop e1 "<="  e2
    (LT e1 e2)  -> aBinop e1 "<"   e2
    (And e1 e2) -> lBinop e1 "and" e2
    (Or e1 e2)  -> lBinop e1 "or"  e2
    (Not e1) -> 
      hsep  [ text "not"
            , pp e1]
    If e1 e2 e3 -> 
      hsep  [ text "if"
            , pp e1
            , text "then"
            , pp e2 
            , text "else"
            , pp e3
            ]
    Cond branches other ->
      let decl = text "cond:"
          branches' = vcat $ case other of
            Just e'  -> map (uncurry condBranch) branches ++ [condOther e']
            Nothing -> map (uncurry condBranch) branches
      in  nest 2 decl $$ nest 4 branches' 
    LitB True -> text "true"
    LitB False -> text "false"
  
  where
    binop = expBinop pp
    aBinop = expBinop pp
    lBinop = expBinop pp

    pp :: Expr -> Doc
    pp e = 
      if prec e < prec expr
        then parens (printExpr e)
        else         printExpr e
        
    prec :: Expr -> Int
    prec e =
      case e of
        LitD _ -> 10
        LitB _ -> 10
        Neg _ -> 1
        Not _ -> 1
        Exp _ -> 1
        Log _ -> 1
        Add _ _ -> 6
        Sub _ _ -> 6
        Mul _ _ -> 7
        Div _ _ -> 7
        LT _ _ -> 4
        LTE _ _ -> 4
        EQ _ _ -> 4
        GTE _ _ -> 4
        GT _ _ -> 4
        And _ _ -> 3
        Or _ _ -> 3
        Var _ -> 10
        If {} -> 0
        Cond {} -> 0

    condBranch :: Expr -> Expr -> Doc
    condBranch e1 e2 = 
      printExpr e1 <+>
      text "if" <+>
      printExpr e2

    condOther :: Expr -> Doc
    condOther e =
      printExpr e <+>
      text "otherwise"

expBinop :: (a -> Doc) -> a -> String -> a -> Doc
expBinop pr e1 op e2 = 
  hsep  [ pr e1
        , text op
        , pr e2
        ]

printEvent :: Event -> Doc
printEvent Event{..} = decl $+$ nest 2 body
  where
    decl :: Doc
    decl = text "event" <+>
           text (unpack eventName) <>
           char ':'

    body :: Doc
    body = vcat [when, rate, effect]

    rate :: Doc
    rate = text "rate:" $+$ nest 2 (printExpr eventRate)

    when :: Doc
    when = case eventWhen of
      Nothing -> empty
      Just w -> text "when:" $+$ nest 2 (printExpr w)

    effect :: Doc
    effect = text "effect:" $+$ nest 2 statements
    
    statements :: Doc
    statements = vcat $ map (uncurry printAssign) eventEffect

    printAssign :: Text -> Expr -> Doc
    printAssign ident e = 
      hsep [ text (unpack ident)
           , char '='
           , printExpr e]


printModel :: Model -> Doc
printModel Model{..} = decl $+$ nest 2 body
  where
    decl :: Doc
    decl = text "model" <+>
           text (unpack modelName) <>
           char ':'

    body :: Doc
    body = vcat (state ++ events)

    state :: [Doc]
    state = map printDecl modelDecls 

    events :: [Doc]
    events = map printEvent modelEvents

    printDecl :: Decl -> Doc
    printDecl (Let name val) = 
      fsep [ text "let"
           , text (unpack name)
           , char '='
           , printExpr val
           ]
    printDecl (State name val) = 
      fsep [ text "state"
           , text (unpack name)
           , char '='
           , printExpr val
           ]
    printDecl (Assert e) =
      fsep [ text "assert"
           , printExpr e
           ]
