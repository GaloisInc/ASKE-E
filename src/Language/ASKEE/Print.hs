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

printModelExpr :: ModelExpr -> Doc
printModelExpr e =
  case e of
    ArithExpr e -> printArithExpr e
    IfExpr e1 e2 e3 -> 
      (parens . hsep) [ text "if"
                      , printLogExpr e1
                      , text "then"
                      , printModelExpr e2 
                      , text "else"
                      , printModelExpr e3
                      ]
    CondExpr (Condition branches other) ->
      let decl = text ("cond:")
          branches' = vcat $ case other of
            Just e  -> (map (uncurry condBranch) branches) ++ [condOther e]
            Nothing -> (map (uncurry condBranch) branches)
      in  nest 2 decl $$ nest 4 branches' 

printArithExpr :: ArithExpr -> Doc
printArithExpr e = 
  case e of
    (Add e1 e2) -> binop e1 "+"   e2
    (Sub e1 e2) -> binop e1 "-"   e2
    (Mul e1 e2) -> binop e1 "*"   e2
    (Div e1 e2) -> binop e1 "/"   e2
    (Neg e1) -> parens (char '-' <> printArithExpr e1)
    (ALit d) -> double d
    (Var i) -> text (unpack i)
  
  where
    binop = expBinop printArithExpr

printLogExpr :: LogExpr -> Doc
printLogExpr e =
  case e of
    (GT e1 e2)  -> aBinop e1 ">"   e2
    (GTE e1 e2) -> aBinop e1 ">="  e2
    (EQ e1 e2)  -> aBinop e1 "=="  e2
    (LTE e1 e2) -> aBinop e1 "<="  e2
    (LT e1 e2)  -> aBinop e1 "<"   e2
    (And e1 e2) -> lBinop e1 "and" e2
    (Or e1 e2)  -> lBinop e1 "or"  e2
    (Not e1) -> 
      (parens . hsep) [ text "not"
                      , printLogExpr e1]
  
  where
    aBinop = expBinop printArithExpr
    lBinop = expBinop printLogExpr

condBranch :: ArithExpr -> LogExpr -> Doc
condBranch e1 e2 = 
  printArithExpr e1 <+>
  text "if" <+>
  printLogExpr e2

condOther :: ArithExpr -> Doc
condOther e =
  printArithExpr e <+>
  text "otherwise"

expBinop :: (a -> Doc) -> a -> String -> a -> Doc
expBinop print e1 op e2 = 
  (parens . hsep) [ print e1
                  , text op
                  , print e2
                  ]

printEvent :: Event -> Doc
printEvent (Event {..}) = decl $+$ nest 2 body
  where
    decl :: Doc
    decl = text "event" <+>
           text (unpack eventName) <>
           char ':'

    body :: Doc
    body = vcat [rate, when, effect]

    rate :: Doc
    rate = text "rate:" $+$ nest 2 (printModelExpr eventRate)

    when :: Doc
    when = case eventWhen of
      Nothing -> empty
      Just w -> text "when:" $+$ nest 2 (printLogExpr w)

    effect :: Doc
    effect = text "effect:" $+$ nest 2 statements
    
    statements :: Doc
    statements = vcat $ map (uncurry printAssign) eventEffect

    printAssign :: Text -> ArithExpr -> Doc
    printAssign ident exp = 
      hsep [ text (unpack ident)
           , char '='
           , printArithExpr exp]


printModel :: Model -> Doc
printModel (Model {..}) = decl $+$ nest 2 body
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
           , printModelExpr val
           ]
    printDecl (State name val) = 
      fsep [ text "state"
           , text (unpack name)
           , char '='
           , printModelExpr val
           ]
