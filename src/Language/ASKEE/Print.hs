{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.Print where

import Data.Text (unpack,  Text )

import Language.ASKEE.Syntax

import Prelude hiding (GT, EQ, LT, (<>))

import Text.PrettyPrint

-- Notes:
-- Doesn't currently print `cond` correctly
-- More line spacing could be nice?

printExp :: Exp -> Doc
printExp (Add e1 e2) = expBinop e1 "+"   e2
printExp (Sub e1 e2) = expBinop e1 "-"   e2
printExp (Mul e1 e2) = expBinop e1 "*"   e2
printExp (Div e1 e2) = expBinop e1 "/"   e2
printExp (GT e1 e2)  = expBinop e1 ">"   e2
printExp (GTE e1 e2) = expBinop e1 ">="  e2
printExp (EQ e1 e2)  = expBinop e1 "=="  e2
printExp (LTE e1 e2) = expBinop e1 "<="  e2
printExp (LT e1 e2)  = expBinop e1 "<"   e2
printExp (And e1 e2) = expBinop e1 "and" e2
printExp (Or e1 e2)  = expBinop e1 "or"  e2
printExp (Neg e1) = 
  parens (char '-' <> printExp e1)
printExp (Not e1) = 
  (parens . hsep) [ text "not"
                  , printExp e1]
printExp (If e1 e2 e3) = 
  (parens . hsep) [ text "if"
                  , printExp e1
                  , text "then"
                  , printExp e2 
                  , text "else"
                  , printExp e3
                  ]
printExp (Cond (Condition branches other)) =
  let decl = text ("cond:")
      branches' = vcat $ case other of
        Just e  -> (map (uncurry condBranch) branches) ++ [condOther e]
        Nothing -> (map (uncurry condBranch) branches)
  in  nest 2 decl $$ nest 4 branches' 
printExp (Real d) = double d
printExp (Var i) = text (unpack i)

condBranch :: Exp -> Exp -> Doc
condBranch e1 e2 = 
  printExp e1 <+>
  text "if" <+>
  printExp e2

condOther :: Exp -> Doc
condOther e =
  printExp e <+>
  text "otherwise"

expBinop :: Exp -> String -> Exp -> Doc
expBinop e1 op e2 = 
  (parens . hsep) [ printExp e1
                  , text op
                  , printExp e2
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
    rate = text "rate:" $+$ nest 2 (printExp eventRate)

    when :: Doc
    when = case eventWhen of
      Nothing -> empty
      Just w -> text "when:" $+$ nest 2 (printExp w)

    effect :: Doc
    effect = text "effect:" $+$ nest 2 statements
    
    statements :: Doc
    statements = vcat $ map (uncurry printAssign) eventEffect

    printAssign :: Text -> Exp -> Doc
    printAssign ident exp = 
      hsep [ text (unpack ident)
           , char '='
           , printExp exp]


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
           , printExp val
           ]
    printDecl (State name val) = 
      fsep [ text "state"
           , text (unpack name)
           , char '='
           , printExp val
           ]

sirModel :: Model
sirModel = Model name decls events
  where
    name = "SIR"
    decls = [ Let "beta" (Real 0.1)
            , Let "gamma" (Real 0.1)
            , State "Susceptible" (Real 999)
            , State "Infected" (Real 1)
            , State "Recovered" (Real 0)
            , Let "total_pop" (Add (Add (Var "Susceptible") (Var "Infected")) (Var "Recovered"))
            , Let "conditional" (Cond (Condition [(Real 1.0, Var "beta" `GTE` Real 0.1)] (Just $ Real 0)))
            ]
    events = [ infect
             , remove
             ]
    infect = 
      Event {
        eventName = "Infect",
        eventWhen =
          (Just (And (GT (Var "Susceptible") (Real 0))
                     (GT (Var "Infected")    (Real 0)))),
        eventRate = infectRate,
        eventEffect = 
          [ ("Susceptible", Sub (Var "Susceptible") (Real 1))
          , ("Infected",    Add (Var "Infected")    (Real 1))],
        eventMetadata = Nothing
      }

    remove = 
      Event {
        eventName = "Remove",
        eventWhen =
          (Just (GT (Var "Infected")    (Real 0))),
        eventRate = removeRate,
        eventEffect = 
          [ ("Infected",  Sub (Var "Infected")  (Real 1))
          , ("Recovered", Add (Var "Recovered") (Real 1))],
        eventMetadata = Nothing
      }

    infectRate = Div (Mul (Mul (Var "beta") 
                              (Var "Susceptible")) 
                          (Var "Infected"))
                     (Var "total_pop")
    removeRate = Mul (Var "gamma") (Var "Infected")