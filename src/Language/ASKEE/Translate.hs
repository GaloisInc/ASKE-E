{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.Translate where

import Control.Monad                    ( (>=>) )

import Language.ASKEE.Core.DiffEq       ( asEquationSystem )
import Language.ASKEE.Core.ImportASKEE  ( modelAsCore )

import Language.ASKEE.DEQ.Print         ( ppDiffEqs )

import Language.ASKEE.GenLexer          ( lexModel )
import Language.ASKEE.GenParser         ( parseModel )

import Language.ASKEE.Latex.GenLexer    ( lexLatex )
import Language.ASKEE.Latex.GenParser   ( parseLatex )

import Language.ASKEE.Print             ( printModel )

import Language.ASKEE.RNet.GenLexer     ( lexRNet )
import Language.ASKEE.RNet.GenParser    ( parseRNet )
import Language.ASKEE.RNet.Reaction     ( reactionsAsModel )
import Language.ASKEE.RNet.Syntax       ( ReactionNet(..) )


{-

A somewhat double-dispatch/OO-flavored approach to cross-format translation.

-}

data ModelSyntax = ASKEE | DiffEq | RNet | Latex
  deriving Show

type Translator a = String -> Either String a

class AsASKEE a where
  asASKEEConcrete :: a -> Translator String
  -- asASKEEAbstract :: a -> Translator ASKEE.Model

class AsDiffEq a where
  asDiffEqConcrete :: a -> Translator String
  -- asDiffEqAbstract :: a -> Translator DEQ.DiffEqs

class AsRNet a where
  asRNetConcrete :: a -> Translator String
  -- asRNetAbstract :: a -> Translator ReactionNet

class AsLatex a where
  asLatexConcrete :: a -> Translator String

instance AsASKEE ModelSyntax where
  asASKEEConcrete syntax s =
    case syntax of
      ASKEE -> Right s
      DiffEq -> undefined
      RNet ->
        do  ReactionNet bs rs <- lexParse lexRNet parseRNet
            model <- reactionsAsModel "foo" bs rs
            pure $ show $ printModel model
      Latex -> undefined

    where
      lexParse :: (String -> Either String [a]) -> ([a] -> Either String b) -> Either String b
      lexParse l p = (l >=> p) s

instance AsDiffEq ModelSyntax where
  asDiffEqConcrete syntax s =
    case syntax of
      ASKEE ->
        do  model <- lexParse lexModel parseModel
            coreModel <- modelAsCore [] model
            let eqs = asEquationSystem coreModel
            pure $ show $ ppDiffEqs eqs
      DiffEq -> Right s
      RNet -> 
        do  ReactionNet bs rs <- lexParse lexRNet parseRNet
            model <- reactionsAsModel "foo" bs rs
            coreModel <- modelAsCore [] model 
            let eqs = asEquationSystem coreModel
            pure $ show $ ppDiffEqs eqs
      Latex -> 
        do  eqs <- lexParse lexLatex parseLatex
            pure $ show $ ppDiffEqs eqs

    where
      lexParse :: (String -> Either String [a]) -> ([a] -> Either String b) -> Either String b
      lexParse l p = (l >=> p) s

instance AsRNet ModelSyntax where
  asRNetConcrete syntax s =
    case syntax of
      ASKEE -> undefined
      DiffEq -> undefined
      RNet -> Right s
      Latex -> undefined

instance AsLatex ModelSyntax where
  asLatexConcrete syntax s =
    case syntax of
      ASKEE -> undefined
      DiffEq -> undefined
      RNet -> undefined
      Latex -> Right s