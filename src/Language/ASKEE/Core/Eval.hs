module Language.ASKEE.Core.Eval where

import Data.Map(Map)
import qualified Data.Map as Map

import Language.ASKEE.Panic(panic)
import Language.ASKEE.Core

evalDouble :: Expr -> Map Ident Double -> Double
evalDouble expr env =
  case expr of
    Var x -> env Map.! x

    Literal l ->
      case l of
        Num d -> d
        Bool {} -> typeError

    If c e1 e2 ->
      if evalBool c env then evalDouble e1 env else evalDouble e2 env

    Op1 op e ->
      case op of
        Neg -> negate (evalDouble e env)
        Not -> typeError

    Op2 op e1 e2 ->
      case op of
        Add -> evalDouble e1 env + evalDouble e2 env
        Mul -> evalDouble e1 env * evalDouble e2 env
        Sub -> evalDouble e1 env - evalDouble e2 env
        Div -> evalDouble e1 env / evalDouble e2 env
        Lt  -> typeError
        Leq -> typeError
        Eq  -> typeError
        And -> typeError
        Or  -> typeError

    Fail str -> error str

  where
  typeError = panic "evalDouble" [ "Expected double, but got a bool" ]


evalBool :: Expr -> Map Ident Double -> Bool
evalBool expr env =
  case expr of
    Literal l ->
      case l of
        Num _  -> typeError
        Bool b -> b

    Var {} -> typeError

    If c e1 e2 -> if evalBool c env then evalBool e1 env else evalBool e2 env

    Op1 op e ->
      case op of
        Neg -> typeError
        Not -> not (evalBool e env)

    Op2 op e1 e2 ->
      case op of
        Add -> typeError
        Mul -> typeError
        Sub -> typeError
        Div -> typeError
        Lt  -> evalDouble e1 env < evalDouble e2 env
        Leq -> evalDouble e1 env <= evalDouble e2 env
        Eq  -> evalDouble e1 env == evalDouble e2 env
        And -> evalBool e1 env && evalBool e2 env
        Or  -> evalBool e1 env || evalBool e2 env

    Fail str -> error str

  where
  typeError = panic "evalBool" [ "Expected bool, but got a double" ]
