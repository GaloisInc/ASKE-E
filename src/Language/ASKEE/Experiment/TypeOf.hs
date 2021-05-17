module Language.ASKEE.Experiment.TypeOf where

import qualified Data.Map as Map

import Language.ASKEE.Experiment.Syntax
import Language.ASKEE.Panic(panic)


-- | Compute the type of a type-checked thing.
class TypeOf t where
  typeOf :: t -> Type

instance TypeOf Literal where
  typeOf lit =
    case lit of
      LitNum {} -> TypeNumber
      LitBool {} -> TypeBool

instance TypeOf TypedName where
  typeOf = getType

instance TypeOf Expr where
  typeOf expr =
    case expr of
      Lit l -> typeOf l
      Var x -> typeOf x
      Dot _ _ ty ->
        case ty of
          Just t -> t
          Nothing -> panic "typeOf" ["Missing type in selector"]

      Call f es ->
        case f of
          Add -> TypeNumber
          Multiply -> TypeNumber
          Divide -> TypeNumber
          Subtract -> TypeNumber
          Negate -> TypeNumber
          Not -> TypeBool
          LessThan -> TypeBool
          GreaterThan -> TypeBool
          LessThanEqual -> TypeBool
          GreaterThanEqual -> TypeBool
          Equal -> TypeBool
          NotEqual -> TypeBool
          Or -> TypeBool
          And -> TypeBool
          Range -> TypeStream TypeNumber

      Point fields ->
        TypePoint (Map.fromList [ (x,typeOf e) | (x,e) <- fields ])


