module Language.ASKEE.Exposure.TypeOf where

import qualified Data.Map as Map

import Language.ASKEE.Exposure.Syntax
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

      Call f _es ->
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
          Range -> TypeVector TypeNumber
          VMean -> TypeNumber

      Point fileds ->
        TypePoint (Map.fromList [ (x,typeOf e) | (x,e) <- fileds ])

      At slicedThing _slicer -> 
        case typeOf slicedThing of
          TypeRandomVar (TypeStream ty) -> TypeRandomVar (TypeVector ty)
          ty -> panic "typeOf"  [ "Expected sliced thing:"
                                , show slicedThing
                                , "to be a `TypeRandomVar (TypeStream a)`."
                                , "Instead, it was:"
                                , show ty
                                ]

      Measure _ mTool _ _ -> TypeRandomVar $ typeOf mTool

      Sample _sampleNum sampledThing ->
        case typeOf sampledThing of
          TypeRandomVar ty -> TypeVector ty
          ty -> panic "typeOf"  [ "Expected sampled thing:"
                                , show sampledThing
                                , "to be a `TypeRandomVar a`."
                                , "Instead, it was:"
                                , show ty
                                ]

      Probability _ -> TypeNumber