{-# Language OverloadedStrings #-}
module Language.ASKEE.Experiment.TraverseType where

import Data.Text(Text)
import Data.Functor.Identity(Identity(..), runIdentity)
import Data.Functor.Const(Const(..), getConst)
import Language.ASKEE.Experiment.Syntax

collect :: (TraverseType t, Monoid m) => (Type -> m) -> t -> m
collect f t =
  getConst $ traverseType (Const . f) t

mapType :: TraverseType t => (Type -> Type) -> t -> t
mapType f t =
  runIdentity $ traverseType (Identity . f) t


-- data Const w a = Const w

-- instance Functor (Const w) where
--   fmap _ (Const c) = Const c

-- instance Monoid w => Applicative (Const w) where
--   pure _ = Const mempty
--   Const w <*> Const w2 = Const (w <> w2)

class TraverseType t where
  traverseType :: Applicative f => (Type -> f Type) -> t -> f t

instance TraverseType Text where
  traverseType _ = pure

instance (TraverseType s, TraverseType t) => TraverseType (s,t) where
  traverseType f (s,t) = (,) <$> traverseType f s <*> traverseType f t

instance TraverseType t => TraverseType (Maybe t) where
  traverseType f = traverse (traverseType f)

instance TraverseType t => TraverseType [t] where
  traverseType f = traverse (traverseType f)

-- NOTE: this jsut applies the functions to the type, it does NOT
-- travser the subtypes
instance TraverseType Type where
  traverseType = id

instance TraverseType TypedName where
  traverseType f name =
    (\mb -> name { tnType = mb }) <$> traverseType f (tnType name)

instance TraverseType Expr where
  traverseType f expr =
    case expr of
      Lit {}      -> pure expr
      Var i       -> Var <$> traverseType f i
      Call fu es  -> Call fu <$> traverseType f es
      Dot e l     -> (`Dot` l) <$> traverseType f e

instance TraverseType Stmt where
  traverseType f stmt =
    case stmt of
      Set x e   -> Set <$> traverseType f x <*> traverseType f e
      Let x e   -> Let <$> traverseType f x <*> traverseType f e
      If ts ys  -> If <$> traverseType f ts <*> traverseType f ys

instance TraverseType SampleExpr where
  traverseType f se =
    SampleExpr <$> traverseType f (seName se)
               <*> traverseType f (seArgs se)
               <*> traverseType f (seRange se)

instance TraverseType MeasureExpr where
  traverseType f me =
    MeasureExpr <$> traverseType f (meMeasureName me)
                <*> traverseType f (meDataset me)
                <*> traverseType f (meArgs me)
                <*> traverseType f (meTypeArgs me)

instance TraverseType MeasureDecl where
  traverseType f md =
    MeasureDecl <$> traverseType f (measureName md)
                <*> pure (measureTArgs md)
                <*> traverseType f (measureConstraints md)
                <*> traverseType f (measureArgs md)
                <*> traverseType f (measureVars md)
                <*> traverseType f (measureDataBinder md)
                <*> traverseType f (measureImpl md)

instance TraverseType ExperimentDecl where
  traverseType f ed =
    ExperimentDecl <$> traverseType f (experimentName ed)
                   <*> traverseType f (experimentArgs ed)
                   <*> traverseType f (experimentStmts ed)
                   <*> traverseType f (experimentReturn ed)

instance TraverseType ExperimentStmt where
  traverseType f stmt =
    case stmt of
      ESLet n e -> ESLet <$> traverseType f n <*> traverseType f e
      ESSample n se -> ESSample <$> traverseType f n <*> traverseType f se
      ESMeasure n me -> ESMeasure <$> traverseType f n <*> traverseType f me

instance TraverseType Decl where
  traverseType f decl =
    case decl of
      DMeasure m    -> DMeasure    <$> traverseType f m
      DExperiment e -> DExperiment <$> traverseType f e

instance TraverseType TypeConstraint where
  traverseType f c =
    case c of
      HasField recTy label fieldTy ->
        HasField <$> f recTy <*> pure label <*> f fieldTy

instance TraverseType MeasureType where
  traverseType f m =
    MeasureType <$> traverseType f (mtArgs m)
                <*> f (mtData m)
                <*> f (mtResult m)
