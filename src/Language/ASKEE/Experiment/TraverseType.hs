{-# Language OverloadedStrings #-}
module Language.ASKEE.Experiment.TraverseType where

import Data.Text(Text)
import Language.ASKEE.Experiment.Syntax


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

instance TraverseType MeasureDecl where
  traverseType f md =
    MeasureDecl <$> traverseType f (measureName md)
                <*> traverseType f (measureArgs md)
                <*> traverseType f (measureVars md)
                <*> traverseType f (measureImpl md)

instance TraverseType ExperimentDecl where
  traverseType f ed =
    ExperimentDecl <$> traverseType f (experimentName ed)
                   <*> traverseType f (experimentArgs ed)
                   <*> traverseType f (experimentInputs ed)
                   <*> traverseType f (experimentMeasures ed)
                   <*> traverseType f (experimentLets ed)
                   <*> traverseType f (experimentYield ed)

instance TraverseType Decl where
  traverseType f decl =
    case decl of
      DMeasure m    -> DMeasure    <$> traverseType f m
      DExperiment e -> DExperiment <$> traverseType f e
