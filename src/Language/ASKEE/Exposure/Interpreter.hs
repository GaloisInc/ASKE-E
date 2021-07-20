{-# Language BangPatterns #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}
module Language.ASKEE.Exposure.Interpreter where

import qualified Data.Aeson as JSON

import Control.Monad.IO.Class
import GHC.Float.RealFracMethods (floorDoubleInt)
import qualified Control.Monad.State as State
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Except as Except
import Control.Applicative((<|>), Alternative(..))
import Data.Foldable(traverse_)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(isJust)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Vector.Storable as VS
import qualified Numeric.GSL.Interpolation as Interpolation
import Data.Maybe(catMaybes)
import Witherable (Witherable(..))


import qualified Language.ASKEE.Core as Core
import qualified Language.ASKEE.Model as Model
import qualified Language.ASKEE.Model.Basics as MB
import qualified Language.ASKEE.DataSeries as DS
import qualified Language.ASKEE.CPP as CPP

import Language.ASKEE.Exposure.Syntax
import Data.List (transpose)

data ExposureInfo = ExposureInfo
  deriving Show

runEval :: EvalRead -> Env -> Eval a -> IO (Either Text a, Env, EvalWrite)
runEval evRead env ev = RWS.runRWST (Except.runExceptT (unEval ev)) evRead env

-------------------------------------------------------------------------------

evalStmts :: [Stmt] -> Env -> IO (Either Text ([DisplayValue], [StmtEff]), Env)
evalStmts stmts env = evalLoop env stmts

initialEnv :: Env
initialEnv = Env Map.empty Map.empty

evalLoop :: Env -> [Stmt] -> IO (Either Text ([DisplayValue], [StmtEff]), Env)
evalLoop env stmts = go emptyEvalRead
  where
    go m =
         do (lr, env', w) <- runEval m env $ interpretStmt `traverse_` stmts
            case lr of
              Left e -> pure (Left e, env')
              Right _
                | ewDeps w == Set.empty ->
                    pure (Right (ewDisplay w, ewStmtEff w), env')
                | otherwise ->
                    do  evs <- simulate (ewDeps w)
                        case evs of
                          Left e -> pure (Left e, env')
                          Right vs -> go (m <> EvalRead vs)

-------------------------------------------------------------------------------


simulate :: Set Value -> IO (Either Text (Map Value Value))
simulate vs =
  pure $ Left (Text.unlines $ "simulation is not implemented:":
                              (Text.pack . show <$> Set.toList vs))

-------------------------------------------------------------------------------
-- eval monad
data Env = Env
  { envTopLevelVars :: Map Ident Value
  , envLocalVars    :: Map Ident Value
  }
data EvalWrite = EvalWrite
  { ewDeps :: Set Value
  , ewDisplay :: [DisplayValue]
  , ewStmtEff :: [StmtEff]
  }

instance Semigroup EvalWrite where
  a <> b =
        EvalWrite (ewDeps a <> ewDeps b)
                  (ewDisplay a <> ewDisplay b)
                  (ewStmtEff a <> ewStmtEff b)

instance Monoid EvalWrite where
  mempty = EvalWrite Set.empty [] []

setEnv :: Env -> Eval ()
setEnv = RWS.put

writeStmtEff :: StmtEff -> Eval ()
writeStmtEff e =
  RWS.tell (EvalWrite Set.empty [] [e])

displayValue :: Value -> Eval ()
displayValue v =
  RWS.tell (EvalWrite Set.empty [DisplayValue v] [])

suspend :: Value -> Eval Value
suspend v
  | v == VSuspended = pure VSuspended
  | otherwise =
    do  mbV <- RWS.asks (Map.lookup v . erPrecomputed)
        case mbV of
          Nothing ->
            do RWS.tell (EvalWrite (Set.singleton v) [] [])
               pure VSuspended
          Just v' -> pure v'

data EvalRead = EvalRead
  { erPrecomputed :: Map Value Value
  }

emptyEvalRead :: EvalRead
emptyEvalRead = EvalRead Map.empty

instance Semigroup EvalRead where
  a <> b = EvalRead (erPrecomputed a <> erPrecomputed b)

instance Monoid EvalRead where
  mempty = EvalRead mempty

data StmtEff =
  StmtEffBind Ident Value

newtype Eval a = Eval { unEval :: Except.ExceptT Text (RWS.RWST EvalRead EvalWrite Env IO) a }
  deriving newtype ( Functor, Applicative, Monad, MonadIO
                   , RWS.MonadReader EvalRead
                   , Except.MonadError Text
                   , RWS.MonadState Env
                   , RWS.MonadWriter EvalWrite
                   , Alternative
                   )

-- TODO: catch exceptions
io :: IO a -> Eval a
io = liftIO

throw :: Text -> Eval a
throw = Except.throwError

notImplemented :: Text -> Eval a
notImplemented what = throw ("not implemented: " <> what)

getVarValue :: Ident -> Eval Value
getVarValue i =
  do  mbV <- State.gets $ \env -> Map.lookup i (envLocalVars env) <|> Map.lookup i (envTopLevelVars env)
      case mbV of
        Nothing -> throw ("variable " <> i <> " is not defined")
        Just v -> pure v

bindTopLevelVar :: Ident -> Value -> Eval ()
bindTopLevelVar i v =
  do  mbV <- State.gets (Map.lookup i . envTopLevelVars)
      case mbV of
        Nothing ->
          State.modify (\env -> env { envTopLevelVars = Map.insert i v (envTopLevelVars env) })
        Just _ ->
          throw ("variable " <> i <> " is already bound here")

withLocalVar :: Ident -> Value -> Eval a -> Eval a
withLocalVar i v thing =
  do  mbV <- State.gets $ \env -> Map.lookup i (envLocalVars env) <|> Map.lookup i (envTopLevelVars env)
      case mbV of
        Nothing ->
          do  State.modify $ \env -> env { envLocalVars = Map.insert i v (envLocalVars env) }
              thingInside <- thing
              State.modify $ \env -> env { envLocalVars = Map.delete i (envLocalVars env) }
              pure thingInside
        Just _ ->
          throw ("variable " <> i <> " is already bound here")

-------------------------------------------------------------------------------

instance JSON.ToJSON ExposureInfo where
  toJSON ExposureInfo = JSON.object []

interpretStmt :: Stmt -> Eval ()
interpretStmt stmt =
  case stmt of
    StmtLet var expr     ->
      do  eval <- interpretExpr expr
          writeStmtEff (StmtEffBind var eval)
          bindTopLevelVar var eval

    StmtDisplay dispExpr -> interpretDisplayExpr dispExpr

interpretExpr :: Expr -> Eval Value
interpretExpr e0 =
  case e0 of
    EVar var         -> getVarValue var
    EVal v           -> pure v

    ECall fun args   ->
      do  args' <- interpretExpr `traverse` args
          if VSuspended `elem` args'
            then pure VSuspended
            else interpretCall fun args'

    ECallWithLambda fun args lambdaVar lambdaExpr ->
      do args' <- interpretExpr `traverse` args
         interpretCallWithLambda fun args' lambdaVar lambdaExpr

    -- TODO: will we also need suspend for this?
    EMember e lab ->
      do  v <- interpretExpr e
          case v of
            -- TODO: typecheck model expr
            VModelExpr m -> pure $ VModelExpr (EMember m lab)
            _ -> mem v lab

    EList es ->
      do  vs <- interpretExpr `traverse` es
          pure $ VArray vs

    EListRange start stop step ->
      do  start' <- interpretExpr start
          stop'  <- interpretExpr stop
          step'  <- interpretExpr step
          case (start', stop', step') of
            (VDouble startD, VDouble stopD, VDouble stepD) ->
              pure $ VArray $ map VDouble [startD, startD + stepD .. stopD]
            _ -> typeErrorArgs [start', stop', step'] "all values in a range should be doubles"

interpretDisplayExpr :: DisplayExpr -> Eval ()
interpretDisplayExpr (DisplayScalar scalar) = do
  v <- interpretExpr scalar
  displayValue v


interpretCall :: FunctionName -> [Value] -> Eval Value
interpretCall fun args =
  case fun of
    FAdd         -> compilable (binarith (+))
    FSub         -> compilable (binarith (-))
    FMul         -> compilable (binarith (*))
    FDiv         -> compilable (binarith (/))
    FGT          -> compilable (bincmpDouble (>))
    FGTE         -> compilable (bincmpDouble (>=))
    FLT          -> compilable (bincmpDouble (<))
    FLTE         -> compilable (bincmpDouble (<=))
    FEQ          -> compilable (bincmpDouble (==))
    FNEQ         -> compilable (bincmpDouble (/=))
    FNot         -> compilable (unaryBool not)
    FAnd         -> compilable (bincmpBool (&&))
    FOr          -> compilable (bincmpBool (||))
    FProb        ->
      case args of
        [VArray vs] -> do
          vs' <- traverse getBoolValue vs
          pure $ VDouble $ fromIntegral (count id vs') / fromIntegral (length vs)
        _ -> typeError "P expects a fold and a double as its arguments"
    FSample      ->
      case args of
        [VDFold df e, VDouble d] -> execSim (SFSample d) df e >>= interpretExpr
        _ -> typeError "sample expects a fold and a double as its arguments"
    FAt          ->
      case args of
        [VModelExpr e, VDouble d] -> pure $ VDFold (DFAt d) e
        [VModelExpr e, times@(VArray _)] ->
          do  times' <- array double times
              pure $ VDFold (DFAtMany times') e
        [VArray vs, VDouble d] -> do
          -- TODO: Using getArrayContents here is gross. We should consider
          -- restructuring things to avoid assuming the level of VArray nesting.
          vs' <- traverse getArrayContents vs
          VArray <$> traverse (\v -> atPoint v d) vs'
        [v1, times@(VArray _)] -> atPoints v1 times
        _ -> typeError "at expects a model and a double as its arguments"

    FLoadEasel   ->
      case args of
        [VString path] ->
          do  eitherVal <-
                io $
                  do  src <- Text.readFile (Text.unpack path)
                      let m = Model.parseModel MB.EaselType src >>= Model.toCore
                      pure (VModelExpr . EVal . VModel <$> m)
              case eitherVal of
                Left err -> throw (Text.pack err)
                Right m -> pure m
        _ -> typeError "loadESL expects a single string argument"

    FLoadCSV ->
      case args of
        [VString path] ->
          do ds <- io $ DS.parseDataSeriesFromFile $ Text.unpack path
             pure $ VModelExpr $ EVal $ VDataSeries ds
        _ -> typeError "loadCSV expects a single string argument"

    FMean ->
      case args of
        [v] -> chooseLift (doubleArraySummarize mean) (typeError "mean") v
        _ -> typeError "mean"

    FMin ->
      case args of
        [v] -> chooseLift (doubleArraySummarize minimum) (typeError "min") v
        _ -> typeError "min"

    FMax ->
      case args of
        [v] -> chooseLift (doubleArraySummarize maximum) (typeError "max") v
        _ -> typeError "max"


    FInterpolate ->
      case args of
        [arg1, arg2] -> interpretInterpolate arg1 arg2
        _            -> typeError "interpolate expects exactly two arguments"

    FHistogram ->
      case args of
        [series, nbins] -> interpretHistogram series nbins
        _               -> typeError "histogram expects exactly two arguments"

    FTimedTime ->
      case args of
        [VTimed _ t] -> pure $ VDouble t
        [v@(VArray _)] ->
          (VArray . fmap (VDouble . snd) <$> array (timed value) v) <|> typeError "time"
        _ -> typeError "time"

    FTimedValue ->
      case args of
        [VTimed a _] -> pure a
        [v@(VArray _)] ->
          (VArray . fmap fst <$> array (timed value) v) <|> typeError "time"
        _ -> typeError "time"

    FIn ->
      case args of
        [VModelExpr e, VDouble start, VDouble end]
          | start < end -> pure $ VDFold (DFIn start end) e
          | otherwise   -> typeError "in 'in' - start time is before end time"
        [va@(VArray _), VDouble start, VDouble end] ->
          tryLiftArrayInto va $
            \v -> do  vs <- array (timed value) v
                      let vs' = filter (\(_, t) -> start <= t && t <= end) vs
                      pure $ VArray $ uncurry VTimed <$> vs'
          <|> typeError "invalid array argument to 'in'"
        _ -> typeError "in"

    FPlot ->
      case args of
        v1:v2:v3:rest -> interpretPlot v1 v2 v3 rest
        _   -> typeError "plot expects an array of points"

    FScatter ->
      case args of
        v1:v2:v3:rest -> interpretScatter v1 v2 v3 rest
        _   -> typeError "plot expects an array of points"

  where

    doubleArraySummarize f v =
      do  v' <- array double v
          pure $ VDouble (f v')

    typeError = typeErrorArgs args

    getMexpr (VModelExpr e) = Just e
    getMexpr _ = Nothing
    isMexpr e = isJust (getMexpr e)

    asMexprArg (VModelExpr e) = e
    asMexprArg v = EVal v

    liftLeftBin f =
      case args of
        [l, r] -> liftPrim' (\l' -> f l' r) l
        _ -> pure Nothing

    liftRightBin f =
      case args of
        [l, r] -> liftPrim' (\r' -> f l r') r
        _ -> pure Nothing

    liftBin g err =
      do  l1 <- liftLeftBin g
          l2 <- liftRightBin g
          case l1 <|> l2 of
            Just v -> pure v
            _      -> throw err

    bincmpDoubleMb f v1 v2 =
      case (v1, v2) of
        (VDouble d1, VDouble d2) -> pure . Just $ VBool (f d1 d2)
        _ -> pure Nothing

    bincmpDouble f = liftBin (bincmpDoubleMb f) "Cannot compare these (non-boolean) values"

    bincmpBoolMb f v1 v2 =
      case (v1, v2) of
        (VBool b1, VBool b2) -> pure . Just $ VBool (f b1 b2)
        _ -> pure Nothing

    bincmpBool f = liftBin (bincmpBoolMb f) "Cannot compare these (non-boolean) values"

    binarithMb f v1 v2 =
      case (v1, v2) of
        (VDouble d1, VDouble d2) -> pure . Just $ VDouble (f d1 d2)
        _ -> pure Nothing

    binarith f = liftBin (binarithMb f) "Cannot do arithmetic on these values"

    unaryBool f =
      case args of
        [v] ->
          v `liftInto` \case
            VBool b -> pure $ VBool (f b)
            _ -> typeError "Expected a single boolean argument"
        _ -> typeError "Expected a single boolean argument"


    compilable orElse =
      if any isMexpr args
        then pure $ VModelExpr (ECall fun (asMexprArg <$> args))
        else orElse

interpretInterpolate :: Value -> Value -> Eval Value
interpretInterpolate (VModelExpr (EVal arg1)) arg2 =
  interpretInterpolate arg1 arg2
interpretInterpolate (VDataSeries (DS.DataSeries{DS.times = oldTimes, DS.values = oldValueMap}))
                     (VArray points) =
  do pointDs <- getDoubleValue `traverse` points
     pure $ VDataSeries $ DS.DataSeries
       { DS.times = pointDs
       , DS.values = Map.map (\oldValues ->
                               map (Interpolation.evaluateV Interpolation.Linear
                                                            (VS.fromList oldTimes)
                                                            (VS.fromList oldValues))
                                   pointDs)
                             oldValueMap
       }
interpretInterpolate arg1 arg2 =
  typeErrorArgs [arg1, arg2] "interpolate expects a data series and an array as arguments"

interpretHistogram :: Value -> Value -> Eval Value
interpretHistogram (VModelExpr e) nBins =
  do v <- interpretExpr e
     interpretHistogram v nBins
interpretHistogram (VDataSeries ds) nBins =
  interpretHistogram (seriesAsPoints ds) nBins
interpretHistogram (VArray vs) n =
  do ds <- traverse asDouble vs
     nBins <- case n of
                   VInt i    -> pure i
                   VDouble d -> pure $ floorDoubleInt d
                   _ -> typeErrorArgs [n] "second argument of histogram must be a positive integer"
     let maxVal   = maximum ds
         minVal   = minimum ds
         size     = (maxVal - minVal) / fromIntegral nBins
         -- min catches values on the boundary
         toBin v  = min (floorDoubleInt ((v - minVal) / size)) (nBins-1)
         binned   = toBin <$> ds

     if size == 0 then
       -- degenerate case when everything fits into one bin
       let (lo, hi) = if maxVal >= 0 then (0, maxVal) else (maxVal, 0)
       in return (VHistogram lo hi (abs maxVal) (Map.singleton 0 (length vs)))
      else
       return (VHistogram minVal maxVal size (buildHist binned))

  where
    asDouble (VDouble d) = pure d
    asDouble (VInt i) = pure $ fromIntegral i
    asDouble (VTimed v _) = asDouble v
    asDouble _ = throw "histogram expects an array of numeric values"
    -- Let's do the obvious thing for now
    buildHist :: [Int] -> Map Int Int
    buildHist = foldr (Map.alter incBin) mempty

    incBin Nothing  = Just 1
    incBin (Just c) = Just (c + 1)
interpretHistogram vs n =
  typeErrorArgs [vs, n] "interpretHistogram"

interpretPlot :: Value -> Value -> Value -> [Value] -> Eval Value
interpretPlot xs yss xlab optLabels =
  do labels <- traverse str optLabels
     xlab'  <- str xlab
     xs'    <- array double xs
     yss'   <- array (array double) yss

     let rest   = take needed ["y" <> Text.pack (show i) | i <- [length yss'..]]
         needed = length yss'  - length labels

     return $ VPlot xlab' (labels ++ rest) xs' (transpose yss')


interpretScatter :: Value -> Value -> Value -> [Value] -> Eval Value
interpretScatter xs ysss xlab optLabels =
  do labels <- traverse str optLabels
     xlab'  <- str xlab
     xs'    <- array double xs
     yss'   <- array (array (array double)) ysss
     let rest   = take needed ["y" <> Text.pack (show i) | i <- [length yss'..]]
         needed = length yss'  - length labels
     return $ VScatter xlab'  (labels ++ rest) xs' yss'


interpretCallWithLambda :: FunctionWithLambdaName -> [Value] -> Ident -> Expr -> Eval Value
interpretCallWithLambda fun args lambdaVar lambdaExpr =
  case fun of
    FFilter ->
      case args of
        [VArray vs] -> VArray <$> wither filterAction vs
        _           -> typeError "filter"
  where
    typeError = typeErrorArgs args

    filterAction :: Value -> Eval (Maybe Value)
    filterAction arrayVal = withLocalVar lambdaVar arrayVal $
      do  lambdaVal <- interpretExpr lambdaExpr
          b <- getBoolValue lambdaVal
          pure $ if b then Just arrayVal else Nothing

typeErrorArgs :: [Value] -> Text -> Eval a
typeErrorArgs _args msg = throw $ Text.unlines
  [ "type error: " <> msg
  {- Uncomment the line below if you want a _lot_ of debugging output -}
  , "arguments: " <> Text.pack (show _args)
  ]

-- run a single simulation - emitting a new (evaulable expr)
execSim :: SampleFold -> DynamicalFold -> Expr -> Eval Expr
execSim sf df e = unfoldS . unfoldD <$> runSimExpr sampleCount expLength e
  where
    expLength =
      case df of
        DFAt d -> d
        DFAtMany d -> maximum d
        DFIn _ end -> end

    sampleCount =
      case sf of
        SFProbability d -> floor d
        SFSample d -> floor d

    unfoldD e1 =
      case df of
        DFAt d -> ECall FAt [e1, EVal $ VDouble d]
        DFAtMany ds -> ECall FAt [e1, EVal $ VArray (VDouble <$> ds)]
        DFIn start end -> ECall FIn [e1, EVal $ VDouble start, EVal $ VDouble end]
    unfoldS e1 =
      case sf of
        SFProbability n -> ECall FProb [e1, EVal $ VDouble n]
        SFSample _ -> e1

-- TODO: generalized traversal?
runSimExpr :: Int -> Double -> Expr -> Eval Expr
runSimExpr n t e0 =
  case e0 of
    EVal (VModel m) -> EVal <$> runSim n t m
    EVal _ -> pure e0
    EVar _ -> pure e0
    ECall fname args -> ECall fname <$> runSimExpr n t `traverse` args
    ECallWithLambda fname args lambdaVar lambdaExpr ->
      ECallWithLambda fname <$> (runSimExpr n t `traverse` args)
                            <*> pure lambdaVar
                            <*> (runSimExpr n t lambdaExpr)
    EMember e lab -> EMember <$> runSimExpr n t e <*> pure lab
    EList es -> EList <$> runSimExpr n t `traverse` es
    EListRange start stop step -> EListRange <$> runSimExpr n t start <*> runSimExpr n t stop <*> runSimExpr n t step

runSim :: Int -> Double -> Core.Model -> Eval Value
runSim n t mdl =
  do  series <- io mkSeries
      pure $ VArray (seriesAsPoints <$> series)
  where
    -- TODO: we should just get the raw simulation data?
    mkSeries = CPP.simulate mdl 0 t (t/100) Nothing n

seriesAsPoints :: DS.DataSeries Double -> Value
seriesAsPoints ds = VArray (pointToValue <$> DS.toDataPoints ds)
  where
    pointToValue p =
      let vals = VPoint (VDouble <$> DS.ptValues p)
      in VTimed vals (DS.ptTime p)


getArrayContents :: Value -> Eval [Value]
getArrayContents v =
  case v of
    VArray arr -> pure arr
    _ -> throw "Cannot use non-array value as array"

getTimedValue :: Value -> Eval (Value, Double)
getTimedValue v =
  case v of
    VTimed v' t -> pure (v',t)
    _ -> throw "Value is not timed"

getBoolValue :: Value -> Eval Bool
getBoolValue v =
  case v of
    VBool b -> pure b
    _ -> throw "Value is a not a boolean"

getDoubleValue :: Value -> Eval Double
getDoubleValue v =
  case v of
    VDouble d -> pure d
    _ -> throw "Value is not a double"

-- this presumes quite a few things
atPoints :: Value -> Value -> Eval Value
atPoints vals times =
  do  pts <- array (array (timed value)) vals
      times' <- array double times
      pure $ VArray (atArrs pts <$> times')
  where
    atArrs arrA t = VTimed (VArray . catMaybes $ (`atSimple` t) <$> arrA) t

atSimple :: [(a, Double)] -> Double -> Maybe a
atSimple lst t =
  case lst of
    []     -> Nothing
    (e, t'):_ | t <= t' -> Just e
    _:lst' -> atSimple lst' t

atPoint :: [Value] -> Double -> Eval Value
atPoint vs t =
  case vs of
    [] -> throw "cannot 'at' on an empty sequence"
    e:r ->
      do tv <- getTimedValue e -- TODO: I think the assumption is that the first
                               -- value is at 0 or something similar?
         go r (fst tv)
  where
    go vs' v =
      case vs' of
        [] -> pure v
        v':r ->
          do (val, t') <- getTimedValue v'
             if t' > t
               then pure v
               else go r val

mem :: Value -> Ident -> Eval Value
mem v0 l = liftPrim getMem v0
  where
    getMem v =
      case v of
        VPoint p ->
          case Map.lookup l p of
            Nothing -> throw ("cannot find member '" <> l <> "' in point")
            Just v' -> pure v'
        VTimed _ t | l == "time" -> pure $ VDouble t
        _ -> throw "type does not support membership"

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- Count the number of times a predicate is true
count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs


-------------------------------------------------------------------------------
-- typing

chooseOr :: [Eval a] -> Eval a -> Eval a
chooseOr e r = foldr (<|>) r e

array :: (Value -> Eval a) -> Value ->  Eval [a]
array f v0 =
  case v0 of
    VArray vs -> f `traverse` vs
    _ -> throw "Expecting array"

parallelArrays :: (Value -> Eval a) -> Value -> Value ->  Eval [(a,a)]
parallelArrays f v1 v2 =
  do  es1 <- array f v1
      es2 <- array f v2
      if length es1 == length es2
        then pure (es1 `zip` es2)
        else throw "Expecting arrays of same length"


double :: Value -> Eval Double
double v0 =
  case v0 of
    VDouble d -> pure d
    _ -> throw "Expecting number"

str :: Value -> Eval Text
str v0 =
  case v0 of
    VString t -> pure t
    _ -> throw "Expecting number"

timed :: (Value -> Eval a) -> Value ->  Eval (a, Double)
timed f v0 =
  case v0 of
    VTimed a d ->
      do  a' <- f a
          pure (a', d)
    _ -> throw "Expecting timed value"

modelExpr :: Value -> Eval Expr
modelExpr v0 =
  case v0 of
    VModelExpr e -> pure e
    _ -> throw "Expecting model"

value :: Value -> Eval Value
value = pure

timedLift :: (Value -> Eval Value) -> Value -> Eval (Value, Value -> Value)
timedLift f v =
  case v of
    VTimed a t ->
      do  a' <- f a
          pure (a', (`VTimed` t))
    e ->
      do  e' <- f e
          pure (e', id)

-------------------------------------------------------------------------------
-- lifts

chooseLiftInto :: Value -> (Value -> Eval Value) -> Eval Value -> Eval Value
chooseLiftInto v0 f orElse = chooseLift f orElse v0

chooseLift :: (Value -> Eval Value) -> Eval Value -> Value -> Eval Value
chooseLift f orElse v0 =
  f v0 <|>
  case v0 of
    VTimed v' t -> VTimed <$> chooseLift f orElse v' <*> pure t
    VArray vs -> VArray <$> (chooseLift f orElse `traverse` vs)
    _ -> orElse

liftInto :: Value -> (Value -> Eval Value) -> Eval Value
liftInto = flip liftPrim

liftPrim :: (Value -> Eval Value) -> Value -> Eval Value
liftPrim f v =
  case v of
    VTimed v' t -> VTimed <$> liftPrim f v' <*> pure t
    VArray vs -> VArray <$> (liftPrim f `traverse` vs)
    _ -> f v

liftPrim' :: Applicative f => (Value -> Eval (f Value)) -> Value -> Eval (f Value)
liftPrim' f v =
  case v of
    VTimed v' t ->
      do  v'' <- liftPrim' f v'
          pure (VTimed <$> v'' <*> pure t)
    VArray vs ->
      do  vs' <- liftPrim' f `traverse` vs
          pure $ VArray <$> sequenceA vs'
    _ -> f v

tryLiftArray :: (Value -> Eval Value) -> Value -> Eval Value
tryLiftArray f v0 =
  case v0 of
    VArray v -> (VArray <$> f `traverse` v) <|> f v0
    _ -> f v0

tryLiftArrayInto :: Value -> (Value -> Eval Value) -> Eval Value
tryLiftArrayInto = flip tryLiftArray
