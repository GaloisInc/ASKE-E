{-# Language BangPatterns #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.ASKEE.Exposure.Interpreter where

import qualified Data.Aeson as JSON

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad (unless, foldM, when, filterM)
import Control.Monad.IO.Class
import GHC.Float.RealFracMethods (floorDoubleInt)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Except as Except
import Control.Applicative((<|>), Alternative(..))
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import Data.Foldable(traverse_)
import qualified Data.List.Extra as List
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(isJust, catMaybes)
import Data.Text(Text)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy as Text (toStrict)
import Data.Function (on)
import Data.Set(Set)
import Data.List (transpose, sort, sortBy, groupBy)
import qualified Data.Set as Set
import qualified Data.Vector.Storable as VS
import qualified Numeric.GSL.Interpolation as Interpolation
import Witherable (Witherable(..))


import qualified Language.ASKEE as A
import qualified Language.ASKEE.Core as Core
import qualified Language.ASKEE.Core.Syntax as CoreS
import qualified Language.ASKEE.Core.Print as CorePP
import qualified Language.ASKEE.Model as Model
import qualified Language.ASKEE.Model.Basics as MB
import qualified Language.ASKEE.DataSeries as DS
import           Language.ASKEE.Core.Convert ( coreAsModel )
import qualified Language.ASKEE.Core.ModelVisualization as CoreViz
import qualified Language.ASKEE.CPP as CPP
import qualified Language.ASKEE.DEQ.Simulate as DEQ
import           Language.ASKEE.ESL.Convert ( modelAsCore )
import           Language.ASKEE.ESL.Manipulate ( join )
import           Language.ASKEE.Latex.Syntax (Latex(..))

import qualified Language.ASKEE.Exposure.Plot as Plot
import Language.ASKEE.Exposure.Plot (PlotStyle)
import Language.ASKEE.Exposure.Syntax
import Language.ASKEE.Exposure.Python

data ExposureInfo = ExposureInfo
  deriving Show

runEval :: EvalRead -> Env -> Eval a -> IO (Either Text a, Env, EvalWrite)
runEval evRead env ev =
  RWS.runRWST (Except.runExceptT (unEval ev)) evRead env

-------------------------------------------------------------------------------

initialEnv :: Env
initialEnv = Env Map.empty

evalLoop :: EvalRead -> Env -> [Stmt] -> IO (Either Text ([DisplayValue], [StmtEff]), Env)
evalLoop evr env stmts = go evr
  where
    go :: EvalRead -> IO (Either Text ([DisplayValue], [StmtEff]), Env)
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
                          Right vs -> go (m { erPrecomputed = erPrecomputed m <> vs })

-------------------------------------------------------------------------------


simulate :: Set Value -> IO (Either Text (Map Value Value))
simulate vs =
  pure $ Left (Text.unlines $ "simulation is not implemented:":
                              (Text.pack . show <$> Set.toList vs))

-------------------------------------------------------------------------------
-- eval monad
newtype Env = Env
  { envTopLevelVars :: Map Ident Value
  } deriving (Generic)
    deriving newtype NFData

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
  , erLocalVars   :: Map Ident Value
  , erPutFileFn   :: EvalWriteFileFn
  , erGetFileFn   :: EvalReadFileFn
  , erPyHandle    :: PythonHandle
  }

type EvalIO a        = IO (Either Text a) -- ^ Either an error message or the value
type EvalReadFileFn  = FilePath -> EvalIO LBS.ByteString
type EvalWriteFileFn = FilePath -> LBS.ByteString -> EvalIO ()

mkEvalReadEnv :: EvalReadFileFn -> EvalWriteFileFn -> PythonHandle -> EvalRead
mkEvalReadEnv rd wr hdl = EvalRead
  { erPrecomputed = Map.empty
  , erLocalVars   = Map.empty
  , erPutFileFn   = wr
  , erGetFileFn   = rd
  , erPyHandle    = hdl
  }


data StmtEff =
  StmtEffBind Ident Value
  deriving (Generic, NFData)

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

putFile :: FilePath -> LBS.ByteString  -> Eval ()
putFile f contents =
  do putF <- RWS.asks erPutFileFn
     res  <- io $ putF f contents
     case res of
       Left err -> throw err
       Right _  -> pure ()

getFile :: FilePath -> Eval LBS.ByteString
getFile f =
  do getF <- RWS.asks erGetFileFn
     res  <- io $ getF f
     case res of
       Left err -> throw err
       Right t  -> pure t

notImplemented :: Text -> Eval a
notImplemented what = throw ("not implemented: " <> what)

getVarValue :: Ident -> Eval Value
getVarValue i =
  do  localEnv    <- Reader.asks erLocalVars
      topLevelEnv <- State.gets envTopLevelVars
      case Map.lookup i localEnv <|> Map.lookup i topLevelEnv of
        Nothing -> throw ("variable " <> i <> " is not defined")
        Just v -> pure v

bindTopLevelVar :: Ident -> Value -> Eval ()
bindTopLevelVar i v =
  State.modify (\env -> env { envTopLevelVars = Map.insert i v (envTopLevelVars env) })

withLocalVar :: Ident -> Value -> Eval a -> Eval a
withLocalVar i v =
  Reader.local (\er -> er { erLocalVars = Map.insert i v (erLocalVars er) })

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
          fmap VModelExpr (EMember . EVal . VModel <$> model v <*> pure lab) <|> mem v lab

    EIndex e idx ->
      do v <- interpretExpr e
         idx' <- interpretExpr idx
         fmap VModelExpr (EMember . EVal . VModel <$> model v <*> str idx') <|> index v idx'

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

    EPoint binds ->
      do  binds' <- traverse (traverse interpretExpr) binds
          pure $ VPoint $ Map.fromList binds'

interpretDisplayExpr :: DisplayExpr -> Eval ()
interpretDisplayExpr (DisplayScalar scalar) = do
  v <- interpretExpr scalar
  displayValue v


interpretCall :: FunctionName -> [Value] -> Eval Value
interpretCall fun args =
  case fun of
    FPython      -> callPython args
    FAdd         -> compilable add
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
        [v] -> interpretProb v
        _ -> typeError "P expects a fold and a double as its arguments"
    FSample      ->
      case args of
        [VDFold df e, VDouble d] -> execSim (Sample 1000 (SFSample d)) df e >>= interpretExpr
        _ -> typeError "sample expects a fold and a double as its arguments"
    FSimulate    ->
      case args of
        [VDFold df e] -> execSim (SimDiffEq 1000) df e >>= interpretExpr
        _ -> typeError "simulate expects a fold as its argument"
    FFit         ->
      case args of
        [m,VPoint datas,VArray params] ->
          interpretFit m datas params
        _ ->
          typeError "fit expects a model, a data series, and a sequence of parameter names"
    FAt          ->
      case args of
        [VModelExpr e, VDouble d] -> pure $ VDFold (DFAt d) e
        [VModelExpr e, times@(VArray _)] ->
          do  times' <- array double times
              pure $ VDFold (DFAtMany times') e
        -- Sampled data is special: each v in vs is a sample of the same time domain,
        -- so they should be grouped accordingly
        [VSampledData vs, VDouble d] -> do
          vs' <- traverse getArrayContents vs
          VArray <$> traverse (\v -> atPoint v d) vs'
        [VArray vs, VDouble d] -> atPoint vs d
        [v1, times@(VArray _)] -> atPoints v1 times
        -- 'at peak' forms
        [VModelExpr e, VString i, times@(VArray _)] ->
          do times' <- array double times
             pure $ VDFold (DFAtPeak i times') e
        [VSampledData vs, VString i, _] ->
          atPeak vs i
        _ -> typeError "at expects a model and a double as its arguments"

    FLoadEasel   ->
      case args of
        [VString path] ->
          do  src <- Text.toStrict . Text.decodeUtf8 <$> getFile (Text.unpack path)
              let m = Model.parseModel MB.EaselType src >>= Model.toCore
              case VModelExpr . EVal . VModel <$> m of
                Left err -> throw (Text.pack err)
                Right mdl -> pure mdl
        _ -> typeError "loadESL expects a single string argument"

    FLoadCSV ->
      case args of
        [VString path] ->
          do csv <- getFile (Text.unpack path)
             case DS.parseDataSeries csv of
               Left err -> throw (Text.pack err)
               Right ds -> pure $ seriesAsPoints ds
        _ -> typeError "loadCSV expects a single string argument"

    FJoin ->
      case args of
        [ VArray sm1, VArray sm2, VArray ss] | Just ss' <- twoStrsM `traverse` ss ->
            case (sm1, sm2) of
              ([VString s1, VModelExpr (EVal (VModel m1))], [VString s2, VModelExpr (EVal (VModel m2))]) ->
                pure $
                VModelExpr $
                EVal $
                VModel $
                modelAsCore (join (Map.fromList ss') s1 s2 (coreAsModel m1) (coreAsModel m2) )
              _ -> typeError "all models must be specified in a list with a suffix, optionally empty"
          | otherwise -> typeError "all variable joins must be two-element arrays of strings"
        _ -> typeError $ "join expects two [suffix, model] lists and a list of variables to join" <> Text.pack (show args)
      where
        twoStrsM (VArray [VString s1, VString s2]) = Just (s1, s2)
        twoStrsM _ = Nothing

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
        [VString title, VArray series, VArray times, VString timeLabel] ->
          interpretPlot title series times timeLabel
        _   -> typeError "plot expects an array of points"

    FSeries ->
      case args of
        [VArray vs, VString title, VPoint options] -> interpretSeries vs title options
        _ -> typeError "series expects data, a title, and options"

    FScatter ->
      case args of
        v1:v2:v3:rest -> interpretScatter v1 v2 v3 rest
        _   -> typeError "plot expects an array of points"

    FAsEqnArray ->
      case args of
        [v] -> interpretAsEqnArray v
        _   -> typeError "asEqnArray expects a single argument"

    FMSE ->
      case args of
        [vPredicted, vActual] -> interpretMeanError (^(2 :: Int)) vPredicted vActual
        _                     -> typeError "mse expects two arguments"

    FMAE ->
      case args of
        [vPredicted, vActual] -> interpretMeanError abs vPredicted vActual
        _                     -> typeError "mae expects two arguments"

    FSkillRank ->
      case args of
        [vsample, valphas, vactual] -> interpretModelSkillRank vsample valphas vactual
        _                     -> typeError "modelSkillRank expects three arguments"

    FTable ->
      case args of
        [VArray labels, VArray columns] ->
          do  labels'  <- traverse str labels
              columns' <- traverse (array pure) columns
              let numLabels  = length labels'
                  numColumns = length columns'
              unless (numLabels == numColumns) $
                throw $ "Expected " <> Text.pack (show numLabels)
                                    <> "columns, received "
                                    <> Text.pack (show numColumns)
              unless (List.allSame (map length columns')) $
                throw "Columns must all have the same number of elements"
              pure $ VTable labels' columns'
        _ -> typeError "table expects a list of labels and a list of columns as arguments"

    FSimplify ->
      case args of
        [v, VArray arr] ->
          do  m <- model v
              states <-
                case strings arr of
                  Just ss -> pure ss
                  Nothing ->
                    typeError "simplify's list of states must be string literals"
              pure . VModelExpr . EVal $ VModel (Core.pruneModel (Set.fromList states) m)
        _ -> typeError "simplify expects a model and a list of states"

    FWithParams ->
      case args of
        [m, p] -> interpretWithParams m p
        _      -> typeError "withParams expects two arguments"

    FLoadPNC ->
      case args of
        [s] ->
          do  path <- str s
              src <- Text.toStrict . Text.decodeUtf8 <$> getFile (Text.unpack path)
              let pncE = Model.parseModel MB.GrometPncType src >>= Model.toCore
              case pncE of
                Left err -> throw (Text.pack err)
                Right pnc ->
                  let (pnc', _) = Core.legalize pnc
                  in  (pure . VModelExpr . EVal . VModel) pnc'
        _ -> typeError "loadPNC expects one argument"

    FDescribeModel ->
      case args of
        [v] ->
          do  m <- model v
              let desc = A.describeModelInterface (Model.Core m)
                  showVal (Just v0) = MB.describeValue v0
                  showVal Nothing = "--"
                  inputRow p = VString <$> [A.portName p, "parameter", MB.describeValueType (A.portValueType p), showVal (A.portDefault p)]
                  outputRowVal p =
                    case Map.lookup (A.portName p) (CoreS.modelInitState m) <|> Map.lookup (A.portName p) (CoreS.modelLets m) of
                      Nothing -> "--"
                      Just e  -> Text.pack . show $ CorePP.ppExpr e

                  outputRow p = VString <$> [A.portName p, "state", MB.describeValueType (A.portValueType p), outputRowVal p]
                  rows = (inputRow <$> A.modelInputs desc) ++ (outputRow <$> A.modelOutputs desc)

              pure $ VTable ["Name", "State/Param", "Data Type", "Value"] (transpose rows)
        _ -> typeError "describe expects one argument"

    FModelGraph ->
      case args of
        [v] ->
          do  m <- model v
              esvg <- liftIO $ CoreViz.renderModelAsSimpleFlowGraphToRawImageIO (CoreViz.MVConfig CoreViz.MVNoIndirectEdges) m CoreViz.ImageSvg
              case esvg of
                Left err -> throw err
                Right svg -> pure $ VSVG svg
        _ -> typeError "modelGraph expects one argument"

    FModelSize ->
      case args of
        [v] ->
          do  m <- model v
              let sizeStr = "states: " <> (Text.pack . show  . length) (CoreS.modelStateVars m)
                            <> "   events: " <> (Text.pack . show . length) (CoreS.modelEvents m)
              pure $ VString sizeStr
        _ -> typeError "modelSize expects one argument"

  where
    strings = mapM (\case VString s -> Just s; _ -> Nothing)

    doubleArraySummarize f v =
      do  v' <- array double v
          pure $ VDouble (f v')

    typeError = typeErrorArgs args

    getMexpr (VModelExpr e) = Just e
    getMexpr _ = Nothing
    isMexpr e = isJust (getMexpr e)

    asMexprArg (VModelExpr e) = e
    asMexprArg v = EVal v

    add = do
      l1 <- lincmpAdd
      l2 <- liftLeftBin (binarithMb (+))
      l3 <- liftRightBin (binarithMb (+))
      case l1 <|> l2 <|> l3 of
        Just v  -> pure v
        Nothing -> throw "Cannot do addition on these values"

    lincmpAdd =
      case args of
        [l, r] -> go l r
        _      -> pure Nothing
      where
        go :: Value -> Value -> Eval (Maybe Value)
        go (VDouble d1) (VDouble d2) =
          pure $ Just $ VDouble (d1 + d2)
        go (VTimed v1 t1) (VTimed v2 t2)
          | t1 == t2
          = do  v <- go v1 v2
                pure $ fmap (\x -> VTimed x t1) v
          | otherwise
          = throw $ Text.unlines [ "Mismatched times"
                                 , "t1: " <> Text.pack (show t1)
                                 , "t2: " <> Text.pack (show t2)
                                 ]
        go a1@VArray{} a2@VArray{} =
          do  a  <- parallelArrays value a1 a2
              a' <- traverse (uncurry go) a
              pure $ fmap VArray $ sequenceA a'
        go _ _ = pure Nothing

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

interpretPlot :: Text -> [Value] -> [Value] -> Text -> Eval Value
interpretPlot title series xs xLabel =
  do series' <- traverse plotSeries series
     xs'     <- traverse double xs
     pure $ VPlot $ Plot.Plot
       { Plot.plotTitle = title
       , Plot.plotSeries = series'
       , Plot.plotVs = xs'
       , Plot.plotVsLabel = xLabel
       }

interpretProb :: Value -> Eval Value
interpretProb = chooseLift go (throw "exepecting a collection of booleans")
  where
    go arr =
      case arr of
        VArray vs ->
          do vs' <- traverse getBoolValue vs
             pure $ VDouble $ fromIntegral (count id vs') / fromIntegral (length vs)
        _ -> throw "P() expects a collection of booleans"


interpretSeries :: [Value] -> Text -> Map Text Value -> Eval Value
interpretSeries vs label opts =
  do vs' <- traverse double vs
     pure $ VSeries $ Plot.PlotSeries
       { Plot.plotSeriesLabel = label
       , Plot.plotSeriesData  = vs'
       , Plot.plotSeriesStyle = style
       , Plot.plotColor       = color
       }
  where
    style =
      case Map.lookup "style" opts of
        Just (VString s) -> interpStyle $ CI.mk s
        _                -> Plot.Line

    color =
      case Map.lookup "color" opts of
        Just (VString s)
          -> Just $ Plot.ColorScheme $ CI.foldedCase $ CI.mk s
        Just (VArray arr)
          |  Just range <- traverse asString arr
          -> Just $ Plot.ColorRange range
        _ -> Nothing

    asString :: Value -> Maybe Text
    asString (VString s) = Just s
    asString _           = Nothing

    interpStyle :: CI Text -> PlotStyle
    interpStyle "points"  = Plot.Points
    interpStyle "circles" = Plot.Circles
    interpStyle "squares" = Plot.Squares
    interpStyle "line"    = Plot.Line
    interpStyle _         = Plot.Line

interpretFit :: Value -> Map Text Value -> [Value] -> Eval Value
interpretFit mv ds ps =
  do m   <- Model.Core <$> model mv
     ps' <- traverse str ps
     ds' <- traverse (array double) ds
     dataSeries <-
       case Map.lookup "time" ds' of
         Nothing ->
           throw "fit() data requires a 'time' series"
         Just ts ->
           let rest = Map.delete "time" ds'
           in pure $ DS.DataSeries ts rest
     case Model.toDeqs m of
       Left err ->
         throw $ Text.pack err
       Right deq ->
         do let iface     = A.describeModelInterface m
                ifaceErrs = A.paramsNotExistErrors (Set.fromList ps') iface
            case ifaceErrs of
              [] ->
                do let (res, _) = DEQ.fitModel deq dataSeries mempty $ Map.fromList (zip ps' (repeat 0))
                       vals = VPoint $ Map.map (VDouble . fst) res
                       errs = VPoint $ Map.map (VDouble . snd) res
                   return $ VPoint $ Map.fromList [ ("values", vals), ("errors", errs) ]
              _ ->
                throw (Text.unlines ifaceErrs)

interpretInterpolate :: Value -> Value -> Eval Value
interpretInterpolate (VModelExpr (EVal arg1)) arg2 =
  interpretInterpolate arg1 arg2
interpretInterpolate arr@(VArray vs) (VArray points) =
  do pointDs  <- getDoubleValue `traverse` points
     oldTimes <- array double =<< mem arr "time"

     let buildMap m (VPoint pt) =
           do pt' <- traverse double pt
              pure $ Map.unionWith (++) m (pure <$> pt')
         buildMap _ _ =
           throw "Expected array of points"

     grouped <- foldM buildMap mempty vs
     let interpolated =
           Map.map (\oldValues ->
                      map (Interpolation.evaluateV Interpolation.Linear
                                                   (VS.fromList oldTimes)
                                                   (VS.fromList oldValues))
                          pointDs)
                   grouped

     let unMapped = transpose [ [ (i, VDouble d) | d <- ds ] | (i, ds) <- Map.toList interpolated ]
     pure $ VArray (VPoint . Map.fromList <$> unMapped)
interpretInterpolate arg1 arg2 =
  typeErrorArgs [arg1, arg2] "interpolate expects a data series and an array as arguments"

interpretHistogram :: Value -> Value -> Eval Value
interpretHistogram (VModelExpr e) nBins =
  do v <- interpretExpr e
     interpretHistogram v nBins
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

interpretAsEqnArray :: Value -> Eval Value
interpretAsEqnArray (VModelExpr (EVal v)) = interpretAsEqnArray v
interpretAsEqnArray (VModel m) =
  case Model.toDeqs (Model.Core m) of
    Left err -> throw $ Text.pack err
    Right d  -> pure $ VLatex $ Latex d
interpretAsEqnArray v = typeErrorArgs [v] "asEqnArray expects a model"

interpretMeanError :: (Double -> Double) -> Value -> Value -> Eval Value
interpretMeanError f vPredicted vActual =
  case (vPredicted, vActual) of
    (VDouble predicted, VArray actual) ->
      do  ads <- traverse double actual
          pure $ meanError $ map (predicted,) ads
    (VArray predicted, VDouble actual) ->
      do  pds <- traverse double predicted
          pure $ meanError $ map (,actual) pds
    (VArray{}, VArray{}) ->
      do  pas <- parallelArrays double vPredicted vActual
          pure $ meanError pas
    (_, _) -> typeErrorArgs [vPredicted, vActual]
                "mse/mae expects two arrays of doubles as arguments"
  where
    meanError :: [(Double, Double)] -> Value
    meanError pas = VDouble $ mean $ map (\(p, a) -> f (p - a)) pas

interpretWithParams :: Value -> Value -> Eval Value
interpretWithParams (VModelExpr (EVal m)) p =
  interpretWithParams m p
interpretWithParams (VModel m) (VPoint p) =
  do  p' <- traverse double p
      pure $ VModelExpr . EVal . VModel $ Core.addParams p' m
interpretWithParams m p =
  typeErrorArgs [m, p] "withParams expects a model and a point as arguments"

typeErrorArgs :: [Value] -> Text -> Eval a
typeErrorArgs _args msg = throw $ Text.unlines
  [ "type error: " <> msg
  {- Uncomment the line below if you want a _lot_ of debugging output -}
  , "arguments: " <> Text.pack (show _args)
  ]

data SimMethod = Sample Double SampleFold
                 -- ^ First parameter is time-granularity of sampling
               | SimDiffEq Double
                 -- ^ Time-granularity of simulation
               deriving (Show)

-- run a single simulation - emitting a new (evaulable expr)
execSim :: SimMethod -> DynamicalFold -> Expr -> Eval Expr
execSim how df e = unfoldS . unfoldD <$> runSimExpr how expLength e
  where
    expLength =
      case df of
        DFAt d -> d
        DFAtMany d -> maximum d
        DFAtPeak _ d -> maximum d
        DFIn _ end -> end

    unfoldD e1 =
      case df of
        DFAt d -> ECall FAt [e1, EVal $ VDouble d]
        DFAtMany ds -> ECall FAt [e1, EVal $ VArray (VDouble <$> ds)]
        DFAtPeak i ds -> ECall FAt [e1, EVal $ VString i, EVal $ VArray (VDouble <$> ds)]
        DFIn start end -> ECall FIn [e1, EVal $ VDouble start, EVal $ VDouble end]
    unfoldS e1 =
      case how of
        Sample _ (SFProbability n) -> ECall FProb [e1, EVal $ VDouble n]
        Sample _ (SFSample _) -> e1
        SimDiffEq _ -> e1


-- TODO: generalized traversal?
runSimExpr :: SimMethod -> Double -> Expr -> Eval Expr
runSimExpr how t e0 =
  case e0 of
    EVal (VModel m) -> EVal <$> runSim how t m
    EVal _ -> pure e0
    EVar _ -> pure e0
    ECall fname args -> ECall fname <$> runSimExpr how t `traverse` args
    ECallWithLambda fname args lambdaVar lambdaExpr ->
      ECallWithLambda fname <$> (runSimExpr how t `traverse` args)
                            <*> pure lambdaVar
                            <*> (runSimExpr how t lambdaExpr)
    EIndex e idx  -> EIndex <$> runSimExpr how t e <*> pure idx
    EMember e lab -> EMember <$> runSimExpr how t e <*> pure lab
    EList es -> EList <$> runSimExpr how t `traverse` es
    EListRange start stop step -> EListRange <$> runSimExpr how t start <*> runSimExpr how t stop <*> runSimExpr how t step
    EPoint es -> EPoint <$> traverse (traverse (runSimExpr how t)) es

runSim :: SimMethod -> Double -> Core.Model -> Eval Value
runSim (Sample delta sf) t mdl =
  do  series <- io mkSeries
      pure $ VSampledData (seriesAsPoints <$> series)
  where
    -- TODO: we should just get the raw simulation data?
    mkSeries = CPP.simulate mdl 0 t (t/delta) Nothing d
    d = case sf of
          SFProbability n -> floor n
          SFSample n      -> floor n

runSim (SimDiffEq delta) t mdl =
  case Model.toDeqs (Model.Core mdl) of
    Left err   ->
      throw (Text.pack err)
    Right deqs ->
      do let series = DEQ.simulate deqs mempty mempty [0,t/delta..t]
         pure $ seriesAsPoints series

callPython :: [Value] -> Eval Value
callPython args =
  case args of
    VString f : args' ->
      do py <- Reader.asks erPyHandle
         res <- liftIO $ evaluate py f args'
         case res of
           Success v -> pure $ unPython v
           Failure e -> Except.throwError e
    _ ->
      throw "python() requires at least a function name"

interpretWIS :: Value -> Value -> Value -> Eval Value
interpretWIS samples alphas pt =
  do samples' <- array double samples
     alphas'  <- array double alphas
     pt'      <- double pt
     pure $ VDouble $ wis samples' alphas' pt'

interpretModelSkillRank :: Value -> Value -> Value -> Eval Value
interpretModelSkillRank modelObsSamples alphas truth =
  do modelObsSamples' <- array (array (array double)) modelObsSamples
     alphas'          <- array double alphas
     truth'           <- array double truth

     when (any (wrongNumberObservations (length truth')) modelObsSamples') $
       throw "can't compute skill rank with mismatched numbers of observations"

     when (any hasEmptyObservations modelObsSamples') $
       throw "can't compute skill rank with an empty observation"

     pure $ VArray (VDouble <$> skillRank modelObsSamples' alphas' truth')
  where
    hasEmptyObservations :: [[Double]] -> Bool
    hasEmptyObservations obss = any null obss

    wrongNumberObservations n obs = length obs /= n

-- The first parameter is a list of samples indexed by (model number,
-- observation number, sample number). The second param is a list of alphas
-- Third param is the "ground truth", i.e. a list of values indexed by
-- observation number. Returns a list of ranks (value in [0,1]) indexed by model
-- number (higher is better)
skillRank :: [[[Double]]] -> [Double] -> [Double] -> [Double]
skillRank modelObservations alphas truth =
  -- Returns the _mean_ of the ranks across all observations for a given model
  [ mean (Map.findWithDefault [0.0] mi ranksByModel)
  | mi <- [0..n-1]
  ]
  where
    -- Group all of the per-observation ranks by model.
    ranksByModel =
      foldr (\(mi, r) -> Map.insertWith (++) mi [r]) mempty rankedByObservation

    -- Given all scores (indexed by model and observation number), For each
    -- observation assign each model a rank, (lower rank means lower interval
    -- score for that observation)
    rankedByObservation =
      assignRanks =<< byObservation

    byObservation =
        groupBy ((==)`on`obsNo)
      $ sortBy (compare`on`obsNo) wmi

    assignRanks :: [(Int, Int, Double)] -> [(Int, Double)]
    assignRanks obsScores =
      [ (modelNo s, rank (fromIntegral r))
      | s <- sortBy (compare`on`score) obsScores
      | r <- [(1::Int)..]
      ]

    rank :: Double -> Double
    rank r = 1.0 - ((r - 1.0)/(fromIntegral n - 1))

    n = length modelObservations

    -- Calculate WIS for all models mi and all observations
    wmi = concat [ goModel mi m | m <- modelObservations | mi <- [0..] ]

    obsNo (_,i,_)    = i
    modelNo (mi,_,_) = mi
    score (_,_,s)    = s

    -- for model mi:
    -- for each observation i
    -- calculate the WIS at i.
    goModel :: Int -> [[Double]] -> [(Int, Int, Double)]
    goModel mi obsSamples =
      [ (mi, i, wis obs alphas y) | obs <- obsSamples | y <- truth | i <- [0..] ]


-- | Weighted interval score for a given set of samples, alphas and observation.
-- Implementation based on
-- <https://github.com/GaloisInc/ASKE-E/blob/main/docs/metrics/weighted_interval_score.py>
wis :: [Double] -> [Double] -> Double -> Double
wis vals alphas y =
  (1/(k + 0.5)) * (w0 * abs (y-m) + agg_interval)
  where
    k = fromIntegral (length alphas - 1)
    w0 = 0.5
    go a = -- w_k
           (a/2)
           -- interval score
         * intervalScoreAlpha (quant (a/2))
                              (quant (1 - (a/2)))
                              a
                              y
    agg_interval = sum (go <$> alphas)

    m     = quant 0.5
    quant = quantile vals'
    vals' = sort vals

-- The first argument should be non-empty and sorted in increasing order
-- Performs the equivalent of `numpy.quantile` with `interpolation='lower'` when
-- the quantile lies between two data points.
quantile :: [a] -> Double -> a
quantile sortedData q = sortedData !! safe
  where
    safe   = max 0 (min needed (length sortedData - 1))
    needed = floorDoubleInt (q * fromIntegral (length sortedData))

intervalScoreAlpha :: Double -> Double -> Double -> Double -> Double
intervalScoreAlpha l u alpha y =
    (u-l) -- Penalize wide interval
  + (w*(l-y)) `onlyIf` (y < l) -- Penalize a "too-high" lower bound
  + (w*(y-u)) `onlyIf` (y > u) -- Penalize a "too-low" upper bound
  where
    w = 2/alpha

    onlyIf e p
      | p         = e
      | otherwise = 0

seriesAsPoints :: DS.DataSeries Double -> Value
seriesAsPoints ds = VArray (pointToValue <$> DS.toDataPoints ds)
  where
    pointToValue p =
      let m    = VDouble <$> DS.ptValues p
          t    = DS.ptTime p
      in  VTimed (VPoint m) t

timedPointsAsSeries :: [Value] -> Eval (DS.DataSeries Double)
timedPointsAsSeries vs =
  do (ts, m) <- foldM go ([], mempty) (reverse vs)
     pure $ DS.DataSeries ts m
  where
    go (ts, ptMap) v =
      do (p, t) <- timed withPoint v
         pure (t:ts, Map.unionWith (++) p ptMap)

    withPoint (VPoint pt) = traverse (fmap pure . double) pt
    withPoint _ = throw "Expected point of doubles"

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

-- | Vals should be either a sequence of timed data,
-- or a VSampledData
atPoints :: Value -> Value -> Eval Value
atPoints vals times =
  do times' <- array double times
     case vals of
       VSampledData samples ->
         do pts <- traverse (array (timed value)) samples
            pure $ VArray (atArrs pts <$> times')
       vals' ->
         do let f v =
                  do pts <- array (timed value) v
                     pure $ VArray $ catMaybes [ VTimed <$> atSimple pts t <*> pure t | t <- times' ]
            chooseLift f (typeErrorArgs [vals] "atPoints expects samples or an array of timed values") vals'
  where
    atArrs arrA t = VTimed (VArray . catMaybes $ (`atSimple` t) <$> arrA) t

atPeak :: [Value] -> Ident -> Eval Value
atPeak samples i =
  VArray <$> traverse perSample samples
  where
    perSample :: Value -> Eval Value
    perSample s =
      do s' <- array value s
         ds <- traverse (`mem`i) s'
         ds' <- fmap fst <$> traverse (timed double) ds
         let m = maximum ds'
         head <$> filterM (\pt ->
           do (val, _) <- timed double =<< pt `mem` i
              pure $ val >= m) s'

atSimple :: [(a, Double)] -> Double -> Maybe a
atSimple lst t =
  case lst of
    []     -> Nothing
    (e, t'):_ | t <= t' -> Just e
    [(e, _)] -> Just e
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

index :: Value -> Value -> Eval Value
index v i =
  identIdx <|> intIdx
  where
    identIdx = mem v =<< str i
    intIdx   =
      do v' <- array value v
         i' <- asInt i
         when (length v' <= i') $
           throw "Index out of bounds"
         pure (v' !! i')
    asInt (VInt iv) = pure iv
    asInt (VDouble d) = pure (floor d)
    asInt _ = throw "Array index must be numeric"

mem :: Value -> Ident -> Eval Value
mem v0 l = chooseLift getMem nope v0
  where
    nope = throw $ "cannot find member '" <> l <> "' in value"
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

model :: Value -> Eval Core.Model
model v =
  case v of
    VModel m -> pure m
    VModelExpr (EVal v') -> model v'
    _ -> throw "Expecting model"

plotSeries :: Value -> Eval (Plot.PlotSeries [Double])
plotSeries v =
  case v of
    VSeries ps -> pure ps
    _ -> throw "Expecting plot series"

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
    _ -> throw "Expecting string"

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


asModelExprValue :: Value -> Eval Value
asModelExprValue v =
  do  m <- model v
      (pure . VModelExpr . EVal . VModel) m

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
    VSampledData vs -> VSampledData <$> (chooseLift f orElse `traverse` vs)
    _ -> orElse

liftInto :: Value -> (Value -> Eval Value) -> Eval Value
liftInto = flip liftPrim

liftPrim :: (Value -> Eval Value) -> Value -> Eval Value
liftPrim f v =
  case v of
    VTimed v' t -> VTimed <$> liftPrim f v' <*> pure t
    VArray vs -> VArray <$> (liftPrim f `traverse` vs)
    VSampledData vs -> VSampledData <$> (liftPrim f `traverse` vs)
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
    VSampledData vs ->
      do  vs' <- liftPrim' f `traverse` vs
          pure $ VSampledData <$> sequenceA vs'
    _ -> f v

tryLiftArray :: (Value -> Eval Value) -> Value -> Eval Value
tryLiftArray f v0 =
  case v0 of
    VArray v -> (VArray <$> f `traverse` v) <|> f v0
    _ -> f v0

tryLiftArrayInto :: Value -> (Value -> Eval Value) -> Eval Value
tryLiftArrayInto = flip tryLiftArray
