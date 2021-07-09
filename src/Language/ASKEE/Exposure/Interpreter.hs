{-# Language OverloadedStrings #-}
module Language.ASKEE.Exposure.Interpreter where

import qualified Data.Aeson as JSON

import qualified Control.Monad.State as State
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Except as Except
import Data.Foldable(traverse_)
import Control.Monad.Trans(lift)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(isJust)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Set(Set)
import qualified Data.Set as Set
import Language.ASKEE.Exposure.Syntax

import qualified Language.ASKEE.Core as Core
import qualified Language.ASKEE.Model as Model
import qualified Language.ASKEE.Model.Basics as MB
import qualified Language.ASKEE.DataSeries as DS
import qualified Language.ASKEE.CPP as CPP


data ExposureInfo = ExposureInfo
  deriving Show

runEval :: Env -> Eval a -> IO (Either Text (a, Env, EvalWrite))
runEval env ev = Except.runExceptT $ RWS.runRWST ev (EvalRead Map.empty) env

-------------------------------------------------------------------------------



evalStmts :: [Stmt] -> Env -> IO (Either Text (Env, [DisplayValue], [StmtEff]))
evalStmts stmts env = evalLoop env stmts

initialEnv :: Env
initialEnv = Env Map.empty

evalLoop :: Env -> [Stmt] -> IO (Either Text (Env, [DisplayValue], [StmtEff]))
evalLoop env stmts = go Map.empty
  where
    go m =
      let except = RWS.execRWST (interpretStmt `traverse_` stmts) (EvalRead m) env
      in do lr <- Except.runExceptT except
            case lr of
              Left e -> pure $ Left e
              Right (env', w)
                | ewDeps w == Set.empty ->
                    pure $ Right (env', ewDisplay w, ewStmtEff w)
                | otherwise ->
                    do  evs <- simulate (ewDeps w)
                        case evs of
                          Left e -> pure $ Left e
                          Right vs -> go (m `Map.union` vs)

-------------------------------------------------------------------------------


simulate :: Set Value -> IO (Either Text (Map Value Value))
simulate vs =
  pure $ Left (Text.unlines $ "simulation is not implemented:":
                              (Text.pack . show <$> Set.toList vs))

-------------------------------------------------------------------------------
-- eval monad
data Env = Env
  { envVars :: Map Ident Value
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

data StmtEff =
  StmtEffBind Ident Value

type Eval a = RWS.RWST EvalRead EvalWrite Env (Except.ExceptT Text IO) a

-- TODO: catch exceptions
liftIO :: IO a -> Eval a
liftIO = lift . lift

throw :: Text -> Eval a
throw = Except.throwError

notImplemented :: Text -> Eval a
notImplemented what = throw ("not implemented: " <> what)

getVarValue :: Ident -> Eval Value
getVarValue i =
  do  mbV <- State.gets (Map.lookup i . envVars)
      case mbV of
        Nothing -> throw ("variable " <> i <> " is not defined")
        Just v -> pure v

bindVar :: Ident -> Value -> Eval ()
bindVar i v =
  do  mbV <- State.gets (Map.lookup i . envVars)
      case mbV of
        Nothing ->
          State.modify (\env -> env { envVars = Map.insert i v (envVars env) })
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
          bindVar var eval

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

    -- TODO: will we also need suspend for this?
    EMember e lab ->
      do  v <- interpretExpr e
          case v of
            -- TODO: typecheck model expr
            VModelExpr m -> pure $ VModelExpr (EMember m lab)
            _ -> mem v lab

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
        [VDFold df e, VDouble d] -> execSim (SFProbability d) df e >>= interpretExpr
        _ -> typeError
    FSample      ->
      case args of
        [VDFold df e, VDouble d] -> execSim (SFSample d) df e >>= interpretExpr
        _ -> typeError
    FAt          ->
      case args of
        [VModelExpr e, VDouble d] -> pure $ VDFold (DFAt d) e
        _ -> typeError

    FLoadEasel   ->
      case args of
        [VString path] ->
          do  eitherVal <-
                liftIO $
                  do  src <- Text.readFile (Text.unpack path)
                      let m = Model.parseModel MB.EaselType src >>= Model.toCore
                      pure (VModelExpr . EVal . VModel <$> m)
              case eitherVal of
                Left err -> throw (Text.pack err)
                Right m -> pure m
        _ -> typeError

    FLoadCSV ->
      case args of
        [VString path] ->
          do ds <- liftIO $ DS.parseDataSeriesFromFile $ Text.unpack path
             pure $ VModelExpr $ EVal $ VDataSeries ds
        _ -> typeError

    FMean ->
      case args of
        [VModelExpr (EMember (EVal (VDataSeries DS.DataSeries{DS.values = vs})) lab)] ->
          case Map.lookup lab vs of
            Just ds -> pure $ VDouble $ mean ds
            Nothing -> throw $ "no label named: " <> lab
        _ -> typeError
  where
    typeError = throw "type error"

    getMexpr (VModelExpr e) = Just e
    getMexpr _ = Nothing
    isMexpr e = isJust (getMexpr e)

    asMexprArg (VModelExpr e) = e
    asMexprArg v = EVal v

    bincmpDouble f =
      case args of
        [VDouble n1, VDouble n2] -> pure $ VBool (f n1 n2)
        _ -> throw "Cannot compare these values"

    bincmpBool f =
      case args of
        [VBool b1, VBool b2] -> pure $ VBool (f b1 b2)
        _ -> throw "Cannot compare these values"

    binarith f =
      case args of
        [VDouble n1, VDouble n2] -> pure $ VDouble (f n1 n2)
        _ -> throw "Cannot do arithmetic on these values"

    unaryBool f =
      case args of
        [VBool b] -> pure $ VBool (f b)
        _ -> throw "Expected a single boolean argument"

    compilable orElse =
      if any isMexpr args
        then pure $ VModelExpr (ECall fun (asMexprArg <$> args))
        else orElse


-- run a single simulation - emitting a new (evaulable expr)
execSim :: SampleFold -> DynamicalFold -> Expr -> Eval Expr
execSim sf df = runSimExpr sampleCount expLength
  where
    expLength =
      case df of
        DFAt d -> d

    sampleCount =
      case sf of
        SFProbability d -> floor d
        SFSample d -> floor d

-- TODO: generalized traversal?
runSimExpr :: Int -> Double -> Expr -> Eval Expr
runSimExpr n t e0 =
  case e0 of
    EVal (VModel m) -> EVal <$> runSim n t m
    EVal _ -> pure e0
    EVar _ -> pure e0
    ECall fname args -> ECall fname <$> runSimExpr n t `traverse` args
    EMember e lab -> EMember <$> runSimExpr n t e <*> pure lab

runSim :: Int -> Double -> Core.Model -> Eval Value
runSim n t mdl =
  do  series <- liftIO mkSeries
      pure $ VArray (seriesAsPoints <$> series)
  where
    -- TODO: we should just get the raw simulation data?
    mkSeries = CPP.simulate mdl 0 t (t/100) `traverse` (Just <$> [0..n])

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

-- this presumes quite a few things
atPoint :: [Value] -> Double -> Eval Value
atPoint vs t =
  case vs of
    [] -> throw "cannot 'at' on an empty sequence"
    e:r -> go r e
  where
    go vs' v =
      case vs' of
        [] -> pure v
        v':r ->
          do (val, t') <- getTimedValue v'
             if t' > t
               then pure v
               else go r val


liftPrim :: (Value -> Eval Value) -> Value -> Eval Value
liftPrim f v =
  case v of
    VTimed v' t -> VTimed <$> liftPrim f v' <*> pure t
    VArray vs -> VArray <$> (liftPrim f `traverse` vs)
    _ -> f v

mem :: Value -> Ident -> Eval Value
mem v0 l = liftPrim getMem v0
  where
    getMem v =
      case v of
        VPoint p ->
          case Map.lookup l p of
            Nothing -> throw ("cannot find member '" <> l <> "' in point")
            Just v' -> pure v'
        _ -> throw "type does not support membership"

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)
