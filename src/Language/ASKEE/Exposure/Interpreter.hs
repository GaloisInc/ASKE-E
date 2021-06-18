{-# Language OverloadedStrings #-}
module Language.ASKEE.Exposure.Interpreter where

import qualified Data.Aeson as JSON

import qualified Control.Monad.State as State
import qualified Control.Monad.Except as Except
import Control.Monad.Trans(lift)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(isJust)
import Data.Text(Text)
import qualified Data.Text as Text
import Language.ASKEE.Model as Model
import Language.ASKEE.ModelType as MT
import Language.ASKEE.Exposure.Syntax

data Env = Env
  { envVars :: Map Ident Value
  }

type Eval a = State.StateT Env (Except.ExceptT Text IO) a
data ExposureInfo = ExposureInfo
  deriving Show

-------------------------------------------------------------------------------

-- TODO: catch exceptions
liftIO :: IO a -> Eval a
liftIO = lift . lift

throw :: Text -> Eval a
throw = Except.throwError

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

-- TODO: Finish implementing these
interpretStmt :: Stmt -> Eval ()
interpretStmt stmt =
  case stmt of
    StmtLet var expr     ->
      do  eval <- interpretExpr expr
          bindVar var eval

    StmtDisplay dispExpr -> undefined

interpretExpr :: Expr -> Eval Value
interpretExpr e0 =
  case e0 of
    EVar var         -> getVarValue var
    EVal v           -> pure v
    ECall fun args   ->
      do  args' <- interpretExpr `traverse` args
          interpretCall fun args'
    EMember e lab ->
      do  v <- interpretExpr e
          case v of
            -- TODO: typecheck model expr
            VModelExpr m -> pure $ VModelExpr (EMember m lab)
            _ -> throw "type error"


interpretDisplayExpr :: DisplayExpr -> ExposureInfo
interpretDisplayExpr (DisplayScalar scalar) = undefined

interpretCall :: FunctionName -> [Value] -> Eval Value
interpretCall fun args =
  case fun of
    FAdd         -> compilable (binarith (+))
    FSub         -> compilable (binarith (-))
    FMul         -> compilable (binarith (*))
    FDiv         -> compilable (binarith (/))
    FGT          -> compilable (bincmp (>))
    FGTE         -> compilable (bincmp (>=))
    FLT          -> compilable (bincmp (<))
    FLTE         -> compilable (bincmp (<=))
    FEQ          -> undefined
    FNEQ         -> undefined
    FNot         -> undefined
    FAnd         -> undefined
    FOr          -> undefined
    FProb        -> undefined
    FSample      -> undefined
    FAt          -> undefined
    FLoadEasel   ->
      case args of
        [VString path] ->
          do  eitherVal <-
                liftIO $
                  do  src <- readFile (Text.unpack path)
                      let m = Model.parseModel MT.EaselType src >>= toCore
                      pure (VModelExpr . EVal . VModel <$> m)
              case eitherVal of
                Left err -> throw (Text.pack err)
                Right m -> pure m
        _ -> throw "type error"
  where
    getMexpr (VModelExpr e) = Just e
    getMexpr _ = Nothing
    isMexpr e = isJust (getMexpr e)

    asMexprArg (VModelExpr e) = e
    asMexprArg v = EVal v

    bincmp f =
      case args of
        [VDouble n1, VDouble n2] -> pure $ VBool (f n1 n2)
        _ -> throw "Cannot compare these values"

    binarith f =
      case args of
        [VDouble n1, VDouble n2] -> pure $ VDouble (f n1 n2)
        _ -> throw "Cannot do arithmetic on these values"

    compilable orElse =
      if any isMexpr args
        then pure $ VModelExpr (ECall fun (asMexprArg <$> args))
        else orElse


