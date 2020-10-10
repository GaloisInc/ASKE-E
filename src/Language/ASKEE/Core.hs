{-# Language FlexibleContexts #-}

module Language.ASKEE.Core where

import Control.Monad(filterM)
import Data.Array.IO(IOUArray)
import qualified Data.Array.MArray as MArray
import qualified Data.Array.IArray as IArray
import qualified Data.Ix as Ix
import qualified System.Random as Random

type Ident = Int

data Expr =
    ExprAdd    Expr Expr
  | ExprMul    Expr Expr
  | ExprSub    Expr Expr
  | ExprDiv    Expr Expr
  | ExprLT     Expr Expr
  | ExprEQ     Expr Expr
  | ExprGT     Expr Expr
  | ExprNot    Expr
  | ExprNeg    Expr
  | ExprIf     Expr Expr Expr
  | ExprAnd    Expr Expr
  | ExprOr     Expr Expr
  | ExprNumLit Double
  | ExprVar    Ident
  deriving (Show, Eq)

data Event =
  Event { eventRate   :: Expr
        , eventWhen   :: Expr
        , eventEffect :: [(Ident, Expr)]
        }
  deriving (Show, Eq)

data Model =
  Model { modelInitState :: [(Ident, Double)]
        , modelEvents    :: [Event]
        }
  deriving (Show, Eq)

asBool :: Double -> Bool
asBool v = if v == 0.0 then True else False

asDouble :: Bool -> Double
asDouble True = 1.0
asDouble False = 0.0

eval :: IOUArray Int Double -> Expr -> IO Double
eval env e0 =
  case e0 of
    ExprNumLit l -> pure l
    ExprVar v -> MArray.readArray env v
    ExprAdd e1 e2 -> binop (+) e1 e2
    ExprSub e1 e2 -> binop (-) e1 e2
    ExprMul e1 e2 -> binop (*) e1 e2
    ExprDiv e1 e2 -> binop (/) e1 e2
    ExprLT e1 e2 -> cmpop (<) e1 e2
    ExprEQ e1 e2 -> cmpop (==) e1 e2
    ExprGT e1 e2 -> cmpop (>) e1 e2
    ExprAnd e1 e2 -> logop (*) e1 e2
    ExprOr e1 e2 -> logop (+) e1 e2
    ExprNot e -> asDouble . asBool <$> eval env e
    ExprNeg e -> negate <$> eval env e
    ExprIf expTest expThen expElse ->
      do  tst <- eval env expTest
          if tst == 0
            then eval env expElse
            else eval env expThen
  where
    logop f =
      let f' v1 v2 = f v1 v2 == 0
      in cmpop f'

    cmpop f =
      let f' v1 v2 = if f v1 v2 then 1.0 else 0.0
      in binop f'

    binop f e1 e2 =
      do  v1 <- eval env e1
          v2 <- eval env e2
          pure (f v1 v2) 

runModel :: Int -> Model -> IO [(Ident, Double)]
runModel steps m =
  do  env <- MArray.newArray (minIdent, maxIdent) 0.0
      gen <- Random.newStdGen
      _ <- step env `traverse` [1 .. steps]
      (statePair env) `traverse` stateVars

  where
    minIdent = minimum (fst <$> modelInitState m)
    maxIdent = maximum (fst <$> modelInitState m)
    stateVars = fst <$> modelInitState m

    isEnabled :: IOUArray Int Double -> Event -> IO Bool
    isEnabled env e = asBool <$> eval env (eventWhen e)

    enabledEvents :: IOUArray Int Double -> IO (IArray.Array Int Event)
    enabledEvents env =
      do  enabledEvents <- filterM (isEnabled env) (modelEvents m)
          pure $ IArray.listArray (0, length enabledEvents) (modelEvents m)

    -- TODO: select event with rate and compute time increment
    selectEvent :: IOUArray Int Double -> IO Event
    selectEvent env =
      do  evts <- enabledEvents env
          i <- Random.getStdRandom (Random.uniformR (IArray.bounds evts))
          pure $ evts IArray.! i

    statePair :: IOUArray Int Double -> Ident -> IO (Int, Double)
    statePair env i = 
      do val <- MArray.readArray env i 
         pure (i, val)

    exec env (name, e) =
      do  val <- eval env e
          pure (name, val)

    step env _ =
      do  event <- selectEvent env
          results <- exec env `traverse` eventEffect event
          (uncurry (MArray.writeArray env)) `traverse` results



