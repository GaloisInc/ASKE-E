module Langugae.ASKEE.Syntax where
import Data.Array.IO(IOUArray)
import qualified Data.Array.MArray as MArray
import qualified Data.Array.IArray as IArray
import qualified System.Random as Random

type Ident = Int

data Expr =
    ExprAdd    Expr Expr
  | ExprMul    Expr Expr
  | ExprSub    Expr Expr
  | ExprDiv    Expr Expr
  | ExprLT     Expr Expr
  | ExprNot    Expr
  | ExprNeg    Expr
  | ExprIf     Expr Expr Expr
  | ExprAnd    Expr Expr
  | ExprOr     Expr Expr
  | ExprNumLit Double
  | ExprVar    Ident

data Event =
  Event { eventRate   :: Expr
        , eventWhen   :: Expr
        , eventEffect :: [(Ident, Expr)]
        }

data Model =
  Model { modelInitState :: [(Ident, Double)]
        , modelEvents    :: [Event]
        }

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
    ExprAnd e1 e2 -> logop (*) e1 e2
    ExprOr e1 e2 -> logop (+) e1 e2
    ExprNot e ->
      do  v <- eval env e
          pure $ if v == 0.0 then 1.0 else 0.0
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
      let f' v1 v2 = if f v1 v2 then 1.0 else 1.0 
      in binop f'

    binop f e1 e2 =
      do  v1 <- eval env e1
          v2 <- eval env e2
          pure (f v1 v2) 

runModel :: Int -> Model -> IO [(Ident, Double)]
runModel steps m =
  do  env <- MArray.newArray (minIdent, maxIdent) 0.0
      let evts = IArray.listArray (0, (length $ modelEvents m) - 1) (modelEvents m)
      gen <- Random.newStdGen
      step `traverse_` [1 .. steps]

  where
    minIdent = min . fst <$> modelInit m
    maxIdent = max . fst <$> modelInit m
    select gen evts =
      do  i <- Random.uniformR (IArray.range evts) gen
          pure $ evts (IArray.!) i

-- eval :: Int -> Model -> IO [State]
-- eval steps m =
--   do  
--   where
--     maxIdent = fst <$> modelInit m