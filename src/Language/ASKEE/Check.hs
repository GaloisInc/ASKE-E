{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.Check where

import           Control.Monad.State
import           Control.Monad.Writer (Writer, execWriter, tell)

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text

import           Language.ASKEE.Expr (Expr(..))
import           Language.ASKEE.ExprTransform (transformExpr)
import           Language.ASKEE.Panic (panic)
import           Language.ASKEE.Print (printExpr)
import           Language.ASKEE.Syntax (Event(..), Decl(..), Model(..))

import Prelude hiding (GT, EQ, LT, exp)

data Type = 
    Double
  | Boolean
  deriving Eq

instance Show Type where
  show Double = "<double>"
  show Boolean = "<bool>"

data ExprInfo =
  ExprInfo { exprs :: Map Text Expr
           , types :: Map Text Type
           }

type Check = StateT ExprInfo (Either String)

letRepr, stateRepr, assertRepr, eventRepr :: [Char]
letRepr = "let"
stateRepr = "state"
assertRepr = "assert"
eventRepr = "event"



checkPrint :: Model -> IO ()
checkPrint m =
  case checkModel m of
    Left err -> putStrLn err
    Right () -> pure ()

-- | Perform scope- and type-checking of a `Model`
checkModel :: Model -> Either String Model
checkModel m@Model{..} = evalStateT go (ExprInfo Map.empty Map.empty) >> pure m
  where
    go :: Check ()
    go = 
      do  checkDecls modelDecls
          checkEvents modelEvents

-- | Scope- and type-check model declarations
checkDecls :: [Decl] -> Check ()
checkDecls = mapM_ checkDecl
  where
    checkDecl :: Decl -> Check ()
    checkDecl d =
      case d of
        Let v e -> bindCheck letRepr v e
        State v e -> bindCheck stateRepr v e
        Assert e -> exprCheck assertRepr e

-- Scope- and type-check model events
checkEvents :: [Event] -> Check ()
checkEvents = mapM_ checkEvent
  where
    checkEvent :: Event -> Check ()
    checkEvent Event{..} =
      do  case eventWhen of
            Nothing -> pure ()
            Just w ->
              do  scopeCheck eventRepr Nothing w
                  ty <- typeCheck w
                  lift $ expect Nothing w Boolean ty

          scopeCheck eventRepr Nothing eventRate
          ty <- typeCheck eventRate
          lift $ expect Nothing eventRate Double ty

          forM_ eventEffect $ 
            \(var, expr) ->
              do  scopeCheck eventRepr (Just var) expr
                  ty <- typeCheck eventRate
                  lift $ expect Nothing expr Double ty

-- | Scope- and type-check an expression being bound to a variable, adding
-- the expression and type binding to state
bindCheck :: String -> Text -> Expr -> Check ()
bindCheck thing var expr =
  do  scopeCheck thing (Just var) expr
      ty <- typeCheck expr
      ExprInfo exprs types <- get
      let exprs' = Map.insert var expr exprs
          types' = Map.insert var ty types
      put $ ExprInfo exprs' types'

-- | Scope- and type-check a naked expression
exprCheck :: String -> Expr -> Check ()
exprCheck thing expr =
  do  scopeCheck thing Nothing expr
      _ <- typeCheck expr
      pure ()

-- | Scope-check an expression
scopeCheck :: String -> Maybe Text -> Expr -> Check ()
scopeCheck thing varM expr =
  do  ExprInfo es _ <- get
      let eVars = exprVars expr
          boundVars = Map.keysSet es
      unless (eVars `Set.isSubsetOf` boundVars) $
        let unboundVars = show $ Set.toList $ eVars Set.\\ boundVars 
            exprStr = show $ printExpr expr
        in  fail . unlines $
              [ "in \'"<>thing<>"\', expression"
              , exprStr
              , "references unbound variables:"
              , "  "<>unboundVars
              ]
      case varM of
        Nothing -> pure ()
        Just var
          | var `Set.member` boundVars && thing /= eventRepr -> 
            fail . unlines $ 
              [ "in \'"<>thing<>"\'-type (re)binding for \'"<>Text.unpack var<>"\':"
              , "binding cannot shadow an existing binding"
              ]
          -- the below branch may not be needed
          | var `Set.member` eVars && thing /= eventRepr -> 
            fail . unlines $ 
              [ "in \'"<>thing<>"\'-type binding for \'"<>Text.unpack var<>"\':"
              , "variable "<>Text.unpack var<>" cannot be referred to in its own expression:"
              , show (printExpr expr)
              ]
          | otherwise -> pure ()

-- | Type-check an expression
-- Should be run after scope-checking to reduce unbound variable errors
typeCheck :: Expr -> Check Type
typeCheck expr =
  do  ExprInfo _ ts <- get
      lift $ typeOf ts expr
      
-- | Type an expression
typeOf :: Map Text Type -> Expr -> Either String Type
typeOf bindings orig = ty orig
  where
    ty :: Expr -> Either String Type
    ty e = 
      case e of
        Add e1 e2   -> arithTy e1 e2
        Sub e1 e2   -> arithTy e1 e2
        Mul e1 e2   -> arithTy e1 e2
        Div e1 e2   -> arithTy e1 e2
        Neg e1      -> ty e1 >>= expect' e1 Double >> pure Double
        And e1 e2   -> logTy e1 e2
        Or e1 e2    -> logTy e1 e2
        Not e1      -> ty e1 >>= expect' e1 Boolean >> pure Boolean
        LT e1 e2    -> compTy e1 e2
        LTE e1 e2   -> compTy e1 e2
        EQ e1 e2    -> compTy e1 e2
        GTE e1 e2   -> compTy e1 e2
        GT e1 e2    -> compTy e1 e2
        If e1 e2 e3 ->
          do  t1 <- ty e1
              expect' e1 Boolean t1
              t2 <- ty e2
              t3 <- ty e3
              expect' e2 t2 t2
              expect' e3 t2 t3
              pure t2
        Cond [] _ -> panic 
          "no \'cond\' branches" 
          [ "while typechecking cond expression:"
          , show (printExpr e)
          ]
        Cond branches otherM ->
          do  forM_ [condition | (_, condition) <- branches] $ 
                \expr -> ty expr >>= expect' expr Boolean

              expected <- ty $ head [result | (result, _) <- branches]
              forM_ [result | (result, _) <- tail branches] $ 
                \expr -> ty expr >>= expect' expr expected

              case otherM of
                Just other -> ty other >>= expect' other expected
                Nothing -> pure ()
              pure expected
        Var v ->
          case bindings Map.!? v of
            Just t -> pure t
            Nothing -> panic 
              ("encountered unbound variable "<>Text.unpack v) 
              [ "while typechecking expression:"
              , show (printExpr orig)
              ]
        LitD _ -> pure Double
        LitB _ -> pure Boolean

    arithTy :: Expr -> Expr -> Either String Type
    arithTy e1 e2 =
      do  t1 <- ty e1
          t2 <- ty e2
          expect' e1 Double t1 >> expect' e2 Double t2 >> pure Double

    logTy :: Expr -> Expr -> Either String Type
    logTy e1 e2 =
      do  t1 <- ty e1
          t2 <- ty e2
          expect' e1 Boolean t1 >> expect' e2 Boolean t2 >> pure Boolean

    compTy :: Expr -> Expr -> Either String Type
    compTy e1 e2 =
      do  t1 <- ty e1
          t2 <- ty e2
          expect' e1 Double t1 >> expect' e2 Double t2 >> pure Boolean

    expect' :: Expr -> Type -> Type -> Either String ()
    expect' = expect (Just orig)

-- | All variables appearing in an expression
exprVars :: Expr -> Set Text
exprVars = Set.fromList . execWriter . transformExpr extractVars
  where
    extractVars :: Expr -> Writer [Text] Expr
    extractVars expr =
      case expr of
        Var v -> tell [v] >> pure expr
        _     ->             pure expr

-- | Simple type-checking utility function for expected types
expect :: Maybe Expr -> Expr -> Type -> Type -> Either String ()
expect context expr expected got = 
  if expected == got 
    then pure ()
    else 
      case context of
        Nothing -> fail . unlines $
          [ "type mismatch!"
          , "in:"
          , show (printExpr expr)
          , "expected type:"
          , "  "<>show expected
          , "but got type:"
          , "  "<>show got
          ]
        Just orig -> fail . unlines $
          [ "type mismatch!"
          , "in:"
          , show (printExpr expr)
          , "in expression:"
          , show (printExpr orig)
          , "expected type:"
          , "  "<>show expected
          , "but got type:"
          , "  "<>show got
          ]

