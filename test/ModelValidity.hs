{-# LANGUAGE OverloadedStrings #-}

module ModelValidity where

import Control.Monad (when)
import Control.Monad.State (evalStateT)

import Data.Either (isLeft)

import Language.ASKEE.Check
import Language.ASKEE.Expr as Expr
import Language.ASKEE.Syntax

import Test.HUnit hiding (State)

import System.Exit (exitFailure)

simpleCheckDecls :: [Decl] -> Either String ()
simpleCheckDecls ds = evalStateT (checkDecls ds) initExprInfo

expectLeft, expectRight :: Either String a -> IO ()
expectLeft = assert . isLeft
expectRight = either assertFailure pass
  where
    pass = const (pure ())

testEmpty :: Test
testEmpty =
  "empty decls" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = []

testLet :: Test
testLet =
  "single let" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ Let "foo" (LitD 3) ]

testState :: Test
testState =
  "single state" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ State "foo" (LitD 3) ]

testAssert :: Test
testAssert =
  "single assert" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ Assert (LitB False) ]

testAssertNonBool :: Test
testAssertNonBool =
  "assert a non-boolean" ~:
    expectLeft $ simpleCheckDecls decls
  where
    decls = [ Assert (LitD 3) ]

testTimeInScope :: Test
testTimeInScope =
  "time automatically in scope" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ Assert ((Var "time") `GTE` (LitD 0)) ]

testUnscoped :: Test
testUnscoped =
  "foo not in scope" ~:
    expectLeft $ simpleCheckDecls decls
  where
    decls = [ Assert (Var "foo") ]

testLetAssert :: Test
testLetAssert =
  "let-assert" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ Let "foo" (LitB True)
            , Assert (Var "foo")
            ]

testStateAssert :: Test
testStateAssert =
  "state-assert" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ State "foo" (LitB True)
            , Assert (Var "foo")
            ]

testLetAssertNonBool :: Test
testLetAssertNonBool =
  "let-assert non-bool" ~:
    expectLeft $ simpleCheckDecls decls
  where
    decls = [ Let "foo" (LitD 3)
            , Assert (Var "foo")
            ]

testStateAssertNonBool :: Test
testStateAssertNonBool =
  "state-assert non-bool" ~:
    expectLeft $ simpleCheckDecls decls
  where
    decls = [ State "foo" (LitD 3)
            , Assert (Var "foo")
            ]



simpleTypeOf :: Expr -> Either String Type
simpleTypeOf e = evalStateT (typeCheck e) initExprInfo

testTyLitD :: Test
testTyLitD =
  "type of literal number" ~:
    Right Double @=? simpleTypeOf (LitD 3)

testTyLitB :: Test
testTyLitB =
  "type of literal number" ~:
    Right Boolean @=? simpleTypeOf (LitB True)

testTyAdd :: Test
testTyAdd =
  "well-typed (+)" ~:
    Right Double @=? simpleTypeOf (Add (LitD 3) (LitD 3))

testTyAddErr :: Test
testTyAddErr =
  "ill-typed (+)" ~:
    expectLeft $ simpleTypeOf (Add (LitD 3) (LitB True))

testTySub :: Test
testTySub =
  "well-typed (-)" ~:
    Right Double @=? simpleTypeOf (Sub (LitD 3) (LitD 3))

testTySubErr :: Test
testTySubErr =
  "ill-typed (-)" ~:
    expectLeft $ simpleTypeOf (Sub (LitD 3) (LitB True))

testTyMul :: Test
testTyMul =
  "well-typed (*)" ~:
    Right Double @=? simpleTypeOf (Mul (LitD 3) (LitD 3))

testTyMulErr :: Test
testTyMulErr =
  "ill-typed (*)" ~:
    expectLeft $ simpleTypeOf (Mul (LitD 3) (LitB True))

testTyDiv :: Test
testTyDiv =
  "well-typed (/)" ~:
    Right Double @=? simpleTypeOf (Div (LitD 3) (LitD 3))

testTyDivErr :: Test
testTyDivErr =
  "ill-typed (/)" ~:
    expectLeft $ simpleTypeOf (Div (LitD 3) (LitB True))

testTyNeg :: Test
testTyNeg =
  "well-typed negate" ~:
    Right Double @=? simpleTypeOf (Neg (LitD 3))

testTyNegErr :: Test
testTyNegErr =
  "ill-typed negate" ~:
    expectLeft $ simpleTypeOf (Neg (LitB True))

testTyAnd :: Test
testTyAnd =
  "well-typed (&&)" ~:
    Right Boolean @=? simpleTypeOf (And (LitB True) (LitB True))

testTyAndErr :: Test
testTyAndErr =
  "ill-typed (&&)" ~:
    expectLeft $ simpleTypeOf (And (LitD 3) (LitB True))

testTyOr :: Test
testTyOr =
  "well-typed (||)" ~:
    Right Boolean @=? simpleTypeOf (Or (LitB True) (LitB True))

testTyOrErr :: Test
testTyOrErr =
  "ill-typed (||)" ~:
    expectLeft $ simpleTypeOf (Or (LitD 3) (LitB True))

testTyNot :: Test
testTyNot =
  "well-typed not" ~:
    Right Boolean @=? simpleTypeOf (Not (LitB True))

testTyNotErr :: Test
testTyNotErr =
  "ill-typed not" ~:
    expectLeft $ simpleTypeOf (Not (LitD 3))

testTyLT :: Test
testTyLT =
  "well-typed LT" ~:
    Right Boolean @=? simpleTypeOf (Expr.LT (LitD 3) (LitD 3))

testTyLTErr :: Test
testTyLTErr =
  "ill-typed LT" ~:
    expectLeft $ simpleTypeOf (Expr.LT (LitD 3) (LitB True))

testTyLTE :: Test
testTyLTE =
  "well-typed LTE" ~:
    Right Boolean @=? simpleTypeOf (LTE (LitD 3) (LitD 3))

testTyLTEErr :: Test
testTyLTEErr =
  "ill-typed LTE" ~:
    expectLeft $ simpleTypeOf (LTE (LitD 3) (LitB True))

testTyEQ :: Test
testTyEQ =
  "well-typed EQ" ~:
    Right Boolean @=? simpleTypeOf (Expr.EQ (LitD 3) (LitD 3))

testTyEQErr :: Test
testTyEQErr =
  "ill-typed EQ" ~:
    expectLeft $ simpleTypeOf (Expr.EQ (LitD 3) (LitB True))

testTyGTE :: Test
testTyGTE =
  "well-typed GTE" ~:
    Right Boolean @=? simpleTypeOf (GTE (LitD 3) (LitD 3))

testTyGTEErr :: Test
testTyGTEErr =
  "ill-typed GTE" ~:
    expectLeft $ simpleTypeOf (GTE (LitD 3) (LitB True))

testTyGT :: Test
testTyGT =
  "well-typed GT" ~:
    Right Boolean @=? simpleTypeOf (Expr.GT (LitD 3) (LitD 3))

testTyGTErr :: Test
testTyGTErr =
  "ill-typed GT" ~:
    expectLeft $ simpleTypeOf (Expr.GT (LitD 3) (LitB True))

testIfDouble :: Test
testIfDouble =
  "well-typed if resulting in double" ~:
    Right Double @=? simpleTypeOf (If (LitB True) (LitD 4) (LitD 5))

testIfBool :: Test
testIfBool =
  "well-typed if resulting in boolean" ~:
    Right Boolean @=? simpleTypeOf (If (LitB True) (LitB False) (LitB True))

testIfBranchErr :: Test
testIfBranchErr =
  "ill-typed if with conflicting branches" ~:
    expectLeft $ simpleTypeOf (If (LitB True) (LitB False) (LitD 3))

testIfCondErr :: Test
testIfCondErr =
  "ill-typed if with numeric condition" ~:
    expectLeft $ simpleTypeOf (If (LitD 3) (LitD 3) (LitD 3))

testCond :: Test
testCond =
  "well-typed cond without 'otherwise'" ~:
    Right Double @=? simpleTypeOf (Cond [(LitD 3, LitB True)] Nothing)

testCondOther :: Test
testCondOther =
  "well-typed cond with 'otherwise'" ~:
    Right Double @=? simpleTypeOf (Cond [(LitD 3, LitB True)] (Just (LitD 4)))

testCondErr :: Test
testCondErr =
  "ill-typed cond from numeric condition" ~:
    expectLeft $ simpleTypeOf (Cond [(LitD 3, LitD 3)] Nothing)

testCondErrOther :: Test
testCondErrOther =
  "ill-typed cond from disagreeably-typed 'otherwise'" ~:
    expectLeft $ simpleTypeOf (Cond [(LitD 3, LitB True)] (Just (LitB True)))

testCondErrBranches :: Test
testCondErrBranches =
  "ill-typed cond from disagreeably-typed branches" ~:
    expectLeft $ simpleTypeOf (Cond [(LitD 3, LitB True), (LitB True, LitB True)] Nothing)

testVarRef :: Test
testVarRef =
  "well-typed previously-declared variable" ~:
    Right Double @=? evalStateT (bind >> typeCheck expr) initExprInfo
  where
    bind :: Check ()
    bind = bindCheck "<unimportant>" "foo" (LitD 4)

    expr :: Expr
    expr = Var "foo"

testVarRefErr :: Test
testVarRefErr =
  "well-typed previously-declared variable in ill-typed expression" ~:
    expectLeft $ evalStateT (bind >> typeCheck expr) initExprInfo
  where
    bind :: Check ()
    bind = bindCheck "<unimportant>" "foo" (LitD 4)

    expr :: Expr
    expr = And (Var "foo") (Var "foo")

testTimeRef :: Test
testTimeRef =
  "well-typed time variable" ~:
    Right Double @=? simpleTypeOf (Var "time")

testTimeRefErr :: Test
testTimeRefErr =
  "well-typed time variable in ill-typed expression" ~:
    expectLeft $ simpleTypeOf (And (Var "time") (Var "time"))

tests :: [Test]
tests = 
  [ testEmpty
  , testLet
  , testState
  , testAssert
  , testAssertNonBool
  , testTimeInScope
  , testUnscoped
  , testLetAssert
  , testStateAssert
  , testLetAssertNonBool
  , testStateAssertNonBool

  , testTyLitD
  , testTyLitB
  , testTyAdd
  , testTyAddErr
  , testTySub
  , testTySubErr
  , testTyMul
  , testTyMulErr
  , testTyDiv
  , testTyDivErr
  , testTyNeg
  , testTyNegErr
  , testTyAnd
  , testTyAndErr
  , testTyOr
  , testTyOrErr
  , testTyNot
  , testTyNotErr
  , testTyLT
  , testTyLTErr
  , testTyLTE
  , testTyLTEErr
  , testTyEQ
  , testTyEQErr
  , testTyGTE
  , testTyGTEErr
  , testTyGT
  , testTyGTErr
  , testIfDouble
  , testIfBool
  , testIfBranchErr
  , testIfCondErr
  , testCond
  , testCondOther
  , testCondErr
  , testCondErrOther
  , testCondErrBranches
  , testVarRef
  , testVarRefErr
  , testTimeRef
  , testTimeRefErr
  ]

main :: IO ()
main = 
  do  counts <- runTestTT (TestList tests)
      when (errors counts + failures counts > 0)
        exitFailure