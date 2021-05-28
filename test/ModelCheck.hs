{-# LANGUAGE OverloadedStrings #-}

module ModelCheck where

import Control.Monad (when)
import Control.Monad.State (evalStateT)

import Data.Either (isLeft)

import Language.ASKEE.Check
import Language.ASKEE.Expr as Expr
import Language.ASKEE.Syntax

import Test.Tasty(TestTree, TestName)
import Test.Tasty.HUnit hiding (State)

import System.Exit (exitFailure)

simpleCheckDecls :: [Decl] -> Either String ()
simpleCheckDecls ds = evalStateT (checkDecls ds) initExprInfo

expectLeft, expectRight :: Either String a -> IO ()
expectLeft = assert . isLeft
expectRight = either assertFailure pass
  where
    pass = const (pure ())

(~:) :: TestName -> Assertion -> TestTree
(~:) = testCase

infixr 0 ~:

testEmpty :: TestTree
testEmpty =
  "empty decls" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = []

testLet :: TestTree
testLet =
  "single let" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ Let "foo" (LitD 3) ]

testState :: TestTree
testState =
  "single state" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ State "foo" (LitD 3) ]

testAssert :: TestTree
testAssert =
  "single assert" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ Assert (LitB False) ]

testAssertNonBool :: TestTree
testAssertNonBool =
  "assert a non-boolean" ~:
    expectLeft $ simpleCheckDecls decls
  where
    decls = [ Assert (LitD 3) ]

testTimeInScope :: TestTree
testTimeInScope =
  "time automatically in scope" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ Assert ((Var "time") `GTE` (LitD 0)) ]

testUnscoped :: TestTree
testUnscoped =
  "foo not in scope" ~:
    expectLeft $ simpleCheckDecls decls
  where
    decls = [ Assert (Var "foo") ]

testLetAssert :: TestTree
testLetAssert =
  "let-assert" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ Let "foo" (LitB True)
            , Assert (Var "foo")
            ]

testStateAssert :: TestTree
testStateAssert =
  "state-assert" ~:
    expectRight $ simpleCheckDecls decls
  where
    decls = [ State "foo" (LitB True)
            , Assert (Var "foo")
            ]

testLetAssertNonBool :: TestTree
testLetAssertNonBool =
  "let-assert non-bool" ~:
    expectLeft $ simpleCheckDecls decls
  where
    decls = [ Let "foo" (LitD 3)
            , Assert (Var "foo")
            ]

testStateAssertNonBool :: TestTree
testStateAssertNonBool =
  "state-assert non-bool" ~:
    expectLeft $ simpleCheckDecls decls
  where
    decls = [ State "foo" (LitD 3)
            , Assert (Var "foo")
            ]



simpleTypeOf :: Expr -> Either String Type
simpleTypeOf e = evalStateT (typeCheck e) initExprInfo

testTyLitD :: TestTree
testTyLitD =
  "type of literal number" ~:
    Right Double @=? simpleTypeOf (LitD 3)

testTyLitB :: TestTree
testTyLitB =
  "type of literal number" ~:
    Right Boolean @=? simpleTypeOf (LitB True)

testTyAdd :: TestTree
testTyAdd =
  "well-typed (+)" ~:
    Right Double @=? simpleTypeOf (Add (LitD 3) (LitD 3))

testTyAddErr :: TestTree
testTyAddErr =
  "ill-typed (+)" ~:
    expectLeft $ simpleTypeOf (Add (LitD 3) (LitB True))

testTySub :: TestTree
testTySub =
  "well-typed (-)" ~:
    Right Double @=? simpleTypeOf (Sub (LitD 3) (LitD 3))

testTySubErr :: TestTree
testTySubErr =
  "ill-typed (-)" ~:
    expectLeft $ simpleTypeOf (Sub (LitD 3) (LitB True))

testTyMul :: TestTree
testTyMul =
  "well-typed (*)" ~:
    Right Double @=? simpleTypeOf (Mul (LitD 3) (LitD 3))

testTyMulErr :: TestTree
testTyMulErr =
  "ill-typed (*)" ~:
    expectLeft $ simpleTypeOf (Mul (LitD 3) (LitB True))

testTyDiv :: TestTree
testTyDiv =
  "well-typed (/)" ~:
    Right Double @=? simpleTypeOf (Div (LitD 3) (LitD 3))

testTyDivErr :: TestTree
testTyDivErr =
  "ill-typed (/)" ~:
    expectLeft $ simpleTypeOf (Div (LitD 3) (LitB True))

testTyNeg :: TestTree
testTyNeg =
  "well-typed negate" ~:
    Right Double @=? simpleTypeOf (Neg (LitD 3))

testTyNegErr :: TestTree
testTyNegErr =
  "ill-typed negate" ~:
    expectLeft $ simpleTypeOf (Neg (LitB True))

testTyAnd :: TestTree
testTyAnd =
  "well-typed (&&)" ~:
    Right Boolean @=? simpleTypeOf (And (LitB True) (LitB True))

testTyAndErr :: TestTree
testTyAndErr =
  "ill-typed (&&)" ~:
    expectLeft $ simpleTypeOf (And (LitD 3) (LitB True))

testTyOr :: TestTree
testTyOr =
  "well-typed (||)" ~:
    Right Boolean @=? simpleTypeOf (Or (LitB True) (LitB True))

testTyOrErr :: TestTree
testTyOrErr =
  "ill-typed (||)" ~:
    expectLeft $ simpleTypeOf (Or (LitD 3) (LitB True))

testTyNot :: TestTree
testTyNot =
  "well-typed not" ~:
    Right Boolean @=? simpleTypeOf (Not (LitB True))

testTyNotErr :: TestTree
testTyNotErr =
  "ill-typed not" ~:
    expectLeft $ simpleTypeOf (Not (LitD 3))

testTyLT :: TestTree
testTyLT =
  "well-typed LT" ~:
    Right Boolean @=? simpleTypeOf (Expr.LT (LitD 3) (LitD 3))

testTyLTErr :: TestTree
testTyLTErr =
  "ill-typed LT" ~:
    expectLeft $ simpleTypeOf (Expr.LT (LitD 3) (LitB True))

testTyLTE :: TestTree
testTyLTE =
  "well-typed LTE" ~:
    Right Boolean @=? simpleTypeOf (LTE (LitD 3) (LitD 3))

testTyLTEErr :: TestTree
testTyLTEErr =
  "ill-typed LTE" ~:
    expectLeft $ simpleTypeOf (LTE (LitD 3) (LitB True))

testTyEQ :: TestTree
testTyEQ =
  "well-typed EQ" ~:
    Right Boolean @=? simpleTypeOf (Expr.EQ (LitD 3) (LitD 3))

testTyEQErr :: TestTree
testTyEQErr =
  "ill-typed EQ" ~:
    expectLeft $ simpleTypeOf (Expr.EQ (LitD 3) (LitB True))

testTyGTE :: TestTree
testTyGTE =
  "well-typed GTE" ~:
    Right Boolean @=? simpleTypeOf (GTE (LitD 3) (LitD 3))

testTyGTEErr :: TestTree
testTyGTEErr =
  "ill-typed GTE" ~:
    expectLeft $ simpleTypeOf (GTE (LitD 3) (LitB True))

testTyGT :: TestTree
testTyGT =
  "well-typed GT" ~:
    Right Boolean @=? simpleTypeOf (Expr.GT (LitD 3) (LitD 3))

testTyGTErr :: TestTree
testTyGTErr =
  "ill-typed GT" ~:
    expectLeft $ simpleTypeOf (Expr.GT (LitD 3) (LitB True))

testIfDouble :: TestTree
testIfDouble =
  "well-typed if resulting in double" ~:
    Right Double @=? simpleTypeOf (If (LitB True) (LitD 4) (LitD 5))

testIfBool :: TestTree
testIfBool =
  "well-typed if resulting in boolean" ~:
    Right Boolean @=? simpleTypeOf (If (LitB True) (LitB False) (LitB True))

testIfBranchErr :: TestTree
testIfBranchErr =
  "ill-typed if with conflicting branches" ~:
    expectLeft $ simpleTypeOf (If (LitB True) (LitB False) (LitD 3))

testIfCondErr :: TestTree
testIfCondErr =
  "ill-typed if with numeric condition" ~:
    expectLeft $ simpleTypeOf (If (LitD 3) (LitD 3) (LitD 3))

testCond :: TestTree
testCond =
  "well-typed cond without 'otherwise'" ~:
    Right Double @=? simpleTypeOf (Cond [(LitD 3, LitB True)] Nothing)

testCondOther :: TestTree
testCondOther =
  "well-typed cond with 'otherwise'" ~:
    Right Double @=? simpleTypeOf (Cond [(LitD 3, LitB True)] (Just (LitD 4)))

testCondErr :: TestTree
testCondErr =
  "ill-typed cond from numeric condition" ~:
    expectLeft $ simpleTypeOf (Cond [(LitD 3, LitD 3)] Nothing)

testCondErrOther :: TestTree
testCondErrOther =
  "ill-typed cond from disagreeably-typed 'otherwise'" ~:
    expectLeft $ simpleTypeOf (Cond [(LitD 3, LitB True)] (Just (LitB True)))

testCondErrBranches :: TestTree
testCondErrBranches =
  "ill-typed cond from disagreeably-typed branches" ~:
    expectLeft $ simpleTypeOf (Cond [(LitD 3, LitB True), (LitB True, LitB True)] Nothing)

testVarRef :: TestTree
testVarRef =
  "well-typed previously-declared variable" ~:
    Right Double @=? evalStateT (bind >> typeCheck expr) initExprInfo
  where
    bind :: Check ()
    bind = bindCheck "<unimportant>" "foo" (LitD 4)

    expr :: Expr
    expr = Var "foo"

testVarRefErr :: TestTree
testVarRefErr =
  "well-typed previously-declared variable in ill-typed expression" ~:
    expectLeft $ evalStateT (bind >> typeCheck expr) initExprInfo
  where
    bind :: Check ()
    bind = bindCheck "<unimportant>" "foo" (LitD 4)

    expr :: Expr
    expr = And (Var "foo") (Var "foo")

testTimeRef :: TestTree
testTimeRef =
  "well-typed time variable" ~:
    Right Double @=? simpleTypeOf (Var "time")

testTimeRefErr :: TestTree
testTimeRefErr =
  "well-typed time variable in ill-typed expression" ~:
    expectLeft $ simpleTypeOf (And (Var "time") (Var "time"))

tests :: [TestTree]
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

-- main :: IO ()
-- main =
--   do  counts <- runTestTT (TestList tests)
--       when (errors counts + failures counts > 0)
--         exitFailure