module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

cstTest :: TestTree
cstTest = testCase "Constant expression" $ eval envEmpty (CstInt 2) @?= Right (ValInt 2)

addTest :: TestTree
addTest = testCase "Add integers" $ eval envEmpty (Add (CstInt 4) (CstInt 2)) @?= Right (ValInt 6)

subTest :: TestTree
subTest = testCase "Sub integers" $ eval envEmpty (Sub (CstInt 4) (CstInt 2)) @?= Right (ValInt 2)

mulTest :: TestTree
mulTest = testCase "Mul integers" $ eval envEmpty (Mul (CstInt 4) (CstInt 2)) @?= Right (ValInt 8)

divTest :: TestTree
divTest = testCase "Div integers" $ eval envEmpty (Div (CstInt 4) (CstInt 2)) @?= Right (ValInt 2)

powTest :: TestTree
powTest = testCase "Pow integers" $ eval envEmpty (Pow (CstInt 4) (CstInt 2)) @?= Right (ValInt 16)

divZeroTest :: TestTree
divZeroTest = testCase "Div 0" $ eval envEmpty (Div (CstInt 4) (Sub (CstInt 2) (CstInt 2))) @?= Left "Zero division"

powNegTest :: TestTree
powNegTest = testCase "Pow neg" $ eval envEmpty (Pow (CstInt 4) (Sub (CstInt 2) (CstInt 3))) @?= Left "Negative exponent"

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ cstTest,
      addTest,
      subTest,
      mulTest,
      divTest,
      powTest,
      divZeroTest,
      powNegTest,
      testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non integer binop",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Zero division",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True)
    ]
