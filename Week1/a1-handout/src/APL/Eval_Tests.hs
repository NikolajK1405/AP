module APL.Eval_Tests (tests) where

import APL.AST (Exp (..), printExp)
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- Consider this example when you have added the necessary constructors.
-- The Y combinator in a form suitable for strict evaluation.
yComb :: Exp
yComb =
  Lambda "f" $
    Apply
      (Lambda "g" (Apply (Var "g") (Var "g")))
      ( Lambda
          "g"
          ( Apply
              (Var "f")
              (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
          )
      )

fact :: Exp
fact =
  Apply yComb $
    Lambda "rec" $
      Lambda "n" $
        If
          (Eql (Var "n") (CstInt 0))
          (CstInt 1)
          (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
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
          @?= Left "Division by zero",
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
          @?= Right (ValBool True),
          --
          -- TODO - add more
      testCase "ForLoop" $
        eval envEmpty ( ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
          @?= Right (ValInt 45),

      testCase "ForLoop with non-integral" $
        eval envEmpty ( ForLoop ("p", CstBool False) ("i", CstInt 10) (Add (Var "p") (Var "i")))
          @?= Left "Non-integer operand",

      testCase "ValFun" $ eval envEmpty (Let "X" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))))
          @?= Right (ValFun [("X", ValInt 2)] "y" (Add (Var "x") (Var "y"))),

      testCase "Apply" $ eval envEmpty (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
          @?= Right (ValInt 5),

      testCase "Apply non function" $ eval envEmpty (Apply (CstInt 0) (Var "x"))
          @?= Left "Apply non function",

      testCase "Apply unbound var" $ eval envEmpty (Apply (Lambda "y" (Var "y")) (Var "x"))
          @?= Left "Unknown variable: x",

      testCase "Apply unused arg" $ eval envEmpty (Apply (Lambda "x" (CstInt 0)) (Div (CstInt 1) (CstInt 0)))
          @?= Left "Division by zero",

      testCase "TryCatch succes" $ eval envEmpty (TryCatch (CstInt 0) (CstInt 1))
          @?= Right (ValInt 0),

      testCase "TryCatch fail" $ eval envEmpty (TryCatch (Var "missing") (CstInt 1))
          @?= Right (ValInt 1),

      testCase "Factorial" $ eval envEmpty (Apply fact (CstInt 5))
          @?= Right (ValInt 120),

      testCase "Print1" $ printExp (ForLoop ("acc", CstInt 0) ("i", CstInt 3) (Add (Var "acc") (Mul (Var "i") (Var "i"))))
          @?= "(loop acc = 0 for i < 3 do (acc + (i * i)))",

      testCase "Print2" $ printExp (Let "x" (Add (CstInt 2) (CstInt 3)) (If (Eql (Var "x") (CstInt 5)) (Mul (Var "x") (CstInt 10)) (Div (Var "x") (CstInt 2))))
          @?= "(let x = (2 + 3) in (if (x == 5) then (x * 10) else (x / 2)))",

      testCase "Print3" $ printExp (TryCatch (Div (CstInt 1) (CstInt 0)) (Let "x" (CstInt 42) (Var "x")))
          @?= "(try (1 / 0) catch (let x = 42 in x))"
    ]
