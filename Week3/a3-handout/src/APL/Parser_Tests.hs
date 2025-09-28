module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
--import Text.Parsec (parserFail)
--import Distribution.TestSuite (testGroup)
--import Language.Haskell.TH.Syntax (NameSpace(VarName))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ],
      testGroup
        "Function application"
        [ parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x(y z)" $ Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTestFail "x if x then y else z"
        ],
      testGroup
        "Equality and power operators"
        [ parserTest "x*y**z" $ Mul (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x+y==y+x" $ Eql (Add (Var "x") (Var "y")) (Add (Var "y") (Var "x"))
        ],
      testGroup
        "Printing, putting, and getting"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y"),
          parserTest "getx" $ Var "getx",
          parserTest "print \"foo\" x" $ Print "foo" (Var "x")
        ],
      testGroup
        "Lambda, Let, TryCatch and ForLoop"
        [
          parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z"),
          parserTestFail "let true = y in z",
          parserTestFail "x let v = 2 in v",
          parserTest " \\x -> y + z" $ Lambda "x" (Add (Var "y") (Var "z")),
          parserTest " \\x -> 2" $ Lambda "x" (CstInt 2),
          parserTestFail "\\let -> x",
          parserTestFail "\\true -> x",
          parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
          parserTestFail "try x",
          parserTestFail " try x catch",
          parserTest "try f x catch g y" $ TryCatch (Apply (Var "f") (Var "x")) (Apply (Var "g") (Var "y")),
          parserTest "loop x = 1 for n < 10 do x + 1" $ ForLoop ("x", CstInt 1) ("n", CstInt 10) (Add (Var "x") (CstInt 1)),
          parserTestFail "loop let for n < 10 do x + 1",
          parserTestFail "loop true = 1 for n < 10 do x + 1",
          parserTestFail "loop x = 1 for n < 10 x + 1"
        ]
    ]
