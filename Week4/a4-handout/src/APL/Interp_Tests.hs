module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
--import Control.Applicative.Lift (failure)

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),

      testCase "TryCatchOp" $
        runEval ( 
          Free ( TryCatchOp (failure "Oh no!") (pure $ ValInt 1) pure)
        ) @?= ([], Right (ValInt 1)),
      
      testCase "TryCatch" $
        runEval ( 
          eval ( TryCatch (CstInt 5) (Div (CstInt 1) (CstInt 0)))
        ) @?= ([], Right (ValInt 5)),

      testCase "KvPutOp and KvGetOp" $
        runEval (Free ( (KvPutOp (ValInt 0) (ValInt 1)) (Free $ KvGetOp (ValInt 0) $ \val -> pure val) )) @?= ([],Right (ValInt 1)),

      testCase "Transaction goodPut" $
      runEval ( eval $ Let "_" (Transaction (KvPut (CstInt 0) (CstInt 1))) (KvGet $ CstInt 0)) @?= ([], Right (ValInt 1)),

      testCase "Transaction badPut" $
      runEval ( eval ( TryCatch (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))) (KvGet $ CstInt 0))) @?= ([], Left "Unknown key: ValInt 0"),

      testCase "Transaction unknown var" $
      runEval ( eval $ (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))) @?= ([], Left "Unknown variable: die"),

      testCase "Transaction oh shit" $
      runEval ( transaction (evalPrint "weee" >> failure "oh shit")) @?= (["weee"], Left "oh shit"),

      testCase "Transaction nested" $
      runEval (eval (Let "_" (Transaction (Let "_" (KvPut (CstInt 0) (CstInt 1)) (TryCatch (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))) (CstBool True)))) (KvGet $ CstInt 0) )) @?= ([], Right (ValInt 1)),

      testCase "Transaction reject nested" $
      runEval (eval (Let "_" (TryCatch (Transaction (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))) ) (CstBool True)) (KvGet $ CstInt 0))) @?= ([], Left "Unknown key: ValInt 0"),

      testCase "ForLoop working" $ 
      runEval ( eval ( ForLoop ("p", CstInt 0) ("i", CstInt 100) (Let "_"  (Break (CstBool True)) (Var "i")))) @?= ([], Right (ValBool True)),

      testCase "ForLoop Failure" $
      runEval (eval (Break (CstBool True))) @?= ([], Left "Break outside loop")
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ()),

        testCase "TryCatchOp" $ do 
          let badEql = Eql (CstInt 0) (CstBool True) 
              divZero = Div (CstInt 1) (CstInt 0)
          (out,res) <- captureIO[] $ runEvalIO $ eval $ TryCatch badEql divZero
          (out,res) @?= ([], Left "Division by zero"),

        testCase "Missing keys 1" $ do 
          (out,res) <- captureIO["ValInt 5"] $ runEvalIO $ eval $ KvGet (CstInt 0) 
          out @?= ["Invalid key: ValInt 0. Enter a replacement: "]
          res @?= Right (ValInt 5),

        testCase "Missing keys 2" $ do
          (out, res) <- captureIO ["ValBool True"] $ runEvalIO $ evalKvGet (ValInt 0)
          out @?= ["Invalid key: ValInt 0. Enter a replacement: "]
          res @?= Right (ValBool True),

        testCase "Missing keys lol" $ do
          (out, res) <- captureIO ["lol"] $ runEvalIO $ eval $ KvGet (CstInt 0)
          out @?= ["Invalid key: ValInt 0. Enter a replacement: "]
          res @?= Left "Invalid value input: lol"

        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        -- testCase "print 2" $ do
        --    (out, res) <-
        --      captureIO [] $
        --        evalIO' $
        --          Print "This is also 1" $
        --            Print "This is 1" $
        --              CstInt 1
        --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]
