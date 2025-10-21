module APL.Tests where

import APL.AST (Exp (..), VName)
import Test.QuickCheck (Gen,
                        oneof,
                        elements,
                        listOf,
                        sample,
                        sized,
                        Arbitrary (arbitrary, shrink),
                        quickCheck)
import APL.Eval (runEval, eval)

genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (alpha : alphaNums)

genExp :: Int -> Gen Exp
genExp size =
  if size <= 1
  then oneof [CstInt <$> arbitrary, CstBool <$> arbitrary, Var <$> genVar]
  else
    oneof
      [ CstInt <$> arbitrary,
        CstBool <$> arbitrary,
        Add <$> genExp half <*> genExp half,
        Sub <$> genExp half <*> genExp half,
        Mul <$> genExp half <*> genExp half,
        Div <$> genExp half <*> genExp half,
        Pow <$> genExp half <*> genExp half,
        Eql <$> genExp half <*> genExp half,
        If <$> genExp third <*> genExp third <*> genExp third,
        Var <$> genVar,
        Let <$> genVar <*> genExp half <*> genExp half,
        Lambda <$> genVar <*> genExp (size - 2),
        Apply <$> genExp half <*> genExp half,
        TryCatch <$> genExp half <*> genExp half
      ]
   where
     half =  size `div` 2
     third = size `div` 3

shrinkBinOp :: (Exp -> Exp -> Exp) -> Exp -> Exp -> [Exp]
shrinkBinOp op e1 e2 =
    e1 : e2 : [op e1' e2 | e1' <- shrink e1] ++ [op e1 e2' | e2' <- shrink e2]

instance Arbitrary Exp where
  arbitrary = sized genExp

  shrink (CstInt n) = [CstInt n' | n' <- shrink n]
  shrink (CstBool _) = []
  shrink (Add e1 e2) = shrinkBinOp (Add) e1 e2
  shrink (Sub e1 e2) = shrinkBinOp (Sub) e1 e2
  shrink (Mul e1 e2) = shrinkBinOp (Mul) e1 e2
  shrink (Div e1 e2) = shrinkBinOp (Div) e1 e2
  shrink (Pow e1 e2) = shrinkBinOp (Pow) e1 e2
  shrink (Eql e1 e2) = shrinkBinOp (Eql) e1 e2
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Var x) =
    [Var x' | x' <- shrink x, not (null x')]
  shrink (Let vn e1 e2) =
    e1 : [Let vn e1' e2 | e1' <- shrink e1] ++ [Let vn e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    e : [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) = shrinkBinOp (Apply) e1 e2
  shrink (TryCatch e1 e2) = shrinkBinOp (TryCatch) e1 e2


prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc n1 n2 n3 = (n1 + n2) + n3 == n1 + (n2 + n3)

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc e1 e2 e3 = runEval (eval (Add (Add e1 e2) e3)) == runEval (eval (Add e1 (Add e2 e3)))
