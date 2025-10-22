module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), printExp, subExp, VName)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import APL.Parser (parseAPL)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , quickCheck
  , chooseInt
  , elements
  , vectorOf
  , frequency
  , sample
  , withMaxSuccess 
  )

instance Arbitrary Exp where
  arbitrary = sized $ \n -> genExp n []

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genVar :: Gen VName
genVar = do
  n <- chooseInt (1,3)
  alpha <- elements ['a' .. 'z']
  alphaNums <- vectorOf n $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
  let v = (alpha : alphaNums)
  if v `elem` keywords
    then genVar
    else pure v
  

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch"
  ]

genExp :: Int -> [VName] -> Gen Exp
genExp 0 [] = oneof [CstInt <$> (abs <$> arbitrary), CstBool <$> arbitrary]
genExp 0 vs = Var <$> elements vs
genExp size vs = do
  let varChance = case vs of
        [] -> 1
        _ -> 5
  frequency
    [ (10, CstInt <$> ( abs <$> arbitrary))
    , (5, CstBool <$> arbitrary)
    , (7, Add <$> genExp halfSize vs <*> genExp halfSize vs)
    , (7, Sub <$> genExp halfSize vs <*> genExp halfSize vs)
    , (7, Mul <$> genExp halfSize vs <*> genExp halfSize vs)
    , (5, Div <$> genExp halfSize vs <*> genExp halfSize vs)
    , (5, Pow <$> genExp halfSize vs <*> genExp halfSize vs)
    , (5, Eql <$> genExp halfSize vs <*> genExp halfSize vs)
    , (5, If <$> genExp thirdSize vs <*> genExp thirdSize vs <*> genExp thirdSize vs)
    , (varChance, Var <$> case vs of
          [] -> genVar -- Impossible, but here so no warnings
          _ -> elements vs)
    , (10, do
          v <- genVar
          e1 <- genExp halfSize vs
          e2 <- genExp halfSize $ v : vs
          pure $ Let v e1 e2)
    , (10, do
          v <- genVar
          e <- genExp (size - 1) $ v : vs
          pure $ Lambda v e)
    , (5, Apply <$> genExp halfSize vs <*> genExp halfSize vs)
    , (7, TryCatch <$> genExp halfSize vs <*> genExp halfSize vs)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted e =
  case parseAPL "input" (printExp e) of
    Right e' -> e' == e
    Left _ -> False 
    


onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]
