module APL.Eval
  ( Val (..),
    Env,
    envEmpty,
    eval,
  )
where

import APL.AST (Exp (..), VName)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String


type Env = [(VName, Val)]

evalBinOp :: Env -> (Integer -> Integer -> Either Error Integer) -> Exp -> Exp -> Either Error Val
evalBinOp env op e1 e2 =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt v1), Right (ValInt v2)) -> case op v1 v2 of
      (Left err) -> Left err
      (Right v) -> Right $ ValInt v
    (_, _) -> Left "Non integer binop"

eval :: Env -> Exp -> Either Error Val
eval _ (CstInt n) = Right $ ValInt n
eval env (Add e1 e2) = evalBinOp env (\x y -> Right $ x + y) e1 e2
eval env (Sub e1 e2) = evalBinOp env (\x y -> Right $ x - y) e1 e2
eval env (Mul e1 e2) = evalBinOp env (\x y -> Right $ x * y) e1 e2
eval env (Div e1 e2) = evalBinOp env f e1 e2
  where
    f _ 0 = Left "Zero division"
    f x y = Right $ div x y
eval env (Pow e1 e2) = evalBinOp env f e1 e2
  where
    f x y =
      if y < 0
        then Left "Negative exponent"
        else Right $ x ^ y
eval _ (CstBool b) = Right $ ValBool b
eval env (Eql e1 e2) = case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Invalid operands to equality"
eval env (If e1 e2 e3) = case eval env e1 of
  Left err -> Left err
  Right (ValBool True) -> eval env e2
  Right (ValBool False) -> eval env e3
  _ -> Left "Conditional is non bool"
eval env (Var vn) = case envLookup vn env of
  Nothing -> Left $ "Non bound variable" ++ vn
  Just v -> Right v
eval env (Let vn e1 e2) = case eval env e1 of
  Left err -> Left err
  Right v -> eval (envExtend vn v env) e2


-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend vn val env = (vn, val) : env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup vn env = lookup vn env
