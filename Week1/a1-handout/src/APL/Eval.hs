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
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

evalIntBinOp :: (Integer -> Integer -> Either Error Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp f env e1 e2 =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> case f x y of
      Left err -> Left err
      Right z -> Right $ ValInt z
    (Right _, Right _) -> Left "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp' f env e1 e2 =
  evalIntBinOp f' env e1 e2
  where
    f' x y = Right $ f x y

eval :: Env -> Exp -> Either Error Val
eval _env (CstInt x) = Right $ ValInt x
eval _env (CstBool b) = Right $ ValBool b
eval env (Var v) = case envLookup v env of
  Just x -> Right x
  Nothing -> Left $ "Unknown variable: " ++ v
eval env (Add e1 e2) = evalIntBinOp' (+) env e1 e2
eval env (Sub e1 e2) = evalIntBinOp' (-) env e1 e2
eval env (Mul e1 e2) = evalIntBinOp' (*) env e1 e2
eval env (Div e1 e2) = evalIntBinOp checkedDiv env e1 e2
  where
    checkedDiv _ 0 = Left "Division by zero"
    checkedDiv x y = Right $ x `div` y
eval env (Pow e1 e2) = evalIntBinOp checkedPow env e1 e2
  where
    checkedPow x y =
      if y < 0
        then Left "Negative exponent"
        else Right $ x ^ y
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Invalid operands to equality"
eval env (If cond e1 e2) =
  case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "Non-boolean conditional."
eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2
-- TODO: Add cases after extending Exp.
eval env (ForLoop (p, inital) (i, bound) body) =
  case eval env inital of
    Left err -> Left err
    Right v ->
      case eval env bound of
        Left err -> Left err
        Right (ValInt n) ->
          loop env1
          where
            env1 = envExtend i (ValInt 0) $ envExtend p v env
            loop envl = case envLookup i envl of
              Nothing -> Left "Loop index unbound"
              Just (ValInt iv) ->
                if iv < n
                then case eval envl body of
                       Left err -> Left err
                       Right val ->
                         loop $ envExtend p val $ envExtend i (ValInt $ iv + 1) envl
                else case envLookup p envl of
                       Just val -> Right val
                       _ -> Left "For loop result not bound"
              Just _ -> Left "Loop index non integer"
        Right _ -> Left "Non-integral loop bound"
eval env (Lambda vn body) = Right $ ValFun env vn body
eval env (Apply fn xe) = case eval env fn of
  Left err -> Left err
  Right (ValFun envf xn body) ->
    case eval env xe of
      Left err -> Left err
      Right x -> eval (envExtend xn x envf) body
  Right _ -> Left "Apply non function"
eval env (TryCatch e1 e2) =
  case eval env e1 of
    Right v -> Right v
    Left _ -> eval env e2
