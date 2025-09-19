{-# LANGUAGE InstanceSigs #-}
module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

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

type Key = [(Val, Val)]
type State = ([String], Key)

stateEmpty :: State
stateEmpty = ([],[])

newtype EvalM a = EvalM (Env -> State -> (State, Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env state -> (state, Right x)
  (<*>) = ap

instance Monad EvalM where
  -- (>>=) :: EvalM a -> (a -> EvalM b) -> EvalM b
  EvalM m >>= f = EvalM $ \env state ->
    case m env state of
      (state', Left err) -> (state', Left err)
      (state', Right x) ->
        let EvalM f' = f x in
          f' env state'

askEnv :: EvalM Env
askEnv = EvalM $ \env state -> (state, Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env state -> m (f env) state

getState :: EvalM State
getState = EvalM $ \_env state -> (state, Right state)

putState :: State -> EvalM ()
putState state = EvalM $ \_env _ -> (state, Right ())

failure :: String -> EvalM a
failure s = EvalM $ \_env state -> (state, Left s)

-- Design choice, we dont foward the updated state from m1 in the case it fails.
catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env state ->
  case m1 env state of
    (_, Left _) -> m2 env state
    (state', Right x) -> (state', Right x)

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) =
  let ((ss,_), res) = m envEmpty stateEmpty in
    (ss, res)

evalPrint :: String -> EvalM ()
evalPrint s = do
  (ss, ks) <- getState
  putState (ss ++ [s], ks)
  pure ()

evalKvGet :: Val -> EvalM Val
evalKvGet k = do
  (_, ks) <- getState
  EvalM $ \_env state ->
    case lookup k ks of
      Nothing -> (state, Left $ "Invalid key: " ++ show k)
      Just v -> (state, Right v)

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut k v = do
  (ss, ks) <- getState
  putState (ss, (k, v) : ks)
  pure ()

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (ForLoop (loopparam, initial) (iv, bound) body) = do
  initial_v <- eval initial
  bound_v <- eval bound
  case bound_v of
    ValInt bound_int ->
      loop 0 bound_int initial_v
    _ ->
      failure "Non-integral loop bound"
  where
    loop i bound_int loop_v
      | i >= bound_int = pure loop_v
      | otherwise = do
          loop_v' <-
            localEnv (envExtend iv (ValInt i) . envExtend loopparam loop_v) $
              eval body
          loop (succ i) bound_int loop_v'
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print s e) = do
  v <- eval e
  let s' = s ++ ": " ++ (valToStr v) in do
    evalPrint s'
    pure v
  where
    valToStr (ValInt x) = show x
    valToStr (ValBool b) = show b
    valToStr (ValFun _ _ _) = "#<fun>"
eval (KvPut ek ev) = do
  k <- eval ek
  v <- eval ev
  evalKvPut k v
  pure v
eval (KvGet ke) = do
  k <- eval ke
  evalKvGet k
