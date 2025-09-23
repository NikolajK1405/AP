{-# LANGUAGE InstanceSigs #-}
module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String
type Env = [VName]

envEmpty :: Env
envEmpty = []

newtype CheckM a = CheckM (Env -> Either Error a)

instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  pure x = CheckM $ \_env -> Right x
  (<*>) = ap

instance Monad CheckM where
  -- (>>=) :: CheckM a -> (a -> CheckM b) -> CheckM b
  CheckM m >>= f = CheckM $ \env ->
    case m env of
      Left err -> Left err
      Right x ->
        let CheckM f' = f x in
          f' env

askEnv :: CheckM Env
askEnv = CheckM $ \env -> Right env

localEnv :: (Env -> Env) -> CheckM a -> CheckM a
localEnv f (CheckM m) = CheckM $ \env -> m (f env)

failure :: String -> CheckM a
failure s = CheckM $ \_env -> Left s

runCheck :: CheckM a -> Either Error a
runCheck (CheckM m) = m envEmpty


check :: Exp -> CheckM ()
check (CstInt _)  = pure ()
check (CstBool _) = pure ()
check (Var v) = do
  env <- askEnv
  if elem v env
    then pure ()
    else failure ("Variable not in scope: " ++ v)
check (Add e1 e2) = check e1 >> check e2
check (Sub e1 e2) = check e1 >> check e2
check (Mul e1 e2) = check e1 >> check e2
check (Div e1 e2) = check e1 >> check e2
check (Pow e1 e2) = check e1 >> check e2
check (Eql e1 e2) = check e1 >> check e2
check (If cond e1 e2) = check cond >> check e1 >> check e2
check (Let var e1 e2) = do
  check e1
  localEnv ((:) var) (check e2)
check (ForLoop (p, initial) (i, bound) body) = do
  check initial
  check bound
  localEnv ((:) p . (:) i) (check body)
check (Lambda var body) =
  localEnv ((:) var) (check body)
check (Apply e1 e2) = check e1 >> check e2
check (TryCatch e1 e2) = check e1 >> check e2
check (Print _ e) = check e
check (KvPut e1 e2) = check e1 >> check e2
check (KvGet e) = check e

checkExp :: Exp -> Maybe Error
checkExp e = case runCheck (check e) of
    Left err -> Just err
    Right () -> Nothing
