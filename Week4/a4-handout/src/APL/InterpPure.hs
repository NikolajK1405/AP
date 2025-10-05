module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval m0 = let (p, _, res) = runEval' envEmpty stateInitial m0 in
  case res of
    Right v -> (p, Right v)
    Left (ErrFail err) -> (p, Left err)
    Left (Broke _) -> (p, Left "Break outside loop")
  where
    runEval' :: Env -> State -> EvalM a -> ([String], State, Either ErrBreak a)
    runEval' _ s (Pure x) = ([], s, pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, s', res) = runEval' r s m
       in (p : ps, s', res)
    runEval' _ s (Free (ErrorOp e)) = ([], s, Left (ErrFail e))
    runEval' r s (Free (TryCatchOp m1 m2 k)) =
      case runEval' r s m1 of
        (p1, s', Right v) ->
          let (p2, s'', res) = runEval' r s' (k v) in
            (p1 ++ p2, s'', res)
        (p, s', Left (Broke v)) -> (p, s', Left (Broke v))
        (_, _, Left _) ->
          case runEval' r s m2 of
            (p1, s', Right v) ->
              let (p2, s'', res) = runEval' r s' (k v) in
                (p1 ++ p2, s'', res)
            (p, s', Left err) -> (p, s', Left err)
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> ([], s, Left (ErrFail $ "Unknown key: " ++ show key))
        Just v -> runEval' r s (k v)
    runEval' r s (Free (KvPutOp key val m)) =
      runEval' r ((key, val) : s) m
    runEval' r s (Free (TransactionOp m k)) =
      case runEval' r s m of
        (p, _, Left err) -> (p, s, Left err)
        (p, s', Right v) ->
          let (p', s'', res) = runEval' r s' $ k v in
            (p ++ p', s'', res)
    runEval' r s (Free (LoopingOp m k)) =
      case  runEval' r s m of
        (p, s', Left (Broke v)) ->
          let (p', s'', res') = runEval' r s' $ k v in
            (p ++ p', s'', res')
        (p, s', Left err) -> (p, s', Left err)
        (p, s', Right v) ->
          let (p', s'', res') = runEval' r s' $ k v in
            (p ++ p', s'', res')
    runEval' _ s (Free (BreakOp v)) = ([], s, Left (Broke v))
