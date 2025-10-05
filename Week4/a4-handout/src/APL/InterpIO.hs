module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  res <- runEvalIO' envEmpty dbFile evalm
  case res of
    Right v -> pure $ Right v
    Left (ErrFail err) -> pure $ Left err
    Left (Broke _) -> pure $ Left "Break outside loop"
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either ErrBreak a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left $ ErrFail e
    runEvalIO' r db (Free (TryCatchOp m1 m2 k)) = do
      res1 <- runEvalIO' r db m1
      case res1 of
        Right v -> runEvalIO' r db (k v)
        Left (Broke v) -> pure $ Left $ Broke v
        Left _ -> do
          res2 <- runEvalIO' r db m2
          case res2 of
            Right v -> runEvalIO' r db (k v)
            Left err -> pure $ Left err
    runEvalIO' r db (Free (KvGetOp key k)) = do
      dbs <- readDB db
      case dbs of
        Left err -> pure $ Left $ ErrFail err
        Right s ->
          case lookup key s of
            Nothing -> do
              str <- prompt $ "Invalid key: " ++ show key ++ ". Enter a replacement: "
              case readVal str of
                Nothing -> pure $ Left $ ErrFail $ "Invalid value input: " ++ str
                Just v -> runEvalIO' r db (k v)
            Just v -> runEvalIO' r db (k v)
    runEvalIO' r db (Free (KvPutOp key val m)) = do
      dbs <- readDB db
      case dbs of
        Left err -> pure $ Left $ ErrFail err
        Right s ->
          let s' = (key, val) : s
          in do
            writeDB db s'
            runEvalIO' r db m
    runEvalIO' r db (Free (TransactionOp m k)) =
      withTempDB f
      where
        f tdb = do
          copyDB db tdb
          res <- runEvalIO' r tdb m
          case res of
            Left err -> pure $ Left err
            Right v -> do
              copyDB tdb db
              runEvalIO' r db $ k v
    runEvalIO' r db (Free (LoopingOp m k)) = do
      res <- runEvalIO' r db m
      case res of
        Left (Broke v) -> runEvalIO' r db $ k v
        Left err -> pure $ Left err
        Right v -> runEvalIO' r db $ k v
    runEvalIO' _ _ (Free (BreakOp v)) = pure $ Left (Broke v)
