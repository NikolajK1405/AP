module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import Data.Either (isRight, isLeft)
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)


tests :: TestTree
tests =
  localOption (mkTimeout 8000000) $
    testGroup
      "SPC (core)"
      [ testCase "duplicate worker name" $ do
          spc <- startSPC
          w1 <- workerAdd spc "w1"
          assertBool "expected Right Worker" $ isRight w1
          w2 <- workerAdd spc "w1"
          assertBool "expected Left string" $ isLeft w2,

        testCase "simple job" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 1000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobPending
          _ <- workerAdd spc "w1"
          r2 <- jobStatus spc j
          r2 @?= Just JobRunning
          r3 <- jobWait spc j
          r3 @?= Just Done
          v <- readIORef ref
          v @?= True,

        testCase "simple job runs to completion (Done)" $ do
          spc <- startSPC
          _ <- workerAdd spc "w1"
          ref <- newIORef False
          jid <- jobAdd spc $ Job (writeIORef ref True) 1
          reason <- jobWait spc jid
          val <- readIORef ref
          assertBool "job should have executed" val
          reason @?= Just Done,

        testCase "job that crashes results in DoneCrashed" $ do
          spc <- startSPC
          _ <- workerAdd spc "w1"
          jid <- jobAdd spc $ Job (print (div 1 0)) 1
          reason <- jobWait spc jid
          reason @?= Just DoneCrashed,

        testCase "job cancel while running -> DoneCancelled" $ do
          spc <- startSPC
          _ <- workerAdd spc "w1"
          jid <- jobAdd spc $ Job (threadDelay 2000000) 3
          threadDelay 200000 
          jobCancel spc jid
          reason <- jobWait spc jid
          reason @?= Just DoneCancelled,

        testCase "multiple jobs execute sequentially with single worker" $ do
          spc <- startSPC
          _ <- workerAdd spc "w1"
          ref <- newIORef (0 :: Int)
          let mkJob = Job (modifyIORef ref (+1)) 1
          jids <- forM [1..3] $ \_ -> jobAdd spc mkJob
          _ <- forM_ jids (jobWait spc)
          val <- readIORef ref
          val @?= 3,
        
        testCase "multiple jobs: one cancelled, rest complete" $ do
            spc <- startSPC
            ref <- newIORef (0 :: Int)
            let mkJob = Job (threadDelay 200000 >> modifyIORef ref (+1)) 5
            j1 <- jobAdd spc mkJob
            j2 <- jobAdd spc mkJob
            j3 <- jobAdd spc mkJob
            jobCancel spc j2
            _ <- workerAdd spc "w1"
            r1 <- jobWait spc j1
            r2 <- jobWait spc j2
            r3 <- jobWait spc j3
            val <- readIORef ref
            r1 @?= Just Done
            r2 @?= Just DoneCancelled
            r3 @?= Just Done
            val @?= 2,

        testCase "multiple workers handle multiple jobs" $ do
          spc <- startSPC
          _ <- workerAdd spc "w1"
          _ <- workerAdd spc "w2"
          ref <- newIORef (0 :: Int)
          let mkJob = Job (modifyIORef ref (+1) >> threadDelay 200000) 2
          jids <- replicateM 4 (jobAdd spc mkJob)
          _ <- forM_ jids (jobWait spc)
          val <- readIORef ref
          assertBool "at least 4 jobs must complete" (val >= 4),

        testCase "job times out when it exceeds maxSeconds" $ do
          spc <- startSPC
          r <- workerAdd spc "w1"
          assertBool "workerAdd failed" (isRight r)
          jid <- jobAdd spc $ Job (threadDelay 2000000) 1
          res <- jobWait spc jid
          res @?= Just DoneTimeout,


        testCase "job that finishes before maxSeconds does NOT timeout" $ do
          spc <- startSPC
          r <- workerAdd spc "w1"
          assertBool "workerAdd failed" (isRight r)
          jid <- jobAdd spc $ Job (threadDelay 200000) 1
          res <- jobWait spc jid
          res @?= Just Done,

        testCase "status shows running before timeout fires" $ do
          spc <- startSPC
          r <- workerAdd spc "w1"
          assertBool "workerAdd failed" (isRight r)
          jid <- jobAdd spc $ Job (threadDelay 3000000) 1
          threadDelay 200000
          st1 <- jobStatus spc jid
          st1 @?= Just JobRunning
          res <- jobWait spc jid
          res @?= Just DoneTimeout
      ]
