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
  localOption (mkTimeout 3000000) $
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

        testCase "job is pending before worker exists" $ do
          spc <- startSPC
          jid <- jobAdd spc $ Job (pure ()) 1
          s <- jobStatus spc jid
          s @?= Just JobPending,

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

        testCase "multiple workers handle multiple jobs" $ do
          spc <- startSPC
          _ <- workerAdd spc "w1"
          _ <- workerAdd spc "w2"
          ref <- newIORef (0 :: Int)
          let mkJob = Job (modifyIORef ref (+1) >> threadDelay 200000) 2
          jids <- replicateM 4 (jobAdd spc mkJob)
          _ <- forM_ jids (jobWait spc)
          val <- readIORef ref
          assertBool "at least 4 jobs must complete" (val >= 4)


      ]
