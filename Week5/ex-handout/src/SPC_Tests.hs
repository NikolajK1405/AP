module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [ testCase "adding job" $ do
          spc <- startSPC
          _ <- jobAdd spc $ Job (pure ()) 1
          pure (),
{-
        testCase "job status" $ do
          spc <- startSPC
          id <- jobAdd spc $ Job (pure ()) 1
          r <- jobStatus spc id
          r @?= Just JobPending,

        testCase "job cancel" $ do
          spc <- startSPC
          id <- jobAdd spc $ Job (pure ()) 1
          jobCancel spc id
          r <- jobStatus spc id
          r @?= Just (JobDone DoneCancelled),
-}
        testCase "job execution" $ do
          spc <- startSPC
          ref <- newIORef True
          id <- jobAdd spc $ Job (writeIORef ref False) 1
          reason <- jobWait spc id
          r <- readIORef ref
          r @?= False
          reason @?= Just Done,

        testCase "job crash" $ do
          spc <- startSPC
          id <- jobAdd spc $ Job (print (div 1 0)) 1
          reason <- jobWait spc id
          reason @?= Just DoneCrashed,

        testCase "timeout" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneTimeout
      ]
