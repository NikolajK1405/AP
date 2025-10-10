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

        -- Possible racey test, so we added some thread delay
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
          v @?= True


      ]
