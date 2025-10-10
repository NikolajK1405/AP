{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobCancel,
    jobWait,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  deriving (Eq, Ord, Show)

-- Then the definition of the glorious SPC.

-- | Messages sent to SPC.
data SPCMsg = MsgJobAdd Job (ReplyChan JobId)
            | MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
            | MsgJobCancel JobId
            | MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
            | MsgJobDone JobId
            | MsgJobCrashed JobId
            | MsgTick

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobCounter :: JobId,
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcJobRunning :: Maybe (JobId, Seconds, ThreadId),
    spcChan :: Chan SPCMsg
  }


newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  (<*>) = ap
  pure x = SPCM $ \s -> pure (x, s)

instance Monad SPCM where
  SPCM g >>= f = SPCM $ \s -> do
    (x, s') <- g s
    let SPCM x' = f x
    x' s'

get :: SPCM SPCState
get = SPCM $ \s -> pure (s, s)

put :: SPCState -> SPCM ()
put s = SPCM $ \_ -> pure ((), s)

io :: IO a -> SPCM a
io m = SPCM $ \s -> do
  x <- m
  pure (x, s)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM s (SPCM g) = fmap fst $ g s

schedule :: SPCM ()
schedule = do
  s <- get
  case (spcJobRunning s, spcJobsPending s) of
    (Nothing, (id, job) : pending) -> do
      t <- io $ forkIO $ do
        let action = do
              jobAction job
              send (spcChan s) $ MsgJobDone id
            onException :: SomeException -> IO ()
            onException e = do
              send (spcChan s) $ MsgJobCrashed id
        catch action onException
      now <- io $ getSeconds
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put $ s { spcJobRunning = Just (id, deadline, t),
                spcJobsPending = pending
              }
    _ -> pure ()

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job from -> do
      s <- get
      let JobId jobId = spcJobCounter s
      let jobs = spcJobsPending s
      put $ s { spcJobsPending = (JobId jobId, job) : jobs,
                spcJobCounter = JobId $ succ jobId
              }
      io $ reply from $ JobId jobId

    MsgJobStatus id from -> do
      s <- get
      io $ reply from $ case ( lookup id $ spcJobsPending s,
                               spcJobRunning s,
                               lookup id $ spcJobsDone s
                             ) of
        (Just _, _, _) -> Just JobPending
        (_, Just (running_job, _, _), _)
          | running_job == id ->
              Just $ JobRunning
        (_, _, Just r) -> Just $ JobDone r

    MsgJobCancel id -> do
      s <- get
      case lookup id $ spcJobsPending s of
        Nothing -> pure ()
        Just _ -> jobDone id DoneCancelled

    MsgJobWait id from -> do
      s <- get
      case lookup id $ spcJobsDone s of
        Just reason ->
          io $ reply from $ Just reason
        Nothing ->
          put $ s { spcWaiting = (id, from) : spcWaiting s }

    MsgJobDone id -> jobDone id Done

    MsgJobCrashed id -> jobDone id DoneCrashed

    MsgTick -> pure ()

startSPC :: IO SPC
startSPC = do
  let initialState c =
        SPCState
          { spcJobsPending = [],
            spcJobCounter = JobId 0,
            spcJobsDone = [],
            spcWaiting = [],
            spcChan = c,
            spcJobRunning = Nothing
          }
  server <- spawn $ \c -> runSPCM (initialState c) $ forever $ handleMsg c
  void $ spawn $ tickServer server
  pure $ SPC $ server
  where
    tickServer server c = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo server MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job = requestReply c $ MsgJobAdd job

-- | Query the job status.
jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) id = requestReply c $ MsgJobStatus id

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) id = sendTo c $ MsgJobCancel id

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone id reason = do
  s <- get
  case lookup id $ spcJobsDone s of
    Just _ -> pure () -- Already done
    Nothing -> do
      let (waiting, rest) = partition ((== id) . fst) $ spcWaiting s
      forM_ waiting $ \(_, rsvp) -> io $ reply rsvp $ Just reason
      put $ s { spcJobsPending = removeAssoc id $ spcJobsPending s,
                spcJobsDone = (id, reason) : spcJobsDone s,
                spcWaiting = rest,
                spcJobRunning = Nothing
              }

checkTimeouts :: SPCM ()
checkTimeouts = do
  s <- get
  now <- io $ getSeconds
  case spcJobRunning s of
    Just (id, deadline, t)
      | now >= deadline-> do
          io $ killThread t
          jobDone id DoneTimeout
    _ -> pure ()
