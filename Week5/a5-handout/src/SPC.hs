module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    workerAdd,
    workerStop,
  )
where

import Control.Concurrent
  ( forkIO,
    killThread,
    threadDelay,
    ThreadId,
    newChan
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forever, liftM, void, forM_)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)
import Data.List (partition, find)
import GenServer (requestReply)

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

-- | Triple Remove mapping from association list.
tripletRemoveAssoc :: (Eq k) => k -> [(k, v, z)] -> [(k, v, z)]
tripletRemoveAssoc needle ((k, v, z) : kvzs) =
  if k == needle
    then kvzs
    else (k, v, z) : tripletRemoveAssoc needle kvzs
tripletRemoveAssoc _ [] = []

tripleLookup :: Eq a => a -> [(a,b,c)] -> Maybe (b,c)
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
tripleLookup a bc = do 
  (_,b,c) <- find (\(a',_,_) -> a' == a) bc
  pure (b,c)

-- Then the definition of the glorious SPC.

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
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
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

  | JobCancelled
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- | Messages sent to workers. These are sent both by SPC and by
-- processes spawned by the workes.
data WorkerMsg
  = WorkerStart JobId (IO ()) 
  | WorkerCancelled (ThreadId)

-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
  | -- | Some time has passed.
    MsgTick
  | -- | Query whether the worker name is available
    MsgAddWorker WorkerName (ReplyChan (Either String Worker))
  | -- | Sent upon completion of job from worker
    MsgJobDone WorkerName (JobId)
  
  | MsgJobCrashed JobId

  | MsgJobForced WorkerName JobDoneReason 

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, Job, ThreadId)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcJobTimer :: [(JobId, Seconds)],
    -- Workers:
    spcWorkers :: [(WorkerName, Worker)],
    spcBusy :: [(WorkerName, Worker, JobId)],
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcChan :: Chan SPCMsg
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  case (spcJobsPending state, spcWorkers state) of
    ((jid, job) : jRst, (wName, worker) : wRst) -> do
      start <- io getSeconds                              
      let deadline = start + fromIntegral (jobMaxSeconds job)
      t <- io $ forkIO $ do
        let action = do
              jobAction job
              send (spcChan state) $ MsgJobDone wName jid
            onException :: SomeException -> IO ()
            onException _ = send (spcChan state) $ MsgJobCrashed jid
        catch action onException
      put $ state {   spcJobsPending = jRst,
                      spcJobsRunning = (jid, job, t) : spcJobsRunning state,
                      spcWorkers = spcWorkers state,
                      spcBusy = (wName, worker, jid) : spcBusy state,
                      spcJobTimer = (jid, deadline) : spcJobTimer state
                    }
      schedule
    _ -> pure ()


jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jid reason = do
  state <- get
  case lookup jid $ spcJobsDone state of
    Just _ ->
      -- We already know this job is done.
      pure ()
    Nothing -> do
      let (waiting, notWaiting) = partition ((== jid) . fst) (spcWaiting state)
      forM_ waiting $ \(_, rsvp) -> io $ reply rsvp $ Just reason
      put $ state { spcWaiting = notWaiting,
                    spcJobsDone = (jid, reason) : spcJobsDone state,
                    spcJobsRunning = tripletRemoveAssoc jid (spcJobsRunning state),
                    spcJobTimer = removeAssoc jid (spcJobTimer state)
                  }


workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle = undefined

workerIsGone :: WorkerName -> SPCM ()
workerIsGone = undefined

checkTimeouts :: SPCM ()
-- s <- get
--   now <- io $ getSeconds
--   case spcJobRunning s of
--     Just (id, deadline, t)
--       | now >= deadline-> do
--           io $ killThread t
--           jobDone id DoneTimeout
--     _ -> pure ()
checkTimeouts = do 
  s <- get 
  now <- io $ getSeconds
  let dead = [ (jid, dl) | (jid, dl) <- spcJobTimer s, now >= dl ]
      live = [ (jid, dl) | (jid, dl) <- spcJobTimer s, now < dl ]
  forM_ dead $ \(jid, _) -> do
    s1 <- get
    case find (\(j,_,_) -> j == jid) (spcJobsRunning s1) of
      Just (_,_,tid) -> io $ killThread tid
      Nothing ->  pure ()
    case find (\(_,_,j) -> j==jid) (spcBusy s1) of
      Just (wName,_,_) ->  put s1 { spcBusy = tripletRemoveAssoc wName (spcBusy s1) }
      Nothing -> pure ()
    jobDone jid DoneTimeout
  modify (\st -> st { spcJobTimer = live })

workerExists :: WorkerName -> SPCM Bool
workerExists = undefined

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $ state { spcJobsPending = (spcJobCounter state, job) : spcJobsPending state,
                    spcJobCounter = JobId $ succ jobid
                  }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               tripleLookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> Just JobPending
        (_, Just _, _) -> Just JobRunning
        (_, _, Just r) -> Just $ JobDone r
        _ -> Nothing
    MsgAddWorker name rsvp -> do
      state <- get
      case lookup name $ spcWorkers state of
        Just _ -> io $ reply rsvp $ Left "Worker name already in use"
        Nothing -> do
          w <- io $ startWorker c name
          put $ state { spcWorkers = (name, w) : spcWorkers state
                      }
          io $ reply rsvp $ Right w
    MsgJobDone name jid -> do
      state <- get
      put $ state { spcBusy = tripletRemoveAssoc name (spcBusy state)
                  }
      jobDone jid Done
    MsgJobWait jid rsvp -> do
      state <- get
      case lookup jid $ spcJobsDone state of
        Just reason -> do
          io $ reply rsvp $ Just reason
        Nothing ->
          put $ state {spcWaiting = (jid, rsvp) : spcWaiting state}
    MsgJobForced wName reason -> do
      s <- get 
      case find (\(w,_,_) -> w == wName) (spcBusy s) of
        Just (_w, _worker, jid) -> do
          put s { spcBusy = tripletRemoveAssoc wName (spcBusy s) }
          jobDone jid reason
        Nothing -> pure ()

    MsgJobCancel jib -> do 
      s <- get 
      case lookup jib (spcJobsPending s) of
        Just _ -> do
          put s { spcJobsPending = removeAssoc jib (spcJobsPending s)
                , spcJobTimer    = removeAssoc jib (spcJobTimer s)
                }
          jobDone jib DoneCancelled 
        Nothing -> do
            s' <- get
            case find (\(_,_,j) -> j == jib) $ spcBusy s' of 
              Nothing -> pure ()
              Just (wName, worker, _jib) -> 
                case find (\(j,_,_) -> j == jib) (spcJobsRunning s') of
                  Just (_, _, tid) -> do
                    let Worker wSrv = worker
                    io $ sendTo wSrv (WorkerCancelled tid)
                  Nothing -> pure ()

    MsgJobCrashed jid -> do
      s <- get
      case find (\(_,_,j) -> j == jid) (spcBusy s) of
        Just (wName,_,_) ->
          put s { spcBusy = tripletRemoveAssoc wName (spcBusy s) }
        Nothing -> pure ()
      jobDone jid DoneCrashed

    MsgTick -> checkTimeouts


startSPC :: IO SPC
startSPC = do
  let initial_state c =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWorkers = [],
            spcBusy = [],
            spcWaiting = [],
            spcChan = c,
            spcJobTimer = []
          }
  c <- spawn $ \c -> runSPCM (initial_state c) $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 1000000 -- 1,5 seconds
      sendTo c MsgTick

startWorker :: Chan SPCMsg -> WorkerName -> IO Worker
startWorker spcC name = do
  server <- spawn $ \workerC ->
    let loop = do
              msg <- receive workerC
              case msg of
                (WorkerStart jid job) -> do
                  job
                  send spcC  $ MsgJobDone name jid
                  loop
                (WorkerCancelled tid) -> do 
                  killThread tid
                  send spcC $ MsgJobForced name DoneCancelled
                  loop
    in loop 
  pure (Worker server)



-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Asynchronously query the job status. Returns 'Nothing' if job is
-- not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) name = requestReply c $ MsgAddWorker name

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop = undefined