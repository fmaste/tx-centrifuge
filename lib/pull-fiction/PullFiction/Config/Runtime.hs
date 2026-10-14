{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------

-- | Resolves a 'Validated.Config' into a 'Runtime': live STM resources (queues,
-- rate limiters) grouped into name-keyed pools, with the threads running them.
-- See 'Runtime' for the pools and 'resolve' for resolution.
module PullFiction.Config.Runtime
  ( -- * Runtime.
    Runtime
  , config, builders, pipes, recyclers, observers, forwarders, workloads, asyncs
    -- * Initial inputs.
  , InputFold
    -- * Handles.
    -- ** Behaviour handles (what the resource does).
  , BuilderApi     (..)
  , BuilderHandle  (..)
  , ObserverHandle (..)
    -- ** Event handlers (fired here, used for tracing).
  , PipeHandle     (..)
  , RecyclerHandle (..)
    -- * Builder.
  , Builder
  , builderName, builderPipe, builderRecycler, builderAsync
    -- * Recycler.
  , Recycler
  , recyclerName, recyclerInternal, recyclerAsync
    -- * Observer.
  , Observer
  , observerName, observerHandle, observerAsync
    -- * Forwarder.
  , Forwarder
  , forwarderName, forwarderObserver, forwarderRecycler, forwarderAsync
    -- * Workload.
  , Workload
  , workloadName, targets
    -- * OnExhaustion.
  , Raw.OnExhaustion (..)
    -- * Target.
  , Target
  , targetName
  , targetFetcher
  , rateLimiter
  , maxBatchSize, onExhaustion
  , targetAddr, targetPort
    -- * Resolution.
  , resolve
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Concurrent (myThreadId)
import Control.Monad (forever, when)
import Data.Foldable (foldlM)
import GHC.Conc (labelThread)
import Numeric.Natural (Natural)
-----------
-- async --
-----------
import Control.Concurrent.Async qualified as Async
----------------
-- containers --
----------------
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
------------------
-- pull-fiction --
------------------
import PullFiction.Config.Raw qualified as Raw
import PullFiction.Config.Validated qualified as Validated
import PullFiction.Internal.Pipe qualified as Pipe
import PullFiction.Internal.RateLimiter qualified as RL
import PullFiction.Internal.Recycler qualified as Recycler

--------------------------------------------------------------------------------

-- | Fully resolved top-level configuration.
--
-- Everything is a name-keyed pool. Today the name is the workload name for
-- 'builders', 'pipes', 'recyclers' and 'workloads' (one of each per workload),
-- the config key for 'observers', and the wiring path for 'forwarders' (see
-- 'Forwarder'), but the pools are the natural home once the config allows
-- sharing or interconnection.
--
-- Each pool entry also carries its own name as a field (repeating its 'Map'
-- key, for labels and traces) and the async(s) running it, so 'asyncs' just
-- gathers all of them. The exception is 'pipes': a pipe is named by its 'Map'
-- key like every entry, but the entry itself is pure structure, a queue pair
-- ('Pipe.Pipe') carrying neither a name field nor an async.
data Runtime key input payload = Runtime
  { -- | The original validated configuration.
    config     :: !Validated.Config
    -- | Resolved builders (build loops), keyed by name.
  , builders   :: !(Map String Builder)
    -- | Resolved pipes (queue pairs), keyed by name. Pure structure: the only
    -- pool whose entries do not repeat their name as a field (it lives only
    -- in the 'Map' key) and run no async, just the queue pair.
    -- One entry per workload always, but under @\"scope\": \"shared\"@ (the
    -- default) every entry is built over one input queue, so the entries are
    -- distinct pipes sharing their input side (see 'Raw.InputsScope' and
    -- 'resolvePipes').
  , pipes      :: !(Map String (Pipe.Pipe key input payload))
    -- | Resolved recyclers, keyed by name.
  , recyclers  :: !(Map String (Recycler key input payload))
    -- | Resolved observers, keyed by name.
  , observers  :: !(Map String (Observer key))
    -- | Resolved forwarders (observer to recycler bridges), keyed by wiring
    -- path (see 'Forwarder').
  , forwarders :: !(Map String Forwarder)
    -- | Resolved workloads (target groups), keyed by name.
  , workloads  :: !(Map String (Workload key input payload))
    -- | All asyncs (builders + recyclers + forwarders + observers), linked.
    -- Caller should append their own worker asyncs for cleanup.
  , asyncs     :: ![Async.Async ()]
  }

--------------------------------------------------------------------------------

-- | Where the initial inputs come from: a fold over them, which 'resolve' runs
-- once over the queues it has just created. It is a caller-supplied action
-- like the factories 'resolve' takes, which is why it lives here and not in
-- 'Validated.Config', where everything is static data.
--
-- A fold rather than a list so that nothing holds every input at once: a file
-- of millions of them costs what the queues cost and no more.
--
-- The @forall@ is inside, so the accumulator is the consumer\'s to pick and
-- the supplier cannot pin it (rank 2, hence @RankNTypes@). 'resolvePipes'
-- counts positions with it; something else here could fold over the queues
-- themselves.
type InputFold input =
  forall acc. (acc -> input -> IO acc) -> acc -> IO acc

--------------------------------------------------------------------------------

-- | The privileged operations a builder loop may perform, each a closure over
-- this builder's pipe and recycler. The raw 'Pipe.Pipe' and 'Recycler' are not
-- exposed, so a builder drives its own loop with full freedom but cannot reach
-- or corrupt the machinery: it may only pull inputs, publish a payload
-- (recording the recycle bookkeeping), or abandon inputs.
--
-- The builder loop owns conservation of inputs: every batch returned by
-- 'baTakeInputs' should be handed to exactly one of 'baAddPayload' (as its
-- consumed set) or 'baDropInputs'. The engine cannot enforce this, so a loop
-- that leaks a batch merely shrinks the set of recyclable inputs.
data BuilderApi key input payload = BuilderApi
  { -- | Pull this many inputs off the input queue. Blocks until that many are
    -- available.
    baTakeInputs :: Natural -> IO [input]
    -- | Publish a payload. Records its consumed inputs and produced outputs
    -- with the recycler (so the outputs return to the input queue when the
    -- recycle strategy fires), then enqueues the payload. Arguments:
    -- confirmation key, payload, consumed inputs, recyclable outputs.
  , baAddPayload :: key -> payload -> [input] -> [input] -> IO ()
    -- | Abandon inputs already pulled by 'baTakeInputs': they are neither
    -- recycled nor re-enqueued. The correct terminus for an unbuildable (e.g.
    -- dust) batch.
  , baDropInputs :: [input] -> IO ()
  }

-- | Caller-provided builder. 'resolve' spawns one async per builder and runs
-- 'bhRunBuilder' in it, handing over a 'BuilderApi' wired to that builder's
-- pipe and recycler. The builder owns its loop (batching, grouping, coin
-- selection); the engine owns the thread and the machinery behind the API.
newtype BuilderHandle key input payload = BuilderHandle
  { bhRunBuilder :: BuilderApi key input payload -> IO ()
  }

-- | Caller-provided pipe queue-event handlers (e.g. for tracing). 'resolve'
-- unpacks these into the workload's 'Pipe', which fires them as items are added
-- to or removed from its two queues. Queue mechanics are pipe events, not
-- builder events, so they live here and not on 'BuilderHandle'.
data PipeHandle key input = PipeHandle
  { -- | Fired after inputs added to the input queue (initial load or recycle).
    phOnInputsEnqueued  :: !(Pipe.OnInputsEvent input)
    -- | Fired after inputs are removed from the input queue (builder take).
  , phOnInputsDequeued  :: !(Pipe.OnInputsEvent input)
    -- | Fired after a payload is added to the payload queue.
  , phOnPayloadEnqueued :: !(Pipe.OnPayloadEvent key)
    -- | Fired after payload removed from the payload queue (worker dequeue).
  , phOnPayloadDequeued :: !(Pipe.OnPayloadEvent key)
  }

-- | Caller-provided recycler event handlers (e.g. for tracing). 'resolve'
-- unpacks them into the workload's 'Recycler', which fires them as payloads
-- enter the backlog and as inputs are added to the pipe's input queue. Kept
-- separate from 'PipeHandle' because recycling is a 'Recycler' concern, not a
-- 'Pipe' one.
data RecyclerHandle key input payload = RecyclerHandle
  { -- | Fired by the recycler with the added entry (key, consumed inputs,
    -- produced outputs) and the resulting backlog size each time it adds a
    -- payload to the backlog.
    rhOnHold         :: !(Recycler.OnHoldEvent key input)
    -- | Fired by the recycler with the released payload's key, the set it
    -- added to the pipe and the resulting backlog size, each recycle. 'Right'
    -- is the produced outputs, 'Left' the consumed inputs, as on 'ohSubscribe'
    -- below. The key pairs with 'rhOnHold', so the wait is the time between
    -- the two, and the side says what settled the payload: 'Left' only ever
    -- comes from an @on_confirm@ orphan.
  , rhOnRelease      :: !(Recycler.OnReleaseEvent key input)
    -- | Fired with the dropped inputs, the dropped payloads, the fresh inputs
    -- and the resulting backlog size each time a reset drops the queued inputs
    -- and payloads and reseeds the input queue.
  , rhOnReset        :: !(Recycler.OnResetEvent key input payload)
    -- | Optional recovery action. The forwarder runs it on an orphan and feeds
    -- its result to 'Recycler.reset' (keyed under on_confirm, see
    -- 'resolveForwarders'), which drops the pipe's queued inputs and payloads
    -- and reseeds the input queue with it.
  , rhRecover        :: !(Maybe (IO [input]))
  }

-- | Caller-provided observer handle. 'resolve' spawns 'ohRun' in a labeled,
-- linked async and uses the subscription for 'Raw.RecycleOnConfirm' recycling.
data ObserverHandle key = ObserverHandle
  { -- | IO action that runs the observer (e.g. a NodeToNode connection).
    ohRun       :: !(IO ())
    -- | Subscribe to the observer's event stream: returns an STM action that
    -- reads the next event as its recycle key.
    -- 'Right' = confirmed (recycle output inputs).
    -- 'Left'  = orphaned  (recycle original inputs).
    --
    -- Each call must create an independent subscription (every subscriber sees
    -- every event). Observers are independent entities: one observer can serve
    -- many workloads, and what each does with the events (plain confirm
    -- recycling, rollback recovery) is decided per workload, never here.
  , ohSubscribe :: !(IO (STM.STM (Either key key)))
  }

--------------------------------------------------------------------------------

-- | A resolved build loop: it takes inputs from a pipe, builds payloads, and
-- signals a recycler. It references its 'pipes' and 'recyclers' entries /by
-- name/ (today both the workload name). Its loop thread is 'builderAsync', also
-- collected into 'asyncs'.
data Builder = Builder
  { -- | This builder's name (today the workload name).
    builderName     :: !String
    -- | Name of the 'pipes' entry it drives (takes from / adds to).
  , builderPipe     :: !String
    -- | Name of the 'recyclers' entry it signals via 'Recycler.hold'.
  , builderRecycler :: !String
    -- | Linked async running the build loop.
  , builderAsync    :: !(Async.Async ())
  }

-- | A resolved recycler: the 'Internal.Recycler' logic plus the async running
-- its worker. A thin wrapper so the 'recyclers' pool carries its own thread,
-- like 'Builder' and 'Observer'. The 'Recycler.Recycler' inside deliberately
-- holds no async. The observer bridge for 'RecycleOnConfirm' is a separate
-- thread ('resolveForwarders'), not held here.
data Recycler key input payload = Recycler
  { -- | This recycler's name (today the workload name).
    recyclerName     :: !String
    -- | The underlying recycler from "Internal.Recycler".
  , recyclerInternal :: !(Recycler.Recycler key input payload)
    -- | Optional recovery action (from 'rhRecover'): the forwarder runs it on
    -- an orphan and feeds its result to 'Recycler.reset'.
  , recyclerRecover  :: !(Maybe (IO [input]))
    -- | Linked async running its worker (writes this workload's recycled
    -- inputs back onto its pipe's input queue).
  , recyclerAsync    :: !(Async.Async ())
  }

-- | A resolved observer with its lifecycle managed by 'resolve': the
-- caller-provided 'ObserverHandle' plus the async running its 'ohRun'.
data Observer key = Observer
  { -- | Key from the config's @\"observers\"@ object.
    observerName   :: !String
    -- | The caller-provided handle ('ohRun' + 'ohSubscribe'). The forwarders
    -- subscribe to it for the workloads whose strategy names this observer.
  , observerHandle :: !(ObserverHandle key)
    -- | Linked async running the observer connection ('ohRun').
  , observerAsync  :: !(Async.Async ())
  }

-- | A resolved forwarder: the bridge thread that reads one observer
-- subscription and feeds each event through its workload's strategy wiring into
-- the recycler actions. Unlike the other pools it is keyed by the config
-- reference that wired it, @workload\/site\/observer@, with @site@ being
-- @recycle@ (the on_confirm strategy's observer) or @recovery@ (the recovery's
-- explicit observer), since one workload may subscribe to several observers.
-- Key uniqueness relies on names containing no @\'/\'@, enforced at validation
-- time like the rate-limit key scheme. It references what it bridges by name,
-- like 'Builder' does.
data Forwarder = Forwarder
  { -- | This forwarder's name (the @workload\/site\/observer@ wiring path, also
    -- the thread label).
    forwarderName     :: !String
    -- | Name of the 'observers' entry it subscribes to.
  , forwarderObserver :: !String
    -- | Name of the 'recyclers' entry it feeds.
  , forwarderRecycler :: !String
    -- | Linked async reading the subscription forever.
  , forwarderAsync    :: !(Async.Async ())
  }

-- | Fully resolved workload. Builder resources live in 'Builder' on the
-- 'Runtime', not here.
data Workload key input payload = Workload
  { -- | Unique name identifying this workload.
    workloadName :: !String
    -- | Resolved targets, keyed by name.
  , targets      :: !(Map String (Target key input payload))
  }

-- | A fully resolved target. Targets in the same workload share a 'Pipe'.
-- Targets with the same 'Validated.rateLimitKey' share a 'RL.RateLimiter'.
data Target key input payload = Target
  { -- | Unique name identifying this target.
    targetName    :: !String
    -- | Rate-limited, recycling payload fetch for this target, pre-built by
    -- 'resolveTarget' (wraps the shared pipe's fetcher through the workload's
    -- dequeue wiring).
    -- The worker pulls through this and never touches the pipe or the recycler.
  , targetFetcher :: !(Pipe.PayloadFetcher payload)
    -- | Shared when 'Validated.rateLimitKey' matches.
  , rateLimiter   :: !RL.RateLimiter
    -- | Resolved max tokens per request for this target.
  , maxBatchSize  :: !Natural
    -- | What to do when the payload queue is exhausted.
  , onExhaustion  :: !Raw.OnExhaustion
    -- | IP address or hostname of the target endpoint.
  , targetAddr    :: !String
    -- | Port number of the target endpoint.
  , targetPort    :: !Int
  }

--------------------------------------------------------------------------------
-- Resolution.
--------------------------------------------------------------------------------

-- | Limiter cache: maps a sharing key to an already-created rate limiter.
--
-- Threaded across workloads so that top-level Shared limiters are reused.
type LimiterCache = Map String RL.RateLimiter

-- | Resolve a 'Validated.Config' into a 'Runtime'. Everything is built into
-- name-keyed pools in dependency order: observers, then pipes, then recyclers
-- (each references its pipe), then builders and targets (each references a
-- pipe and recycler), then forwarders (each bridges an observer to a
-- recycler). See 'Runtime' for each pool's naming.
--
-- Initial inputs all go into one input queue, which every workload draws from.
-- Under @\"scope\": \"per_workload\"@ there is a queue per workload instead and
-- they are dealt round-robin over them, so they spread evenly without anyone
-- having to know how many there will be (see 'Raw.InputsScope').
resolve
  :: Ord key
  -- | Builder factory (index, name, config).
  => (Int -> String -> Raw.Builder  -> IO (BuilderHandle  key input payload))
  -- | Pipe-events factory (index, workload name, input queue name): the tracing
  -- handlers the pipe fires at each queue event. Both names are given because
  -- the two queues are named by different things: the payload queue is the
  -- workload's own, the input queue may be shared with other workloads.
  -> (Int -> String -> String -> IO (PipeHandle key input))
  -- | Recycler-events factory (index, workload name, input queue name): the
  -- tracing handlers the recycler fires as payloads enter the backlog and as it
  -- recycles inputs. It gets the input queue's name for the same reason the
  -- pipe-events factory does: what it recycles into may be shared.
  -> (Int -> String -> String -> IO (RecyclerHandle key input payload))
  -- | Observer factory (index, name, config).
  -> (Int -> String -> Raw.Observer -> IO (ObserverHandle key))
  -- | Folds over the initial inputs, run once the input queues exist.
  -> InputFold input
  -> Validated.Config
  -> IO (Runtime key input payload)
resolve
    mkBuilderFn mkPipeHandleFn mkRecyclerHandleFn mkObserverFn
    foldInitialInputs validatedConfig = do
  let workloadsMap = Validated.workloads validatedConfig
  -- The name of the one input queue the workloads share. It cannot collide with
  -- a workload name because a workload name may not start with @\@@ (checked at
  -- validation time), the scheme 'Validated.rateLimitKey' already uses.
  let sharedInputQueueName = "@shared"
  -- The input queues, decided by the scope: which one each workload uses, by
  -- name, and what each one is loaded with at startup. An input queue is shared
  -- by every workload whose name here is the same: all of them take from it and
  -- all of their recyclers enqueue back into it. The name is also what the
  -- pipe's input-queue events are reported under (it is passed to the
  -- pipe-events factory).
  let inputQueueNameByWorkload =
        case Validated.initialInputsScope validatedConfig of
          -- One queue for every workload, holding the whole load. A workload
          -- that keeps more inputs in flight simply takes more of it.
          Raw.InputsShared ->
            Map.map (const sharedInputQueueName) workloadsMap
          -- One queue per workload, named after it. Nothing is shared: a
          -- workload only ever sees the inputs of its own queue.
          Raw.InputsPerWorkload ->
            Map.mapWithKey (\wlName _ -> wlName) workloadsMap
  -- Resolve the name-keyed pools in dependency order: each resolver takes the
  -- already-resolved pools its entries reference. Observers stand alone, a
  -- recycler needs its pipe, a builder and a workload's targets need their pipe
  -- and recycler, and a forwarder bridges an observer to a recycler.
  resolvedObservers  <- resolveObservers  mkObserverFn
                          (Validated.observers validatedConfig)
  resolvedPipes      <- resolvePipes      mkPipeHandleFn
                          inputQueueNameByWorkload
                          foldInitialInputs
                          (Validated.payloadQueueCapacity validatedConfig)
                          workloadsMap
  resolvedRecyclers  <- resolveRecyclers  mkRecyclerHandleFn
                          inputQueueNameByWorkload
                          resolvedPipes
                          workloadsMap
  resolvedBuilders   <- resolveBuilders   mkBuilderFn
                          resolvedPipes     resolvedRecyclers
                          workloadsMap
  resolvedWorkloads  <- resolveWorkloads
                          resolvedPipes     resolvedRecyclers
                          workloadsMap
  resolvedForwarders <- resolveForwarders
                          resolvedRecyclers resolvedObservers
                          workloadsMap
  -- Assemble the final runtime.
  pure Runtime
    { config     = validatedConfig
    , builders   = resolvedBuilders
    , pipes      = resolvedPipes
    , recyclers  = resolvedRecyclers
    , observers  = resolvedObservers
    , forwarders = resolvedForwarders
    , workloads  = resolvedWorkloads
    -- Collect all asyncs. Each pool entry carries a single async
    -- ('builderAsync', 'recyclerAsync', 'forwarderAsync', 'observerAsync').
    , asyncs     =    map builderAsync   (Map.elems resolvedBuilders)
                   ++ map recyclerAsync  (Map.elems resolvedRecyclers)
                   ++ map forwarderAsync (Map.elems resolvedForwarders)
                   ++ map observerAsync  (Map.elems resolvedObservers)
    }

--------------------------------------------------------------------------------
-- Named-pool resolution (builders, pipes, recyclers, observers).
--------------------------------------------------------------------------------

-- Definitions are listed builder, pipe, recycler, observer for consistency
-- with the field order. 'resolve' calls them in the reverse (dependency) order.

-- | Resolve one builder per workload into a name-keyed pool: spawn its build
-- loop over the workload's pipe and recycler (both by workload name). Each loop
-- takes inputs, builds a payload, records the tx's recyclable inputs with the
-- recycler ('Recycler.hold'), then enqueues the payload. Each 'Builder'
-- keeps its loop thread as 'builderAsync' (which 'resolve' also collects into
-- 'asyncs'). The pipe fires its own (pure) trace handlers. The recycler owns
-- all recycle timing.
resolveBuilders
  -- | Builder factory (index, name, config).
  :: (Int -> String -> Raw.Builder -> IO (BuilderHandle key input payload))
  -> Map String (Pipe.Pipe key input payload)
  -> Map String (Recycler key input payload)
  -> Map String Validated.Workload
  -> IO (Map String Builder)
resolveBuilders mkBuilderFn resolvedPipes resolvedRecyclers workloadsMap =
  Map.fromAscList <$> mapM
    (\(ix, (wlName, validatedWorkload)) -> do
      let thePipe  = resolvedPipes     Map.! wlName
          recycler = recyclerInternal (resolvedRecyclers Map.! wlName)
          strategy = Raw.builderRecycle (Validated.builder validatedWorkload)
      builderHandle <- mkBuilderFn ix wlName (Validated.builder validatedWorkload)
      -- The safe capability API handed to the builder loop: closures over this
      -- builder's pipe and recycler, never the raw resources. 'baAddPayload'
      -- holds the recyclable inputs with the recycler, then makes the payload
      -- dequeuable, in that order: a release must never precede its
      -- 'Hold' (see the Recycler invariants). Strategy wiring: no
      -- strategy holds nothing, on_build confirms right at the build
      -- ('Recycler.releaseOutputs', reason-free), the deferred strategies
      -- confirm later (at dequeue or on an observer confirm). 'baDropInputs'
      -- abandons a batch: 'Pipe.takeInputs' already removed it from the input
      -- queue, so skipping the recycler is all it takes to drop it.
      let recordBuild key consumed outputInputs = case strategy of
            Nothing -> pure ()
            Just Raw.RecycleOnBuild -> do
              Recycler.hold recycler key consumed outputInputs
              Recycler.releaseOutputs recycler key
            Just _ ->
              Recycler.hold recycler key consumed outputInputs
          api = BuilderApi
            { baTakeInputs = Pipe.takeInputs thePipe
            , baAddPayload = \key payload consumed outputInputs -> do
                recordBuild key consumed outputInputs
                Pipe.addPayload thePipe key payload
            , baDropInputs = \_inputs -> pure ()
            }
      async <- Async.async $ do
        -- Always labeled threads.
        tid <- myThreadId
        labelThread tid wlName
        bhRunBuilder builderHandle api
      Async.link async
      pure ( wlName
           , Builder { builderName     = wlName
                     , builderPipe     = wlName
                     , builderRecycler = wlName
                     , builderAsync    = async
                     }
           )
    )
    -- Zero-based index and name provided to the builder factory.
    (zip [0..] (Map.toAscList workloadsMap))

-- | Resolve one pipe per workload into a name-keyed pool (name = workload
-- name), then load the initial inputs. 'Pipe.mkPipe' only assembles the pipe
-- and wires the caller's tracing handlers. The pipe knows nothing about
-- recycling.
--
-- Every queue is created here, which is where the sharing is decided. One input
-- queue per name in 'inputsByInputQueue', so workloads mapped to the same name
-- get the same queue and share every input in it (see 'Raw.InputsScope'). One
-- payload queue per pipe at the given capacity, never shared: 'Pipe.mkPipe'
-- documents what is missing before two pipes could be given the same one.
--
-- The load is per queue, not per pipe: a queue that several workloads share
-- holds one undivided set of inputs and is filled once, through the pipe of the
-- lowest-named workload on it (loading it through each sharing pipe would
-- enqueue every input once per pipe). Which pipe does it is not observable:
-- every pipe on a queue is given that queue's name for its input-queue events.
resolvePipes
  -- | Pipe-events factory (index, workload name, input queue name): the tracing
  -- handlers the pipe fires at each queue event.
  :: (Int -> String -> String -> IO (PipeHandle key input))
  -- | Name of the input queue each workload's pipe is built over, keyed by
  -- workload name. Two workloads under one name share that queue. The distinct
  -- names here are the queues to create.
  -> Map String String
  -- | Folds over the initial inputs, run once the queues exist.
  -> InputFold input
  -- | Capacity of each payload queue (from 'Validated.payloadQueueCapacity').
  -> Natural
  -> Map String Validated.Workload
  -> IO (Map String (Pipe.Pipe key input payload))
resolvePipes
    mkPipeHandleFn inputQueueNameByWorkload foldInitialInputs
    payloadQueueCapacity workloadsMap = do
  -- The input queues, keyed by name: an empty queue and its depth counter
  -- each, one per distinct name however many workloads share it.
  inputQueues <- mapM
    (\() -> do
      inputQueue <- STM.newTQueueIO
      inputDepth <- STM.newTVarIO (0 :: Natural)
      pure (inputQueue, inputDepth)
    )
    (Map.fromList
      [ (inputQueueName, ())
      | inputQueueName <- Map.elems inputQueueNameByWorkload
      ])
  -- One pipe per workload, each tagged with the name of the input queue it was
  -- built over. Both maps below are projections of this one pass, so no second
  -- lookup can disagree with it about which queue a pipe belongs to.
  builtPipes <- mapM
    (\(ix, (wlName, _validatedWorkload)) -> do
      let inputQueueName = inputQueueNameByWorkload Map.! wlName
          (inputQueue, inputDepth) = inputQueues Map.! inputQueueName
      -- The input queue's name goes to the factory alongside the workload's:
      -- the input-queue events belong to the queue, which the workload may not
      -- have to itself.
      pipeHandle <- mkPipeHandleFn ix wlName inputQueueName
      -- This pipe's own payload queue, the pipeline's sole backpressure.
      payloadQueue <- STM.newTBQueueIO payloadQueueCapacity
      let thePipe = Pipe.mkPipe
                      inputQueue
                      inputDepth
                      payloadQueue
                      (phOnInputsEnqueued  pipeHandle)
                      (phOnInputsDequeued  pipeHandle)
                      (phOnPayloadEnqueued pipeHandle)
                      (phOnPayloadDequeued pipeHandle)
      pure (wlName, inputQueueName, thePipe)
    )
    -- Zero-based index and both names provided to the pipe factory.
    (zip [0..] (Map.toAscList workloadsMap))
  let -- The pool. 'builtPipes' is in ascending workload order, as it was built
      -- from 'Map.toAscList'.
      resolvedPipes = Map.fromAscList
        [ (wlName, thePipe) | (wlName, _, thePipe) <- builtPipes ]
      -- The pipe each input queue is loaded through, keyed by input queue name.
      -- Several pipes can be built over one queue and any of them will do, as
      -- they all report the load under that queue's name, so the first workload
      -- on it wins: @\_later first -> first@ keeps the entry already there.
      loaderPipes = Map.fromListWith (\_later first -> first)
        [ (inputQueueName, thePipe)
        | (_, inputQueueName, thePipe) <- builtPipes
        ]
  -- Fold the initial inputs into the queues, one at a time through
  -- 'Pipe.addInputs', the same call the recycler uses so each enters exactly
  -- like a recycled one. Nothing is held in between: an input is queued as it
  -- arrives, so millions of them cost the queues and no more.
  --
  -- One rule covers both scopes: the queues in name order, round-robin by the
  -- input's position. Under "shared" there is one queue and every input lands
  -- on it; under "per_workload" they spread evenly over the workloads without
  -- anyone having to know how many inputs there will be.
  let loaderPipeList  = Map.elems loaderPipes
      inputQueueCount = length loaderPipeList
  -- The accumulator is the input's position, which is also the count.
  fedCount <- foldInitialInputs
    (\ix input -> do
      Pipe.addInputs (loaderPipeList !! (ix `mod` inputQueueCount)) [input]
      pure $! ix + 1
    )
    (0 :: Int)
  -- The inputs have to cover every queue: with fewer, a queue starts empty and
  -- the workload on it waits for inputs that no build can produce. The rule
  -- lives here because the count exists only once the inputs are in.
  when (fedCount < inputQueueCount) $
    fail $ "Runtime: not enough initial inputs (" ++ show fedCount
        ++ ") for " ++ show inputQueueCount ++ " input queue(s)"
  pure resolvedPipes

-- | Resolve one recycler per workload into a name-keyed pool. For each, build
-- the 'Internal.Recycler' logic and start its worker (the thread that writes
-- this workload's recycled inputs back onto its pipe's input queue, which the
-- workers of other workloads also write to when that queue is shared). Each
-- 'Recycler' wraps that logic with its worker 'recyclerAsync', collected into
-- 'asyncs'. The observer bridge for
-- 'RecycleOnConfirm' is resolved separately, by 'resolveForwarders'.
resolveRecyclers
  :: Ord key
  -- | Recycler-events factory (index, workload name, input queue name): the
  -- tracing handlers the recycler fires as payloads enter the backlog and as it
  -- recycles inputs.
  => (Int -> String -> String -> IO (RecyclerHandle key input payload))
  -- | Name of the input queue each workload recycles into, keyed by workload
  -- name (the same map 'resolvePipes' built its pipes from).
  -> Map String String
  -> Map String (Pipe.Pipe key input payload)
  -> Map String Validated.Workload
  -> IO (Map String (Recycler key input payload))
resolveRecyclers mkRecyclerHandleFn inputQueueNameByWorkload resolvedPipes workloadsMap =
  Map.fromAscList <$> mapM
    (\(ix, (wlName, _validatedWorkload)) -> do
      let inputQueueName = inputQueueNameByWorkload Map.! wlName
      recyclerHandle <- mkRecyclerHandleFn ix wlName inputQueueName
      internal <- Recycler.mkRecycler
                    (resolvedPipes Map.! wlName)
                    (rhOnHold recyclerHandle)
                    (rhOnRelease recyclerHandle)
                    (rhOnReset recyclerHandle)
      worker <- Recycler.runRecycler internal wlName
      Async.link worker
      pure ( wlName
           , Recycler { recyclerName     = wlName
                      , recyclerInternal = internal
                      , recyclerRecover  = rhRecover recyclerHandle
                      , recyclerAsync    = worker
                      }
           )
    )
    -- Zero-based index and both names provided to the recycler factory.
    (zip [0..] (Map.toAscList workloadsMap))

-- | Resolve the observers into a name-keyed pool, spawning each one's 'ohRun'
-- in a labeled, linked async. Each 'Observer' keeps its 'ObserverHandle' (as
-- 'observerHandle'), which the forwarders subscribe to on behalf of each
-- referencing workload's recycler.
resolveObservers
  -- | Observer factory (index, name, config).
  :: (Int -> String -> Raw.Observer -> IO (ObserverHandle key))
  -> Map String Raw.Observer
  -> IO (Map String (Observer key))
resolveObservers mkObserverFn rawObservers =
  Map.fromAscList <$> mapM
    (\(ix, (obsName, rawObs)) -> do
      obsHandle <- mkObserverFn ix obsName rawObs
      obsAsync  <- Async.async $ do
        -- Always labeled threads.
        tid <- myThreadId
        labelThread tid ("observer/" ++ obsName)
        ohRun obsHandle
      Async.link obsAsync
      pure ( obsName
           , Observer { observerName   = obsName
                      , observerHandle = obsHandle
                      , observerAsync  = obsAsync
                      }
           )
    )
    -- Zero-based index and name provided to the observer factory.
    (zip [0..] (Map.toAscList rawObservers))

--------------------------------------------------------------------------------
-- Forwarders (observer to recycler).
--------------------------------------------------------------------------------

-- | Resolve the forwarders that bridge observers to recyclers. Every forwarder
-- is the same thing: a thread that reads one subscribed observer stream and
-- feeds each event through the workload's strategy wiring into the recycler
-- actions. The recycler knows no strategy, this wiring is the only place an
-- observer event is interpreted:
--
--   * 'Right' (confirmed) becomes 'Recycler.releaseOutputs' under on_confirm
--     and is ignored otherwise.
--   * 'Left' (orphaned) runs the workload's recovery action when one is
--     wired (a rollback invalidates the queued inputs, and the payloads
--     built from them, beyond the orphaned payload itself) and feeds its
--     result to 'Recycler.reset'. Under on_confirm the reset is KEYED:
--     built payloads stay held until their release, so the recycler applies
--     the reset only while the orphaned payload is still held, and a
--     foreign or duplicate orphan is ignored. The orphan burst of one
--     rollback then collapses at the backlog gate (at one recovery query
--     per orphan event). Under the optimistic strategies the payload was
--     already released at build or dequeue, so the reset is unkeyed and
--     unconditional, and the forwarder then discards the orphan burst the
--     rollback already delivered (one rollback, one recovery) while still
--     forwarding the confirms in it. Without a recovery a 'Left' becomes
--     'Recycler.releaseConsumed' under on_confirm, and is ignored otherwise.
--
-- A workload subscribes one forwarder per observer reference in its builder:
-- the 'Raw.RecycleOnConfirm' observer, and the observer its 'Raw.Recovery'
-- names explicitly. An on_confirm recovery without an observer adds no
-- reference (the default, the confirm subscription already carries the
-- orphans). Naming the confirm observer again explicitly creates a second
-- subscription, each event then delivered twice (duplicate confirms, and
-- keyed duplicate resets, are ignored at the unknown key).
--
-- These are the sole bridges from the observers (which know a transaction's
-- outcome) to the recyclers, which are observer-agnostic and drain only their
-- own event queues. Workloads with no strategy or no observer reference
-- contribute no forwarder. Returns the name-keyed 'forwarders' pool, each
-- entry named by its wiring path (see 'Forwarder'), whose asyncs 'resolve'
-- collects into 'asyncs'.
resolveForwarders
  -- | The recyclers to notify, keyed by workload name.
  :: Map String (Recycler key input payload)
  -- | Observers keyed by name (a strategy names the ones to subscribe to).
  -> Map String (Observer key)
  -- | Validated workloads (their recycle strategy and recovery select the
  -- observers).
  -> Map String Validated.Workload
  -> IO (Map String Forwarder)
resolveForwarders resolvedRecyclers resolvedObservers workloadsMap =
  fmap (Map.fromList . concat) $ mapM
    (\(wlName, validatedWorkload) ->
      case Raw.builderRecycle (Validated.builder validatedWorkload) of
        Nothing -> pure []
        Just strategy -> do
          -- All lookups are guaranteed present: recyclers are keyed by
          -- workload name, and 'Validated.validate' rejects an undefined
          -- observer reference.
          let recyclerEntry = resolvedRecyclers Map.! wlName
              recycler      = recyclerInternal recyclerEntry
              -- Strategy wiring: what a confirm means here (see the haddock
              -- above).
              confirmRight key = case strategy of
                Raw.RecycleOnConfirm _ -> Recycler.releaseOutputs recycler key
                _                      -> pure ()
              -- One subscription per observer reference, tagged with the
              -- config site that wired it ("recycle" or "recovery"): the
              -- tag makes the pool keys unique when both sites name the
              -- same observer (the documented double subscription).
              subscriptions =
                [ ("recycle", obsName)
                | Raw.RecycleOnConfirm obsName <- [strategy]
                ]
                ++ [ ("recovery", obsName)
                   | Just recovery <-
                       [ Raw.builderRecovery
                           (Validated.builder validatedWorkload)
                       ]
                   , Just obsName  <- [Raw.recoveryObserver recovery]
                   ]
          mapM
            (\(site, obsName) -> do
              let forwarderKey = wlName ++ "/" ++ site ++ "/" ++ obsName
              readEvent <- ohSubscribe
                (observerHandle (resolvedObservers Map.! obsName))
              let -- Strategy wiring: what an orphan means here (see the
                  -- haddock above).
                  step event = case event of
                    Right key -> confirmRight key
                    Left  key -> case recyclerRecover recyclerEntry of
                      Just recover -> do
                        fresh <- recover
                        case strategy of
                          -- on_confirm holds a payload until its release, so at
                          -- its orphan the key is still held and the reset can
                          -- be gated on it: the recycler drops a foreign or
                          -- duplicate one. No drain here, the backlog gate
                          -- absorbs the burst (at one recovery query per orphan
                          -- event).
                          Raw.RecycleOnConfirm _ ->
                            Recycler.reset recycler (Just key) fresh
                          -- The optimistic strategies released the payload at
                          -- build or dequeue, so an own orphan is no longer
                          -- held and the reset cannot be gated: unkeyed and
                          -- unconditional, followed by the drain that absorbs
                          -- the rollback's burst into one recovery.
                          _ -> do
                            Recycler.reset recycler Nothing fresh
                            drainOrphans
                      Nothing -> case strategy of
                        Raw.RecycleOnConfirm _ ->
                          Recycler.releaseConsumed recycler key
                        _ -> pure ()
                  -- Forward the events this subscription already delivered,
                  -- dropping orphans (the unkeyed reset that just ran covers
                  -- them), until it is momentarily empty. One rollback delivers
                  -- a burst of orphans, this absorbs it into one recovery. Only
                  -- the unkeyed reset path may drain: after a keyed reset the
                  -- recycler might have ignored it, and draining would then
                  -- discard genuine orphans.
                  drainOrphans = do
                    mEvent <- STM.atomically
                      ((Just <$> readEvent) `STM.orElse` pure Nothing)
                    case mEvent of
                      Nothing          -> pure ()
                      Just (Left _)    -> drainOrphans
                      Just (Right key) -> confirmRight key >> drainOrphans
              forwarder <- Async.async $ do
                -- Always labeled threads.
                tid <- myThreadId
                labelThread tid forwarderKey
                forever (STM.atomically readEvent >>= step)
              Async.link forwarder
              pure ( forwarderKey
                   , Forwarder { forwarderName     = forwarderKey
                               , forwarderObserver = obsName
                               , forwarderRecycler = wlName
                               , forwarderAsync    = forwarder
                               }
                   )
            )
            subscriptions
    )
    (Map.toAscList workloadsMap)

--------------------------------------------------------------------------------
-- Workload resolution.
--------------------------------------------------------------------------------

-- | Resolve every workload's targets into a name-keyed pool, threading the
-- rate-limiter cache across all of them so top-level Shared limiters are
-- reused.
-- Each workload's targets fetch from that workload's pipe and signal its
-- recycler (both looked up by workload name). See 'resolveWorkload'.
resolveWorkloads
  :: Map String (Pipe.Pipe key input payload)
  -> Map String (Recycler key input payload)
  -> Map String Validated.Workload
  -> IO (Map String (Workload key input payload))
resolveWorkloads resolvedPipes resolvedRecyclers workloadsMap = do
  (resolvedWorkloads, _) <- foldlM
    (\(acc, cache) (wlName, validatedWorkload) -> do
      (resolved, cache') <-
        resolveWorkload
          validatedWorkload
          cache
          (resolvedPipes Map.! wlName)
          (recyclerInternal (resolvedRecyclers Map.! wlName))
      pure (Map.insert wlName resolved acc, cache')
    )
    (Map.empty, Map.empty)
    (Map.toAscList workloadsMap)
  pure resolvedWorkloads

-- | Resolve a single workload: build each target's rate-limited recycling fetch
-- and resolve each target's rate limiter.
--
-- The 'Pipe' and its 'Recycler' come from the 'pipes' \/ 'recyclers' pools
-- (created by 'resolvePipes' \/ 'resolveRecyclers') and are passed in so that
-- all of the workload's targets share the same underlying queues and recycle
-- loop.
--
-- Cascading defaults and conflict checks have already been performed by
-- "PullFiction.Config.Validated". This function only
-- creates rate limiters and fetchers.
resolveWorkload
  :: Validated.Workload
  -- | Limiter cache (threaded as a pure accumulator).
  -> LimiterCache
  -- | Pipe shared by all the workload's targets (from the 'pipes' pool).
  -> Pipe.Pipe key input payload
  -- | Recycler for this workload (from the 'recyclers' pool).
  -> Recycler.Recycler key input payload
  -> IO (Workload key input payload, LimiterCache)
resolveWorkload validatedWorkload cache0 thePipe recycler = do
  let wlName = Validated.workloadName validatedWorkload
      validatedTargets = Validated.targets validatedWorkload
      -- Strategy wiring: only on_pull confirms a payload at dequeue
      -- ('Recycler.releaseOutputs', reason-free).
      confirmDequeued =
        case Raw.builderRecycle (Validated.builder validatedWorkload) of
          Just Raw.RecycleOnDequeue -> Recycler.releaseOutputs recycler
          _                         -> \_key -> pure ()
  (resolvedTargets, cache') <- foldlM
    (\(acc, cache) (tName, validatedTarget) -> do
      (resolved, cache'') <-
        resolveTarget cache thePipe confirmDequeued validatedTarget
      pure (Map.insert tName resolved acc, cache'')
    )
    (Map.empty, cache0)
    (Map.toAscList validatedTargets)
  pure ( Workload { workloadName = wlName
                  , targets      = resolvedTargets
                  }
       , cache'
       )

--------------------------------------------------------------------------------
-- Target resolution.
--------------------------------------------------------------------------------

-- | Resolve a single target: look up or create its rate limiter from the cache,
-- build its rate-limited recycling fetch around the workload's dequeue wiring,
-- then build the 'Target' record.
resolveTarget
  :: LimiterCache
  -> Pipe.Pipe key input payload
  -- | The workload's dequeue wiring, fired with each dequeued payload's key
  -- (for on_pull it confirms the payload, otherwise a no-op).
  -> (key -> IO ())
  -> Validated.Target
  -> IO (Target key input payload, LimiterCache)
resolveTarget cache thePipe confirmDequeued validatedTarget = do
  (limiter, cache') <- getOrCreateLimiter cache validatedTarget
  let onEx = Validated.onExhaustion validatedTarget
      -- The recycling fetch: fetch from the pipe, fire the workload's dequeue
      -- wiring, deliver the payload.
      inner = Pipe.payloadFetcher thePipe limiter onEx
      fetcher = Pipe.PayloadFetcher
        { Pipe.fetchPayload = do
            (key, payload) <- Pipe.fetchPayload inner
            confirmDequeued key
            pure payload
        , Pipe.tryFetchPayload = do
            mKeyPayload <- Pipe.tryFetchPayload inner
            case mKeyPayload of
              Nothing             -> pure Nothing
              Just (key, payload) -> do
                confirmDequeued key
                pure (Just payload)
        }
  pure ( Target
           { targetName    = Validated.targetName validatedTarget
           , targetFetcher = fetcher
           , rateLimiter   = limiter
           , maxBatchSize  = Validated.maxBatchSize validatedTarget
           , onExhaustion  = onEx
           , targetAddr    = Validated.addr validatedTarget
           , targetPort    = Validated.port validatedTarget
           }
       , cache'
       )

-- | Look up or create a 'RL.RateLimiter' for a target. Limiters are shared by
-- the pre-computed 'Validated.rateLimitKey', which encodes the sharing scope:
--
-- * @\@global@: one limiter for all targets across all workloads.
-- * @workloadName@: one per workload.
-- * @workloadName.targetName@: one per target.
-- * no rate-limit source: 'RL.newUnlimited' (uncached).
--
-- A cache hit reuses the existing limiter. A miss creates a
-- 'RL.newTokenBucket', inserts it, and returns it.
getOrCreateLimiter
  :: LimiterCache
  -> Validated.Target
  -> IO (RL.RateLimiter, LimiterCache)
getOrCreateLimiter cache target =
  case Validated.rateLimitSource target of
    Nothing  -> pure (RL.newUnlimited, cache)
    Just src -> do
      let key      = Validated.rateLimitKey src
          tpsValue = Raw.tps (Validated.rateLimit src)
      case Map.lookup key cache of
        Just existing -> pure (existing, cache)
        Nothing       -> do
          limiter <- RL.newTokenBucket tpsValue
          pure (limiter, Map.insert key limiter cache)

