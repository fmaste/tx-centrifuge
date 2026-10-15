{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

-- "cardano-api" 11 deprecates `Api.getTxBody` in favour of `UnsignedTx` from
-- "Cardano.Api.Experimental". This package stays on the stable API so the same
-- source builds against "cardano-node" 10.7.1 through 11.1.1, also the node
-- compiles local packages with `-Werror` so the warning has to go.
{-# OPTIONS_GHC -Wno-deprecations #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Concurrent (threadDelay)
import Control.Exception (catch, finally)
import Control.Monad (foldM, forM_, forever, when)
import Data.Bifunctor (first)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import GHC.Conc.Sync (threadLabel)
import Numeric.Natural (Natural)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
-----------
-- aeson --
-----------
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
-----------
-- async --
-----------
import Control.Concurrent.Async qualified as Async
----------------
-- bytestring --
----------------
import Data.ByteString.Char8 qualified as BS8
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
-------------------------
-- cardano-ledger-core --
-------------------------
import Cardano.Ledger.Coin qualified as L
------------------
-- cardano-node --
------------------
import Cardano.Node.Configuration.POM
  ( parseNodeConfigurationFP
  , makeNodeConfiguration
  , defaultPartialNodeConfiguration
  , PartialNodeConfiguration(..)
  , NodeConfiguration
  , ncProtocolConfig
  )
import Cardano.Node.Handlers.Shutdown (ShutdownConfig(..))
import Cardano.Node.Protocol.Cardano (mkSomeConsensusProtocolCardano)
import Cardano.Node.Protocol.Types (SomeConsensusProtocol(..))
import Cardano.Node.Types
  ( ConfigYamlFilePath(..)
  , KESSource(..)
  , NodeProtocolConfiguration(..)
  , ProtocolFilepaths(..)
  )
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
-------------
-- network --
-------------
import Network.Socket qualified as Socket
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import Ouroboros.Consensus.Config (configBlock, configCodec)
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo(..))
---------------------------------
-- ouroboros-network:framework --
---------------------------------
import Ouroboros.Network.IOManager (withIOManager)
---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
----------
-- text --
----------
import Data.Text qualified as Text
------------------
-- transformers --
------------------
import Control.Monad.Trans.Except (runExceptT)
--------------------------------
-- tx-centrifuge:pull-fiction --
--------------------------------
import PullFiction.Config.Raw qualified as Raw
import PullFiction.Config.Runtime qualified as Runtime
import PullFiction.Config.Validated qualified as Validated
import PullFiction.WorkloadRunner (runWorkload)
---------------------------
-- tx-centrifuge:cardano --
---------------------------
import Cardano.Benchmarking.TxCentrifuge.Chain qualified as Chain
import Cardano.Benchmarking.TxCentrifuge.Era (Era, era)
import Cardano.Benchmarking.TxCentrifuge.NodeToClient qualified as N2C
import Cardano.Benchmarking.TxCentrifuge.NodeToClient.TxIdSync
  qualified as TxIdSyncN2C
import Cardano.Benchmarking.TxCentrifuge.NodeToNode qualified as N2N
import Cardano.Benchmarking.TxCentrifuge.NodeToNode.KeepAlive
  qualified as KeepAlive
import Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxIdSync
  qualified as TxIdSyncN2N
import Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxSubmission
  qualified as TxSubmission
import Cardano.Benchmarking.TxCentrifuge.Fund qualified as Fund
import Cardano.Benchmarking.TxCentrifuge.Tracing qualified as Tracing
import Cardano.Benchmarking.TxCentrifuge.TxAssembly qualified as TxAssembly
-----------------------------------
-- tx-centrifuge:cardano-tracing --
-----------------------------------
import Cardano.Benchmarking.TxCentrifuge.Tracing.Dispatcher
  qualified as TracingDispatcher

--------------------------------------------------------------------------------

-- | Runs the program and names the thread when a linked one kills everything.
--
-- A linked thread's death propagates as an @ExceptionInLinkedThread@ exception
-- carrying, alongside the original exception, only the thread's id and nothing
-- else because the labels we set to every 'Async' with 'GHC.Conc.labelThread'
-- go to the eventlog and never to 'show'. Simpler: the default message is a
-- bare @Thread 910@ with nothing useful to map to our labeled 'Async's.
-- Here the label is read back and put in the message. The table 'main'' prints
-- to stderr covers what this cannot: it is written at startup, so it is there
-- for a death that never reaches this handler, and it lists every 'Async' we
-- link, so an id absent from it belongs to none of them.
main :: IO ()
main =
  main' `catch` \(Async.ExceptionInLinkedThread async_ inner) -> do
    let tid = Async.asyncThreadId async_
    label <- threadLabel tid
    die $ show tid ++ " (" ++ fromMaybe "<unlabelled>" label ++ ") died: "
       ++ show inner

--------------------------------------------------------------------------------

main' :: IO ()
main' = do

  -- Loand and validate config.
  -----------------------------

  (   isDryRun
    , validated
    , InitialFunds foldInitialFunds
    , codecConfig
    , networkId
    , networkMagic
    , tracers
    ) <- loadConfig

  -- Dry run: config and protocol have been validated above, and the funds are
  -- validated by running their fold, which is what reads the fund file or
  -- queries the node.
  when isDryRun $ do
    -- Folded into a count: no input queue exists yet, and none is created, so
    -- this reads the funds and nothing else. The count is forced at every step
    -- so a file of millions does not leave a chain of thunks behind.
    fundCount <- foldInitialFunds
      (\count _fund -> let count' = count + 1 in count' `seq` pure count')
      (0 :: Natural)
    when (fundCount == 0) $ die "initial_inputs: no funds loaded"
    hPutStrLn stderr $
         "Dry run OK: config and " ++ show fundCount
      ++ " funds validated, exiting."
    exitSuccess

  -- Callbacks / handlers.
  ------------------------

  -- From 'String' (address) and 'Int' (port) to 'AddrInfo'.
  let resolveAddr ip port = do
        let hints = Socket.defaultHints
              { Socket.addrSocketType = Socket.Stream
              , Socket.addrFamily     = Socket.AF_INET
              }
        addrs <- Socket.getAddrInfo
          (Just hints)
          (Just ip)
          (Just (show port))
        case addrs of
          []    -> die $ "Cannot resolve target: " ++ ip ++ ":" ++ show port
          (a:_) -> pure a

  -- Builder factory passed to 'Runtime.resolve'. Returns a 'BuilderHandle'.
  -- Receives builder's zero-based index and name and the opaque builder config.
  let mkBuilder builderIndex builderName rawBuilder = do
        -- Interpret the opaque builder config into a concrete builder, with its
        -- destination signing key and address already resolved.
        builder <- interpretBuilder networkId builderIndex rawBuilder
        -- Announce the destination addresses so an operator knows which
        -- addresses to fund and can inspect their UTxOs. One line per builder.
        hPutStrLn stderr $
          "Builder " ++ builderName ++ ": destination addresses "
          ++ Text.unpack
               (Text.intercalate
                 ", "
                 (map (Api.serialiseAddress . snd) (destinations builder))
               )
        -- This builder owns its loop: it pulls a fixed 'inputsPerTx' batch,
        -- builds a transaction, and either publishes it or, when the batch is
        -- unbuildable (an all-dust batch whose input value does not cover the
        -- fee), drops it and stays up. The engine hands over 'api', a sealed
        -- window onto this builder's pipe and recycler.
        pure $ Runtime.BuilderHandle $ \api -> forever $ do
          inputFunds <- Runtime.baTakeInputs api (inputsPerTx builder)
          let buildTxAns = TxAssembly.buildTx
                             era
                             (destinations builder)
                             inputFunds (outputsPerTx builder)
                             (L.Coin (fee builder))
          case buildTxAns of
            -- A per-batch value failure: these particular inputs cannot cover
            -- the fee plus one valid output each (the only 'buildTx' failure
            -- that depends on the inputs). Abandon the batch via 'baDropInputs'
            -- (the engine neither recycles nor enqueues it), then emit the
            -- TxCentrifuge.Builder.InputsDropped trace. Action first, then the
            -- trace, as everywhere else.
            --
            -- Whole-batch, not per-input, on purpose: 'buildTx' sums ALL
            -- inputs, so a batch fails only when they are COLLECTIVELY worth at
            -- most the fee. A small input among good ones is summed in and
            -- spent, never dropped. Filtering out every input with value <= fee
            -- would instead discard dust that is productively swept into a tx
            -- today.
            Left (TxAssembly.InsufficientValue reason) -> do
              Runtime.baDropInputs api inputFunds
              Tracing.traceWith
                (Tracing.trBuilder tracers)
                (Tracing.BuilderInputsDropped builderName inputFunds reason)
            -- The remaining failures do not depend on the batch, so every batch
            -- would hit them and dropping would spin forever: fail loudly.
            -- 'InvalidInput' is a bad builder argument (already guarded in
            -- 'interpretBuilder', so defensive here). 'LedgerFailure' is an
            -- opaque cardano-api construction error.
            Left (TxAssembly.InvalidInput reason) ->
              die $ "TxAssembly.buildTx: invalid builder parameters: " ++ reason
            Left (TxAssembly.LedgerFailure reason) ->
              die $ "TxAssembly.buildTx: ledger construction failed: " ++ reason
            Right (tx, txRef, outputFunds) -> do
              -- Trace the newly built transaction
              -- (TxCentrifuge.Builder.NewTx), purely a construction event, no
              -- pipe/queue info.
              Tracing.traceWith
                (Tracing.trBuilder tracers)
                (Tracing.BuilderNewTx
                  builderName txRef
                  -- The destination addresses.
                  (map snd (destinations builder))
                  inputFunds outputFunds
                )
              Runtime.baAddPayload api txRef tx inputFunds outputFunds

  -- Pipe-events factory passed to 'Runtime.resolve'. Returns a 'PipeHandle'
  -- Given builder's zero-based index, builder name and input queue name create
  -- the handlers for the four pipe's queue mechanics (TxCentrifuge.Pipe.*):
  -- payloads added/removed (with the resulting queue depth) and inputs
  -- added/removed (TxCentrifuge.Pipe.Inputs{Enqueued,Dequeued}, with the inputs
  -- themselves and the resulting queue depth).
  let mkPipeHandle _pipeIndex builderName inputQueueName =
        pure Runtime.PipeHandle
          { Runtime.phOnInputsEnqueued = \inputs depth ->
              Tracing.traceWith (Tracing.trPipe tracers)
                (Tracing.PipeInputsEnqueued inputQueueName depth inputs)
          , Runtime.phOnInputsDequeued = \inputs depth ->
              Tracing.traceWith (Tracing.trPipe tracers)
                (Tracing.PipeInputsDequeued inputQueueName depth inputs)
          , Runtime.phOnPayloadEnqueued = \txRef depth ->
              Tracing.traceWith (Tracing.trPipe tracers)
                (Tracing.PipePayloadEnqueued builderName depth txRef)
          , Runtime.phOnPayloadDequeued = \txRef depth ->
              Tracing.traceWith (Tracing.trPipe tracers)
                (Tracing.PipePayloadDequeued builderName depth txRef)
          }

  -- Recovery factory used by 'mkRecyclerHandle'. When the workload's builder
  -- configured a "recovery" (carrying the observer whose orphan events trigger
  -- it and the input source that rebuilds the queued inputs), build the
  -- recovery action the forwarder runs on an orphan: query the UTxOs currently
  -- at the builder's destination address through the source's socket, whose
  -- result reseeds the builder's input queue (the recycler applies it via its
  -- reset, which drops the stale queued inputs and the stale queued payloads
  -- first). The source is independent of the observer, which stays a shared
  -- entity the recovery never modifies: the orphan signal can come from any
  -- observer type or node while the recovery queries a (typically local)
  -- socket, and builders with and without a recovery can name the same
  -- observer. Only a "utxo_query" source can rebuild inputs (a static file
  -- cannot reflect the chain after a rollback). The socket is probed once here
  -- so a wrong "socket_path" dies at startup, not at the first rollback. At
  -- runtime a failed query reports and retries after a delay until it succeeds:
  -- the orphan event that triggered it is already consumed, so giving up would
  -- swallow this rollback's recovery and leave the queue poisoned until the
  -- next one. The query returns failures as 'Left' and never throws (see
  -- 'UTxOQuery.queryUTxOsAtAddresses'), so the retry loop swallows no
  -- exceptions and a genuine exception still ends the run as everywhere.
  let mkRecover builderIndex builderName =
        case Map.lookup builderName (Validated.workloads validated) of
          Nothing -> pure Nothing
          Just wl ->
            case Raw.builderRecovery (Validated.builder wl) of
              Nothing -> pure Nothing
              Just recovery -> do
                -- The source reference is validated, the lookup is total.
                let sourceName = Raw.recoverySource recovery
                    rawSource  =
                      Validated.inputSources validated Map.! sourceName
                socketPath <- case interpretInputSource rawSource of
                  Left err -> die $
                    "input_sources." ++ sourceName ++ ": " ++ err
                  Right (UTxOQuerySource path) -> pure path
                  Right (GenesisKeysSource _)  -> die $
                    "Builder " ++ builderName ++ ": recovery source "
                    ++ show sourceName ++ " is a static file and cannot"
                    ++ " rebuild inputs after a reset, use a \"utxo_query\""
                    ++ " source"
                builder <- interpretBuilder
                             networkId builderIndex (Validated.builder wl)
                -- Startup probe: only connectivity matters, an empty result is
                -- fine (the address fills up as this builder's transactions
                -- confirm).
                eProbe <- Fund.discoverFundsAtAddresses
                            networkId socketPath
                            (destinations builder)
                case eProbe of
                  Left err -> die $
                    "Builder " ++ builderName ++ ": recovery socket ("
                    ++ socketPath ++ "): " ++ err
                  Right _ -> pure ()
                pure $ Just $
                  let attempt = do
                        eFunds <- Fund.discoverFundsAtAddresses
                                    networkId socketPath
                                    (destinations builder)
                        case eFunds of
                          Left err -> do
                            hPutStrLn stderr $
                              "Builder " ++ builderName
                              ++ ": recovery query failed"
                              ++ " (retrying in 1s): " ++ err
                            threadDelay 1_000_000
                            attempt
                          Right funds -> pure funds
                  in attempt

  -- Recycler-events factory passed to 'Runtime.resolve'. Returns a
  -- 'RecyclerHandle'. Given the builder's zero-based index and name and the
  -- name of the input queue it recycles into, creates the handlers that trace
  -- when a payload enters the backlog (TxCentrifuge.Recycler.Backlog.Held),
  -- when it leaves it (TxCentrifuge.Recycler.Backlog.Released.*), and the
  -- optional recovery action (see 'mkRecover'). The recycler is named
  -- by the workload (one per workload) and the pipe by that input queue, the
  -- same split 'mkPipeHandle' makes, so one enqueue is reported under one name
  -- by both TxCentrifuge.Pipe.InputsEnqueued and this.
  let mkRecyclerHandle recyclerIndex builderName inputQueueName = do
        recover <- mkRecover recyclerIndex builderName
        pure Runtime.RecyclerHandle
          { Runtime.rhOnHold = \txRef consumed outputs backlog ->
              Tracing.traceWith (Tracing.trRecycler tracers)
                (Tracing.RecyclerHeld
                  builderName backlog txRef consumed outputs)
            -- Why the payload was released, read off the event: the recycler
            -- says which set went back, never why. Only an on_confirm orphan
            -- returns the consumed inputs ('Left'), and only a ref an observer
            -- read off the chain carries a block, which on_build and on_pull
            -- never have.
          , Runtime.rhOnRelease = \txRef released backlog ->
              Tracing.traceWith (Tracing.trRecycler tracers) $
                case (released, Chain.txRefBlock txRef) of
                  (Left consumed, _) ->
                    Tracing.RecyclerReleasedOrphaned
                      builderName inputQueueName backlog txRef consumed
                  (Right outputs, Just _) ->
                    Tracing.RecyclerReleasedConfirmed
                      builderName inputQueueName backlog txRef outputs
                  (Right outputs, Nothing) ->
                    Tracing.RecyclerReleasedOptimistic
                      builderName inputQueueName backlog txRef outputs
            -- The trace keeps the dropped payloads by key, not the full
            -- transactions.
          , Runtime.rhOnReset = \droppedInputs droppedPayloads fresh backlog ->
              Tracing.traceWith (Tracing.trRecycler tracers)
                (Tracing.RecyclerReset
                  builderName inputQueueName
                  backlog (map fst droppedPayloads) droppedInputs fresh
                )
          , Runtime.rhRecover = recover
          }

  -- Observer factory passed to 'Runtime.resolve'. Returns an 'ObserverHandle'.
  -- For each observer in the config creates an N2N or N2C connection for
  -- transaction confirmation tracking.
  -- Takes the 'IOManager' as first argument (partial-applied below).
  let mkObserver ioManager _observerIndex observerName rawObserver = do
        -- From JSON/Aeson.Value to the cardano-node specific observer.
        observer <- case interpretObserver rawObserver of
          Left err -> die $ "Observer " ++ observerName ++ ": " ++ err
          Right o  -> pure o
        -- Observer announce loop: dup the confirmation broadcast and log every
        -- confirmed/orphaned tx (TxCentrifuge.Observer.Announce.{Confirmed,
        -- Orphaned}), decoupled from the pipe and recycling. Runs alongside the
        -- connection in 'ohRun'.
        -- Announce is the whole chain: one line per transaction in every
        -- block this observer sees, so with one observer per node the volume
        -- is the chain's throughput times the observers.
        -- TxCentrifuge.Recycler.Backlog.Released.* is the same events filtered
        -- to ours, a release happening only for a ref that recycler's backlog
        -- held, and it carries the block as well. So keep Announce for what
        -- only it shows, which observer saw a transaction and when, and
        -- silence it otherwise.
        let announceLoop broadcast = do
              chan <- STM.atomically $ STM.dupTChan broadcast
              forever $ do
                eitherTxRef <- STM.atomically $ STM.readTChan chan
                -- The broadcast says which: 'Left' orphaned, 'Right'
                -- confirmed, and each is its own namespace. The key carries
                -- the block it was read in, so nothing else is needed here.
                Tracing.traceWith (Tracing.trObserver tracers) $
                  case eitherTxRef of
                    Left  txRef -> Tracing.ObserverOrphaned  observerName txRef
                    Right txRef -> Tracing.ObserverConfirmed observerName txRef
        case observer of
          -- N2N: ChainSync (headers) + BlockFetch (blocks) + KeepAlive.
          --------------------------------------------------------------
          NodeToNode addr port depth -> do
            syncState <- TxIdSyncN2N.emptyState
              TxIdSyncN2N.Config
                { TxIdSyncN2N.confirmationDepth = depth }
            keepAlive <- KeepAlive.keepAliveClient 10
            let clients = N2N.emptyClients
                  { N2N.clientChainSync  =
                      Just $ TxIdSyncN2N.chainSyncClient  syncState
                  , N2N.clientBlockFetch =
                      Just $ TxIdSyncN2N.blockFetchClient syncState
                  , N2N.clientKeepAlive  = Just keepAlive
#ifdef LEIOS_PROTOTYPE
                    -- Recovers the transactions of a Leios block, which
                    -- ChainSync and BlockFetch cannot see: they are announced
                    -- by the block, not carried in it.
                  , N2N.clientLeiosFetch =
                      Just $ TxIdSyncN2N.leiosFetchClient observerName syncState
#endif
                  }
            addrInfo <- resolveAddr addr port
            pure Runtime.ObserverHandle
              { Runtime.ohRun =
                  -- The announce loop runs alongside the connection and is
                  -- cancelled when the connection ends.
                  Async.withAsync
                    (announceLoop (TxIdSyncN2N.stateBroadcast syncState)) $ \announceAsync -> do
                      -- Link the announce loop so its failure aborts the
                      -- observer instead of being silently swallowed.
                      Async.link announceAsync
                      result <- N2N.connect
                        ioManager codecConfig networkMagic tracers
                        addrInfo clients
                      case result of
                        Left err ->
                          die $ "observer " ++ observerName ++ ": " ++ err
                        Right () -> pure ()
              , Runtime.ohSubscribe = do
                  chan <- STM.atomically $
                    STM.dupTChan (TxIdSyncN2N.stateBroadcast syncState)
                  -- The broadcast is already refs, each carrying what the
                  -- observer read of it (see 'Chain.TxRef'), so the recycler
                  -- gets the event unchanged. Foreign transactions reach it
                  -- too: its backlog lookup drops them, which is why no
                  -- filter sits here.
                  pure (STM.readTChan chan)
              }
          -- N2C: LocalChainSync (full blocks, no BlockFetch needed).
          -----------------------------------------------------------
          NodeToClient socketPath depth -> do
            syncState <- TxIdSyncN2C.emptyState
              TxIdSyncN2C.Config
                { TxIdSyncN2C.confirmationDepth = depth }
            let clients = N2C.emptyClients
                  { N2C.clientChainSync =
                      Just $ TxIdSyncN2C.chainSyncClient syncState
                  }
            pure Runtime.ObserverHandle
              { Runtime.ohRun =
                  Async.withAsync
                    (announceLoop (TxIdSyncN2C.stateBroadcast syncState)) $ \announceAsync -> do
                      -- Link the announce loop so its failure aborts the
                      -- observer instead of being silently swallowed.
                      Async.link announceAsync
                      result <- N2C.connect
                        ioManager codecConfig networkMagic tracers
                        socketPath clients
                      case result of
                        Left err ->
                          die $ "observer " ++ observerName ++ ": " ++ err
                        Right () -> pure ()
              , Runtime.ohSubscribe = do
                  chan <- STM.atomically $
                    STM.dupTChan (TxIdSyncN2C.stateBroadcast syncState)
                  -- Already refs, handed over unchanged, as in the N2N
                  -- adapter above.
                  pure (STM.readTChan chan)
              }

  -- The 'TargetWorker' callback (the last caller-supplied handler): run once
  -- per 'Target' by 'runWorkload'. Connects to the target node and drives the
  -- TxSubmission2 client with the two fetch actions. Takes the 'IOManager'
  -- first (partial-applied inside 'withIOManager' below), the same shape as
  -- 'mkObserver'.
  let targetWorker ioManager target fetchTx tryFetchTx = do
        addrInfo <- resolveAddr
          (Runtime.targetAddr target)
          (Runtime.targetPort target)
        keepAliveClient <- KeepAlive.keepAliveClient 10
        result <- N2N.connect ioManager codecConfig networkMagic tracers addrInfo
          N2N.emptyClients
            { N2N.clientKeepAlive = Just keepAliveClient
            , N2N.clientTxSubmission = Just $
                TxSubmission.txSubmissionClient
                  (Tracing.trTxSubmission tracers)
                  (Runtime.targetName target)
                  (Runtime.maxBatchSize target)
                  fetchTx tryFetchTx
            }
        case result of
          Left err -> die $ Runtime.targetName target ++ ": " ++ err
          Right () -> pure ()

  -- Start workloads.
  -------------------

  -- IOManager: no-op on POSIX, required on Windows for IOCP. All network I/O
  -- and cleanup must live inside this block as the handle is invalidated when
  -- 'withIOManager' returns.
  withIOManager $ \ioManager -> do
    -- Resolve runtime: creates observers (via mkObserver), pipes, rate
    -- limiters, and spawns builders. All asyncs are linked and tracked.
    -- The fund fold runs inside `resolve`, over the input queues it creates,
    -- so the fund file is read and closed before the run starts.
    runtime <- Runtime.resolve
      mkBuilder
      mkPipeHandle
      mkRecyclerHandle
      (mkObserver ioManager)
      foldInitialFunds
      validated
    -- Startup delay.
    -- Sleeps after the builders are already spawned and running so they keep
    -- filling the payload queues for the whole delay, while the workers below
    -- open their connections only after it elapses.
    let startupDelaySeconds = Validated.startupDelaySeconds validated
    when (startupDelaySeconds > 0) $ do
      hPutStrLn stderr $ "Startup delay: waiting " ++ show startupDelaySeconds
        ++ " second(s) (builders pre-filling queues)..."
      threadDelay (fromIntegral startupDelaySeconds * 1_000_000)
      hPutStrLn stderr "Startup delay complete, connecting to targets."
    -- For each 'Workload'.
    workers <- concat <$> mapM
      (\workload -> runWorkload workload (targetWorker ioManager))
      (Map.elems $ Runtime.workloads runtime)
    -- runWorkload returns unlinked asyncs; link them here so failures
    -- propagate to the main thread immediately.
    mapM_ Async.link workers
    -- All asyncs (builders and workers) are linked to the main thread and run
    -- forever. ANY completion, whether by exception or normal return, is fatal:
    -- either the pipeline starved ('QueueStarved'), a connection dropped, or a
    -- builder failed.
    --
    -- 'waitAnyCatch' returns as soon as the first async finishes (without
    -- re-throwing, so we keep control). 'finally cancelAll' then cancels every
    -- remaining async before the program exits.
    --
    -- 'Async.link' is still needed: if the main thread is blocked in
    -- 'waitAnyCatch' waiting on async A but async B dies, 'link' delivers the
    -- exception asynchronously, unblocking 'waitAnyCatch' immediately instead
    -- of waiting for A to finish first.
    let allAsyncs = Runtime.asyncs runtime ++ workers
        cancelAll = mapM_ Async.cancel allAsyncs
    -- One line per linked thread, id and label, read back from the thread
    -- itself: a thread's death is reported with its id alone (see 'main'), so
    -- this is what maps it to what it was, a workload's builder, recycler,
    -- target or observer bridge, or an observer itself. Printed once, nothing
    -- being spawned later, and ordered by thread id, which is what a reader
    -- arrives with.
    threads <- mapM
      (\async_ -> do
        let tid = Async.asyncThreadId async_
        label <- threadLabel tid
        pure (tid, fromMaybe "<unlabelled>" label)
      )
      allAsyncs
    forM_
      (sortOn fst threads)
      (\(tid, label) -> hPutStrLn stderr $ show tid ++ " = " ++ label)
    (_, result) <- flip finally cancelAll $
      Async.waitAnyCatch allAsyncs
    case result of
      Left ex ->
        die $ show ex
      Right () ->
        die "async terminated unexpectedly"

--------------------------------------------------------------------------------
-- Input source interpretation.
--------------------------------------------------------------------------------

-- | Interpreted input source: how to obtain UTxO funds. Referenced by
-- @initial_inputs@ (the startup load) and by builder recoveries (rebuilding the
-- queued inputs after a reset).
--
-- This type is node-specific (it references signing keys and sockets), so it
-- lives here rather than in the @pull-fiction@ sub-library, which stores the
-- source params as an opaque 'Aeson.Value' (see 'Raw.InputSource').
data FundSource
  -- | Query the UTxOs at addresses through a NodeToClient socket.
  = UTxOQuerySource !FilePath
  -- | Load funds from a genesis signing-keys file.
  | GenesisKeysSource !FilePath

-- | Interpret a 'Raw.InputSource' (opaque type + params) into a concrete
-- 'FundSource'.
interpretInputSource :: Raw.InputSource -> Either String FundSource
interpretInputSource raw = case Raw.inputSourceType raw of
  "utxo_query" ->
    case Aeson.Types.parseEither parseQuery (Raw.inputSourceParams raw) of
      Left  err -> Left $ "InputSource params error: " ++ err
      Right s   -> Right s
  "genesis_utxo_keys" ->
    case Aeson.Types.parseEither parseGenesis (Raw.inputSourceParams raw) of
      Left  err -> Left $ "InputSource params error: " ++ err
      Right s   -> Right s
  other -> Left $
    "InputSource: unknown \"type\" " ++ show other
    ++ ", expected \"utxo_query\" or \"genesis_utxo_keys\""
  where
    parseQuery = Aeson.withObject "UTxOQuery InputSourceParams" $ \o ->
      UTxOQuerySource <$> o .: "socket_path"
    parseGenesis = Aeson.withObject "GenesisKeys InputSourceParams" $ \o ->
      GenesisKeysSource <$> o .: "signing_keys_file"

-- | Interpret the opaque use-site @params@ of @initial_inputs@ for a
-- @utxo_query@ source: the signing key files. Each key's derived address is
-- queried, and every UTxO found there is tagged with that key so the spending
-- transaction can be signed.
parseInitialKeys :: Aeson.Value -> Aeson.Types.Parser [FilePath]
parseInitialKeys = Aeson.withObject "initial_inputs params" $ \o ->
  o .: "signing_keys"

-- | A fold over the initial funds: the @initial_inputs@ source, interpreted
-- and read. Announces what it is about to read and dies on a read error.
--
-- Boxed in a newtype because a @forall@ cannot be a tuple component, and this
-- travels in 'loadConfig'\'s result. It stays polymorphic because its
-- consumers pick different accumulators: 'Runtime.resolve' folds the funds
-- into the input queues, the dry run folds them into a count.
--
-- A fold and not a list: a fund file streams, so with millions of entries only
-- what the accumulator holds is held at once. The file is open for exactly as
-- long as the fold runs.
newtype InitialFunds = InitialFunds (Runtime.InputFold Fund.Fund)

--------------------------------------------------------------------------------
-- Builder interpretation.
--------------------------------------------------------------------------------

-- | Interpreted "value" builder configuration with defaults applied.
data ValueBuilder
  = ValueBuilder
    { inputsPerTx           :: !Natural
    , outputsPerTx          :: !Natural
    , fee                   :: !Integer
      -- | Non-empty. Output i pays to destination (i mod n), n being the length
      -- of this list, and its recycled fund keeps that key: multiple
      -- destinations let steady-state batches mix keys and stay multi-witness
      -- (typical, not guaranteed; the count follows the keys a batch happens to
      -- draw, see 'TxAssembly.buildTx').
    , destinations :: ![(Api.SigningKey Api.PaymentKey, Api.AddressInEra Era)]
    }

-- | Interpret a 'Raw.Builder' (opaque type + params) into a concrete
-- 'ValueBuilder'. Applies defaults (@inputs_per_tx@ = 1, @outputs_per_tx@ = 1),
-- validates invariants, and resolves the destination signing key and address.
--
-- Each builder pays to (and recycles under) its destination keys: the
-- @destination_signing_keys@ builder param (a list; outputs round-robin over
-- it) or the singular @destination_signing_key@ (setting both is an error),
-- otherwise a per-index built-in key. Addresses are derived from the keys.
interpretBuilder :: Api.NetworkId -> Int -> Raw.Builder -> IO ValueBuilder
interpretBuilder networkId builderIndex raw = case Raw.builderType raw of
  "value" ->
    case Aeson.Types.parseEither parseValueParams (Raw.builderParams raw) of
      Left err -> die $ "Builder params error: " ++ err
      Right (maybeInputs, maybeOutputs, rawFee, maybeDestPath, maybeDestPaths) -> do
        let nInputs  = fromMaybe 1 maybeInputs
            nOutputs = fromMaybe 1 maybeOutputs
        when (nInputs  == 0) $ die "Builder: inputs_per_tx must be >= 1"
        when (nOutputs == 0) $ die "Builder: outputs_per_tx must be >= 1"
        when (rawFee   <  0) $ die "Builder: fee must be >= 0"
        let readDestinations path = do
              eitherSkey <- Fund.readSigningKey path
              case eitherSkey of
                Left err ->
                  die $ "destination signing key (" ++ path ++ "): " ++ err
                Right skey -> pure (skey, Fund.deriveAddress networkId skey)
        dests <- case (maybeDestPath, maybeDestPaths) of
          (Just _, Just _) -> die $
            "Builder: set destination_signing_key or"
            ++ " destination_signing_keys, not both"
          (Nothing, Just []) ->
            die "Builder: destination_signing_keys must be non-empty"
          (Nothing, Just paths) -> mapM readDestinations paths
          (Just path, Nothing)  -> (:[]) <$> readDestinations path
          (Nothing, Nothing)    ->
            pure [createSigningKeyAndAddress networkId builderIndex]
        pure ValueBuilder
          { inputsPerTx  = nInputs
          , outputsPerTx = nOutputs
          , fee          = rawFee
          , destinations = dests
          }
  other -> die $
    "Builder: unknown type " ++ show other ++ ", expected \"value\""
  where
    parseValueParams = Aeson.withObject "ValueParams" $ \o ->
      (,,,,) <$> o .:? "inputs_per_tx"
             <*> o .:? "outputs_per_tx"
             <*> o .:  "fee"
             <*> o .:? "destination_signing_key"
             <*> o .:? "destination_signing_keys"

--------------------------------------------------------------------------------
-- Observer interpretation.
--------------------------------------------------------------------------------

-- | Interpreted observer.
data Observer
  -- | Chain follow via N2N ChainSync (headers) + BlockFetch (blocks).
  = NodeToNode   !String !Int !Natural
  -- | Chain follow via N2C LocalChainSync (full blocks, no BlockFetch needed).
  | NodeToClient !FilePath !Natural

-- | Interpret 'Raw.Observer' (opaque type + params) into a concrete 'Observer'.
interpretObserver :: Raw.Observer -> Either String Observer
interpretObserver raw = case Raw.observerType raw of
  "nodetonode" ->
    case Aeson.Types.parseEither parseN2N (Raw.observerParams raw) of
      Left  err -> Left $ "Observer params error: " ++ err
      Right o   -> Right o
  "nodetoclient" ->
    case Aeson.Types.parseEither parseN2C (Raw.observerParams raw) of
      Left  err -> Left $ "Observer params error: " ++ err
      Right o   -> Right o
  other -> Left $
    "Observer: unknown \"type\" " ++ show other
    ++ ", expected \"nodetonode\" or \"nodetoclient\""
  where
    parseN2N = Aeson.withObject "N2N ObserverParams" $ \o ->
      NodeToNode  <$> o .: "addr"
                  <*> o .: "port"
                  <*> o .: "confirmation_depth"
    parseN2C = Aeson.withObject "N2C ObserverParams" $ \o ->
      NodeToClient <$> o .: "socket_path"
                   <*> o .: "confirmation_depth"

--------------------------------------------------------------------------------
-- Signing key loading
--------------------------------------------------------------------------------

-- | Built-in fallback signing key and address for a builder index, used when a
-- builder has no 'destination_signing_key'. Builds the key from a hex string,
-- applying an integer suffix to the last 3 hex characters, and derives its
-- address via 'deriveAddress'.
createSigningKeyAndAddress
  :: Api.NetworkId
  -> Int
  -> (Api.SigningKey Api.PaymentKey, Api.AddressInEra Era)
createSigningKeyAndAddress networkId n
  | n < 0 || n > 999 =
    error $ "createSigningKeyAndAddress: out of range (0-999): " ++ show n
  | otherwise =
      let -- Hex string (32 bytes = 64 hex chars).
          -- We use 61 chars + 3 chars suffix = 64 chars total.
          -- If the input string is a CBOR-encoded hex string (e.g. from an
          -- .skey file), strip the first 4 characters ("5820") which represent
          -- the CBOR type and length prefix for 32 bytes of raw data.
          prefix = "bed03030fd08a600647d99fa7cd94dae3ddab99b199c3f08f81949db3e422"
          suffix = printf "%03d" n
          hex = prefix ++ suffix
          eitherSkey = Api.deserialiseFromRawBytesHex
                        @(Api.SigningKey Api.PaymentKey)
                        (BS8.pack hex)
      in case eitherSkey of
        Left err -> error $
                      "createSigningKeyAndAddress: Failed to deserialise: "
                      ++ show err
        Right signingKey ->
          (signingKey, Fund.deriveAddress networkId signingKey)

--------------------------------------------------------------------------------
-- Cardano parameters
--------------------------------------------------------------------------------

{-- TODO: Construct a minimal protocol parameters, see TxAssembly.hs last line.
data ProtocolParameters = ProtocolParameters
  { epochLength :: Integer
  , minFeeA     :: Integer
  , minFeeB     :: Integer
  } 

instance Aeson.FromJSON ProtocolParameters where
  parseJSON = Aeson.withObject "ProtocolParameters" $ \o -> do
    pp <- o .: "params"
    ProtocolParameters <$> pp .: "epoch_length" <*> pp .: "min_fee_a" <*> pp .: "min_fee_b"
--}

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Parse CLI args, load all configuration files, create protocol, generate a
-- signing key, interpret where the initial funds come from, and validate
-- config. The funds themselves are not read here: what comes back is the fold
-- that reads them, which 'Runtime.resolve' runs over the input queues.
--
-- Returns a 'Validated.Config' (validated but not yet resolved into a
-- 'Runtime.Runtime'). The caller is responsible for calling 'Runtime.resolve'
-- to create STM resources.
loadConfig
  :: IO ( -- | Whether this is a dry run: validate, then exit before traffic.
          Bool
          -- | Validated configuration (no STM resources yet).
        , Validated.Config
          -- | Folds over the initial funds: where they come from, interpreted
          -- and with its use-site params checked, read when folded.
        , InitialFunds
          -- | Codec config for serialising blocks on the wire.
        , CodecConfig Chain.CardanoBlock
          -- | Network id, for cardano-api: it decides the network bit of every
          -- address this generator derives, and so the pseudo-TxIn of a
          -- genesis fund. Only mainnet-or-not reaches an address; the magic
          -- itself never does.
        , Api.NetworkId
          -- | Network magic, for the handshake with cardano-node: the exact
          -- number, which is what the peer compares and refuses on.
          --
          -- Both come from one place, the magic in the node's genesis
          -- ('protocolToNetworkMagic'), and either derives the other
          -- ('Api.toNetworkMagic' one way, the mainnet-magic test in
          -- 'protocolToNetworkId' the other). They are carried as both so that
          -- no consumer has to convert: the fund and address code takes the
          -- id, the connections take the magic.
        , Api.NetworkMagic
          -- | Logging / metrics tracers.
        , Tracing.Tracers
        )
loadConfig = do
  args <- getArgs
  (isDryRun, configFile) <- case args of
    [cfg]              -> pure (False, cfg)
    ["--dry-run", cfg] -> pure (True, cfg)
    _                  -> die "Usage: tx-centrifuge [--dry-run] <config.json>"

  hPutStrLn stderr "=== Tx Centrifuge ==="
  hPutStrLn stderr ""

  -- Decode the full JSON object once; extract node-specific paths here (like
  -- setupTracers reads trace config from the same file independently) and pass
  -- the rest to the Raw → Validated → Runtime pipeline.
  hPutStrLn stderr $ "Loading config from: " ++ configFile
  rawValue <- Aeson.eitherDecodeFileStrict' configFile
    >>= either (\e -> die $ "JSON: " ++ e) pure
  let parseField field =
        case Aeson.Types.parseEither (Aeson.withObject "Config" (.: field)) rawValue of
          Left err -> die $ "Config: " ++ err
          Right v  -> pure v
  nodeConfigPath <- parseField "nodeConfig"
  raw <- case Aeson.fromJSON rawValue of
    Aeson.Error err   -> die $ "JSON: " ++ err
    Aeson.Success cfg -> pure cfg

  -- Load node configuration and create the consensus protocol first: the
  -- network id it yields is needed to load funds (both to derive query
  -- addresses and to open the LocalStateQuery connection).
  hPutStrLn stderr $ "Loading node config from: " ++ nodeConfigPath
  nodeConfig <- mkNodeConfig nodeConfigPath >>= either die pure
  protocol   <- mkConsensusProtocol nodeConfig >>= either die pure
  codecConfig  <- protocolToCodecConfig protocol
  networkId    <- protocolToNetworkId protocol
  networkMagic <- protocolToNetworkMagic protocol

  -- Where the initial funds come from. Look the initial_inputs source up in
  -- the raw config (validate also checks the reference, but the source is read
  -- here first, so a missing name dies here), interpret it and check its
  -- use-site params, all of it now, so a bad config dies at startup. No fund
  -- is read here: that happens when someone runs the fold, either
  -- 'Runtime.resolve' into the input queues or the dry run into a count.
  initialFunds <- do
    let sourceName  = Raw.initialInputsSource (Raw.initialInputs raw)
        maybeParams = Raw.initialInputsParams (Raw.initialInputs raw)
    rawSource <-
      case Map.lookup sourceName =<< Raw.maybeInputSources raw of
        Nothing  -> die $
          "initial_inputs: undefined input source " ++ show sourceName
        Just src -> pure src
    source <- case interpretInputSource rawSource of
      Left err  -> die $ "input_sources." ++ sourceName ++ ": " ++ err
      Right src -> pure src
    case source of
      -- The source is self-contained, reject use-site params instead of
      -- silently ignoring them.
      GenesisKeysSource path -> case maybeParams of
        Just _  -> die $
          "initial_inputs: a \"genesis_utxo_keys\" source takes no \"params\""
        Nothing -> pure (InitialFunds (\step seed -> do
          hPutStrLn stderr $ "Loading funds from: " ++ path
          Fund.foldFunds networkId path step seed
            >>= either (\err -> die $ "initial_inputs: " ++ err) pure))
      -- The source only knows how to query, the use site says what to query
      -- for, so the keys are read here and travel with it.
      UTxOQuerySource socketPath -> case maybeParams of
        Nothing -> die $
          "initial_inputs: a \"utxo_query\" source needs \"params\" with"
          ++ " the \"signing_keys\" to query under"
        Just params ->
          case Aeson.Types.parseEither parseInitialKeys params of
            Left err       -> die $ "initial_inputs params: " ++ err
            -- The query answers with every fund at once, so here the fold is
            -- a plain 'foldM' and the list is what is held.
            Right keyPaths -> pure (InitialFunds (\step seed -> do
              hPutStrLn stderr $
                "Querying UTxOs via N2C socket: " ++ socketPath
              funds <- Fund.discoverFunds networkId socketPath keyPaths
                >>= either (\err -> die $ "initial_inputs: " ++ err) pure
              foldM step seed funds))

  -- Validate config.
  -- Pipeline: Raw → Validated (static data, the funds are not part of it).
  validated <- either die pure $ Validated.validate raw

  -- Tracers.
  tracers <- TracingDispatcher.setupTracers configFile

  pure ( isDryRun, validated, initialFunds
       , codecConfig, networkId, networkMagic, tracers
       )

--------------------------------------------------------------------------------
-- Protocol helpers (inlined from NodeConfig.hs and OuroborosImports.hs)
--------------------------------------------------------------------------------

mkNodeConfig :: FilePath -> IO (Either String NodeConfiguration)
mkNodeConfig configFp_ = do
  configYamlPc <- parseNodeConfigurationFP . Just $ configFp
  pure $ first show $ makeNodeConfiguration (configYamlPc <> filesPc)
  where
    configFp = ConfigYamlFilePath configFp_
    filesPc :: PartialNodeConfiguration
    filesPc = defaultPartialNodeConfiguration
      { pncProtocolFiles = Last . Just $
          ProtocolFilepaths
          { byronCertFile        = Just ""
          , byronKeyFile         = Just ""
          , shelleyKESSource     = Just (KESKeyFilePath "")
          , shelleyVRFFile       = Just ""
          , shelleyCertFile      = Just ""
          , shelleyBulkCredsFile = Just ""
#ifdef LEIOS_PROTOTYPE
          -- A strict field and only on the "leios-prototype" branch, for the
          -- Leios BLS key. No released "cardano-node" has it, including 11.1.1,
          -- which is numbered above the 11.1.0.164 of that branch, so no
          -- version test finds it.
          , shelleyBLSFile       = Nothing
#endif
          }
      , pncShutdownConfig = Last $ Just $ ShutdownConfig Nothing Nothing
      , pncConfigFile = Last $ Just configFp
      }

mkConsensusProtocol
  :: NodeConfiguration -> IO (Either String SomeConsensusProtocol)
mkConsensusProtocol nodeConfig =
  case ncProtocolConfig nodeConfig of
    NodeProtocolConfigurationCardano
      byronCfg shelleyCfg alonzoCfg conwayCfg
      dijkstraCfg hardforkCfg checkpointsCfg ->
#if MIN_VERSION_cardano_node(11,1,0)
        -- "cardano-node" 11.1 returns the Shelley genesis hash alongside the
        -- protocol and the protocol is wanted here.
        first show . fmap fst <$>
#else
        first show <$>
#endif
          runExceptT (mkSomeConsensusProtocolCardano
            byronCfg shelleyCfg alonzoCfg conwayCfg
            dijkstraCfg hardforkCfg checkpointsCfg Nothing)

protocolToCodecConfig :: SomeConsensusProtocol -> IO (CodecConfig Chain.CardanoBlock)
protocolToCodecConfig (SomeConsensusProtocol Api.CardanoBlockType info) =
#if MIN_VERSION_ouroboros_consensus(4,0,0) || defined(LEIOS_PROTOTYPE)
    -- "ouroboros-consensus" 4 made `protocolInfo` monadic (so the KES agent
    -- can be consulted while building it). Below version 4 the result is pure.
    configCodec . pInfoConfig . fst <$> Api.protocolInfo @IO info
#else
    pure $ configCodec $ pInfoConfig $ fst $ Api.protocolInfo @IO info
#endif
protocolToCodecConfig _ =
  error "protocolToCodecConfig: non-Cardano protocol"

-- | Derive NetworkId from the consensus config. Mainnet uses a
-- well-known magic number; everything else is a testnet.
protocolToNetworkId :: SomeConsensusProtocol -> IO Api.NetworkId
protocolToNetworkId proto = do
  magic <- protocolToNetworkMagic proto
  pure $ case magic of
    Api.NetworkMagic 764824073 -> Api.Mainnet
    nm                         -> Api.Testnet nm

protocolToNetworkMagic :: SomeConsensusProtocol -> IO Api.NetworkMagic
protocolToNetworkMagic
  (SomeConsensusProtocol Api.CardanoBlockType info) =
#if MIN_VERSION_ouroboros_consensus(4,0,0) || defined(LEIOS_PROTOTYPE)
    getNetworkMagic . configBlock . pInfoConfig . fst
      <$> Api.protocolInfo @IO info
#else
    pure $ getNetworkMagic $ configBlock $ pInfoConfig $
      fst $ Api.protocolInfo @IO info
#endif
protocolToNetworkMagic _ =
  error "protocolToNetworkMagic: non-Cardano protocol"
