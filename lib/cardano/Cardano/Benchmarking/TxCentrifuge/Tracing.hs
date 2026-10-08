{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------------------------------------

-- | The trace events the tx-centrifuge emits, and the bundle of tracers that
-- carries them.
--
-- The events are plain data and the tracers are plain contra-tracers: this
-- module says what happened, never how it is rendered or where it goes. The
-- trace-dispatcher side (the @forMachine@ \/ @forHuman@ rendering, the
-- namespaces and the severities) is in the @cardano-tracing@ library,
-- module "Cardano.Benchmarking.TxCentrifuge.Tracing.Dispatcher", so
-- trace-dispatcher is not a dependency of this one.
module Cardano.Benchmarking.TxCentrifuge.Tracing
  ( -- * Tracer bundle.
    Tracers (..)
  , nullTracers
    -- * Traces.
  , BuilderTrace (..)
  , PipeTrace (..)
  , RecyclerTrace (..)
  , ObserverTrace (..)
  , TxSubmission (..)
    -- * Re-exports.
  , traceWith
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Numeric.Natural (Natural)
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
-------------------
-- contra-tracer --
-------------------
import "contra-tracer" Control.Tracer (Tracer, nullTracer, traceWith)
---------------------------------
-- ouroboros-consensus:cardano --
---------------------------------
import Ouroboros.Consensus.Cardano qualified as Consensus (CardanoBlock)
import Ouroboros.Consensus.Shelley.Eras qualified as Eras
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Mempool
---------------------------------
-- ouroboros-network:framework --
---------------------------------
import Ouroboros.Network.Driver.Simple qualified as Simple
---------------------------------
-- ouroboros-network:protocols --
---------------------------------
import Ouroboros.Network.Protocol.KeepAlive.Type qualified as KA
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as STX
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Fund qualified as Fund

--------------------------------------------------------------------------------
-- Tracers
--------------------------------------------------------------------------------

type CardanoBlock = Consensus.CardanoBlock Eras.StandardCrypto

data Tracers = Tracers
  { -- tx-centrifuge traces.
    ------------------------
    -- | Builder trace: new-transaction construction events.
    trBuilder :: !(Tracer IO BuilderTrace)
    -- | Pipe trace: payload/input queue add and remove events, with the
    -- relevant queue depth. Purely about queue mechanics.
  , trPipe :: !(Tracer IO PipeTrace)
    -- | Recycler trace: payloads added to the backlog ('AddToBacklog'),
    -- recycled inputs added back onto a pipe's input queue ('AddToPipe') and
    -- input queue resets ('Reset'). Every event emits the backlog size
    -- ('backlog'), adds counts at DDetailed and above, and the full inputs at
    -- DMaximum.
  , trRecycler :: !(Tracer IO RecyclerTrace)
    -- | Observer trace: on-chain transaction confirmation/rollback events.
  , trObserver :: !(Tracer IO ObserverTrace)
    -- | Clean, structured TxSubmission2 trace emitted by TxSubmission.hs.
  , trTxSubmission :: !(Tracer IO TxSubmission)
    -- ouroboros-network traces.
    ----------------------------
    -- | Low-level protocol trace from ouroboros-network's Driver.runPeer.
  , trTxSubmission2
      :: !( Tracer
              IO
              ( Simple.TraceSendRecv
                  ( STX.TxSubmission2
                      (Mempool.GenTxId CardanoBlock)
                      (Mempool.GenTx CardanoBlock)
                  )
              )
          )
    -- | Low-level protocol trace from ouroboros-network.
  , trKeepAlive
      :: !( Tracer
              IO
              (Simple.TraceSendRecv KA.KeepAlive)
          )

  }

-- | All-silent tracers.
nullTracers :: Tracers
nullTracers = Tracers
  { trBuilder       = nullTracer
  , trPipe          = nullTracer
  , trRecycler      = nullTracer
  , trObserver      = nullTracer
  , trTxSubmission  = nullTracer
  , trTxSubmission2 = nullTracer
  , trKeepAlive     = nullTracer
  }

--------------------------------------------------------------------------------
-- Builder trace messages
--------------------------------------------------------------------------------

-- | Trace messages emitted by the payload builder.
--
-- == Builder pipeline
--
-- The builder consumes input UTxOs (unspent funds) from the input queue, builds
-- and signs a transaction, and results are enqueued for workers to submit. Each
-- transaction produces new output UTxOs. After submission, these outputs can be
-- recycled at different points back to the input queue, forming a closed loop:
--
-- @
-- inputs --> [builder: build & sign tx] --> (tx, outputs) --> [do something]
--    ^                                                               |
--    +---------------------maybe recycle outputs --------------------+
-- @
--
-- == Cardano identifiers
--
-- The Cardano ledger uses a UTxO (Unspent Transaction Output) model. Every
-- transaction consumes existing UTxOs as /inputs/ and produces new UTxOs as
-- /outputs/. Three types from @cardano-api@ identify these objects:
--
-- === 'Api.TxId' — transaction identifier
--
-- A Blake2b-256 hash of the serialised transaction body ('Api.TxBody').
-- Uniquely identifies a transaction on the blockchain. Rendered as a
-- 64-character hex string via 'Api.serialiseToRawBytesHexText'.
--
-- === 'Api.TxIx' — output index
--
-- A zero-based index selecting one output within a transaction.
--
-- === 'Api.TxIn' — UTxO reference
--
-- A @('Api.TxId', 'Api.TxIx')@ pair that uniquely identifies a single UTxO on
-- the ledger. The standard display format is @\"\<hex-txid\>#\<ix\>\"@,
-- produced by 'Api.renderTxIn'.
--
-- A transaction's /input/ 'Api.TxIn's reference existing UTxOs being spent. Its
-- /output/ 'Api.TxIn's are derived from the new 'Api.TxId' paired with
-- sequential indices (0, 1, 2, ...).
--
-- In the tx-centrifuge, each 'Fund' record wraps a 'Api.TxIn' (the UTxO
-- reference), its Lovelace value, and the signing key needed to spend it.
data BuilderTrace
  = -- | A new transaction was built. This is purely about transaction
    -- construction and nothing about the pipe or the queues (see 'PipeTrace').
    --
    -- * 'String': builder name (the workload name).
    -- * 'Api.TxId': Blake2b-256 hash identifying the new transaction.
    --   Obtain via @'Api.getTxId' ('Api.getTxBody' signedTx)@.
    -- * @['Api.AddressInEra']@: the destination addresses this transaction
    --   pays to, round-robin by output index (the builder's own addresses,
    --   from @destination_signing_key(s)@). Rendered as bech32 only at
    --   @DMaximum@.
    -- * @['Fund.Fund']@ (inputs): funds consumed by this transaction. Each
    --   fund's 'Fund.fundTxIn' is a 'Api.TxIn' pointing to an existing UTxO
    --   on the ledger.
    -- * @['Fund.Fund']@ (outputs): funds produced by this transaction. Each
    --   fund's 'Fund.fundTxIn' is derived from the new 'Api.TxId' and a
    --   sequential 'Api.TxIx' index (0, 1, 2, ...).
    BuilderNewTx
      !String !Api.TxId ![Api.AddressInEra Api.ConwayEra] [Fund.Fund] [Fund.Fund]
  | -- | A dust batch was dropped: its total input value did not cover the fee,
    -- so @buildTx@ in "Cardano.Benchmarking.TxCentrifuge.TxAssembly" produced
    -- no valid change output. The inputs are abandoned (dropped from the
    -- builder loop, not recycled) and the service stays up.
    --
    -- * 'String': builder name (the workload name).
    -- * @['Fund.Fund']@: the dropped input funds.
    -- * 'String': the reason (the @buildTx@ error string).
    BuilderInputsDropped !String [Fund.Fund] !String

--------------------------------------------------------------------------------
-- Pipe trace messages
--------------------------------------------------------------------------------

-- | Pipe queue events. The pipe reports depth changes on its two queues as
-- items are added or removed but nothing about how a transaction was built or
-- confirmed. The 'String' names the queue the event happened on (the JSON key
-- is @\"pipe\"@): the input queue for @Inputs*@, the payload queue for
-- @Payload*@.
-- A payload queue belongs to one workload, so its name is that workload's. An
-- input queue may be shared: under @\"initial_inputs\"@
-- @\"scope\": \"shared\"@, the default, there is a single input queue named
-- @\@shared@ holding the inputs of every workload, and the
-- @Inputs*@ events carry that name and that queue's depth. Under
-- @\"per_workload\"@ each workload has its own, named after it, so both names
-- are the same.
data PipeTrace
  = -- | Inputs were added to the input queue. 'Natural' is the input-queue
    -- depth right after. @[Fund.Fund]@ is the inputs added (their count is
    -- emitted at @DDetailed@ and above, their full data at
    -- @DMaximum@).
    PipeInputsEnqueued  !String !Natural [Fund.Fund]
    -- | Inputs were removed from the input queue (taken by the builder).
    -- 'Natural' is the input-queue depth right after. @[Fund.Fund]@ is the
    -- inputs removed (count at @DDetailed@ and above, full data at
    -- @DMaximum@).
  | PipeInputsDequeued  !String !Natural [Fund.Fund]
    -- | A payload was added to the payload queue. 'Natural' is the
    -- payload-queue depth right after the write. 'Api.TxId' is the payload's
    -- tx id (emitted only at @DMaximum@).
  | PipePayloadEnqueued !String !Natural !Api.TxId
    -- | A payload was removed from the payload queue (pulled by a worker).
    -- 'Natural' is the payload-queue depth observed shortly after the pull
    -- (this is read outside the pull transaction, so under concurrency it is a
    -- close approximation, not an exact post-pull snapshot). 'Api.TxId' is the
    -- payload's tx id (emitted only at @DMaximum@).
  | PipePayloadDequeued !String !Natural !Api.TxId

--------------------------------------------------------------------------------
-- Recycler trace messages
--------------------------------------------------------------------------------

-- | Recycler events. The recycler holds a payload's recyclable input UTxOs
-- until a release picks a set, then adds it back onto a pipe's input queue
-- (closing the loop). It reports each add of a payload's entry to its
-- backlog ('RecyclerAddToBacklog': the key and both input sets), each add of
-- inputs to a pipe ('RecyclerAddToPipe': which inputs were added, by which
-- recycler, into which queue) and each reset of a pipe ('RecyclerReset'). The
-- 'String' names are the recycler and (on 'RecyclerAddToPipe' and
-- 'RecyclerReset') the input queue it recycles into, which is why they are
-- recorded separately: one recycler per workload, but the input queue can be
-- shared (see 'PipeTrace' for the names and @\"initial_inputs\"@ @\"scope\"@
-- for the scope). The 'Natural' is the resulting backlog size.
data RecyclerTrace
  = -- | A payload's entry was added to the backlog: its key and its two
    -- input sets, held until a release picks one. 'String' is the recycler
    -- name, 'Natural' the resulting backlog size, 'Api.TxId' the payload's
    -- key, first @[Fund.Fund]@ the consumed inputs, second the produced
    -- outputs (counts at @DDetailed@ and above, txId and full data
    -- at @DMaximum@).
    RecyclerAddToBacklog !String !Natural !Api.TxId [Fund.Fund] [Fund.Fund]
    -- | Recycled inputs were added back onto a pipe's input queue. First
    -- 'String' is the recycler name, second 'String' the input queue, 'Natural'
    -- the resulting backlog size, @[Fund.Fund]@ the added inputs (count at
    -- @DDetailed@ and above, full data at @DMaximum@).
  | RecyclerAddToPipe !String !String !Natural [Fund.Fund]
    -- | The recycler reset a pipe's input queue: the queued inputs were
    -- discarded, the payloads built from them were dropped from the payload
    -- queue, and the input queue was reseeded with fresh inputs (a builder's
    -- recovery). First 'String' is the recycler name, second 'String' the
    -- input queue, 'Natural' the resulting backlog size (always @0@, a reset
    -- clears the backlog), @[Api.TxId]@ the dropped queued payloads by txId,
    -- first @[Fund.Fund]@ the dropped inputs, second the fresh ones added to
    -- the queue (counts at @DDetailed@ and above, full data at
    -- @DMaximum@).
  | RecyclerReset     !String !String !Natural [Api.TxId] [Fund.Fund] [Fund.Fund]

--------------------------------------------------------------------------------
-- Observer trace messages
--------------------------------------------------------------------------------

-- | Observer events. Logged entirely in @Main.hs@ from the observer's on-chain
-- confirmation stream, decoupled from the pipe and from recycling.
data ObserverTrace
  = -- | The observer saw a transaction confirmed or orphaned (rolled back).
    --
    -- * 'String': observer name (from the config's @\"observers\"@ object).
    -- * 'Api.TxId': the confirmed/orphaned transaction's id.
    -- * 'Bool': @True@ if orphaned (rolled back), @False@ if confirmed.
    ObserverAnnounce !String !Api.TxId !Bool

--------------------------------------------------------------------------------
-- TxSubmission trace messages
--------------------------------------------------------------------------------

-- | Clean, structured trace of the TxSubmission2 protocol as seen from the
-- generator side. Replaces the verbose @Show@-based tracing in
-- @ouroboros-network@'s @TraceSendRecv@ with fields that are easy to parse and
-- verify.
--
-- Every constructor carries a @target@ field identifying the remote node: the
-- name of the target it was configured under.
data TxSubmission
  = -- | The node asked for transaction identifiers (@MsgRequestTxIds@).
    --
    -- * 'String':               target node name.
    -- * @['Api.TxId']@:         TxIds we have not yet received an ACK for.
    -- * 'Int':                  number of TxIds the node is acknowledging (ACK).
    -- * 'Int':                  number of new TxIds the node is requesting (REQ).
    RequestTxIds !String [Api.TxId] !Int !Int
    -- | We replied to @MsgRequestTxIds@ with TxId\/size pairs.
    --
    -- * 'String':               target node name.
    -- * 'Int':                  number of TxIds the node is acknowledging (ACK).
    -- * 'Int':                  number of new TxIds the node is requesting (REQ).
    -- * @['Api.TxId']@:         updated unacked TxIds (after ACK + new announcements).
    -- * @[('Api.TxId','Int')]@: TxIds we announced in this reply with its sizes in bytes.
  | ReplyTxIds   !String !Int !Int [Api.TxId] [(Api.TxId,Int)]
    -- | The node asked for full transactions by TxId (@MsgRequestTxs@).
    --
    -- * 'String':               target node name.
    -- * @['Api.TxId']@:         TxIds the node requested.
  | RequestTxs   !String [Api.TxId]
    -- | We replied to @MsgRequestTxs@ with the requested transactions.
    --
    -- * 'String':               target node name.
    -- * @['Api.TxId']@:         TxIds the node requested.
    -- * @[('Api.TxId','Int')]@: TxIds we actually sent (subset of requested; a
    --                           TxId is missing if it wasn't in the unacked list).
  | ReplyTxs     !String [Api.TxId] [(Api.TxId,Int)]

