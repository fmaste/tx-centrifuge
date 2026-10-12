{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
--- Every instance here is for a type declared elsewhere: the tx-centrifuge
--- events in "Cardano.Benchmarking.TxCentrifuge.Tracing", the protocol messages
--- in "ouroboros-network". Orphans are the point of this module.
{-# OPTIONS_GHC -Wno-orphans #-}

--------------------------------------------------------------------------------

-- | The trace-dispatcher side of the tx-centrifuge's tracing: how every trace
-- event is rendered, under which namespace and severity it is published, and
-- the setup that reads the config file.
--
-- Two kinds of event are rendered here, kept together so that there is one
-- place to look when a trace comes out wrong:
--
-- * The tx-centrifuge's own events, declared in
--   "Cardano.Benchmarking.TxCentrifuge.Tracing" together with the bundle of
--   contra-tracers the library writes them to.
--
-- * The "ouroboros-network" protocol messages (TxSubmission2 and KeepAlive),
--   whose instances are copied from @cardano-node@.
--
-- 'setupTracers' fills that bundle with trace-dispatcher tracers, one per
-- namespace. This library is the only one that depends on trace-dispatcher:
-- the events themselves are plain data, so nothing underneath has to know how
-- they are published.
module Cardano.Benchmarking.TxCentrifuge.Tracing.Dispatcher
  ( setupTracers
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Exception (SomeException, try)
-----------
-- aeson --
-----------
import Data.Aeson (Object, Value (String), (.=), object)
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
--------------------------
-- cardano-crypto-class --
--------------------------
import Cardano.Crypto.Hash qualified as Hash
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
-------------------
-- contra-tracer --
-------------------
-- `mkTracer` comes from the library in 0.2.1 and later. In 0.1 it does not
-- exist, so this module defines it below from the `Tracer` constructor.
-- Version 0.2.0.0 cannot be used at all: its constructor no longer accepts a
-- function and it has no `mkTracer` yet, so neither branch below compiles. The
-- cabal file forbids that version, so cabal never chooses it.
#if MIN_VERSION_contra_tracer(0,2,1)
import "contra-tracer" Control.Tracer (mkTracer)
#else
import "contra-tracer" Control.Tracer (Tracer (..))
#endif
---------------------------------
-- ouroboros-network:protocols --
---------------------------------
import Ouroboros.Network.Protocol.KeepAlive.Type qualified as KA
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as STX
#ifdef NETWORK_TRACING_MERGED
-------------------------------
-- ouroboros-network:tracing --
-------------------------------
#else
-----------------------------------------
-- ouroboros-network:framework-tracing --
-----------------------------------------
#endif
-- For the MetaTrace and LogFormatting instances of:
-- - Simple.TraceSendRecv
-- - Stateful.TraceSendRecv
-- The module `Ouroboros.Network.Tracing` is exposed by two different
-- sublibraries of the `ouroboros-network` package: `framework-tracing` in older
-- ones, `tracing` in newer ones. The version number does not tell you which.
-- `#if` above is only to have the correct sublibrary name in the comment.
-- Which sublibrary is used is decided in the cabal file, by the
-- `network-tracing-merged` flag, which also defines NETWORK_TRACING_MERGED.
import Ouroboros.Network.Tracing ()
----------
-- text --
----------
import Data.Text qualified as Text
----------------------
-- trace-dispatcher --
----------------------
import Cardano.Logging qualified as Logging
---------------------
-- typed-protocols --
---------------------
-- Unqualified to keep the copied instance heads below as they are in
-- cardano-node.
import Network.TypedProtocol.Codec (AnyMessage (AnyMessageAndAgency))
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Chain qualified as Chain
import Cardano.Benchmarking.TxCentrifuge.Fund qualified as Fund
-- Unqualified: the instances below read as if the events were declared here.
import Cardano.Benchmarking.TxCentrifuge.Tracing
  ( BuilderTrace (..)
  , ObserverTrace (..)
  , PipeTrace (..)
  , RecyclerTrace (..)
  , Tracers (..)
  , TxSubmission (..)
  )

--------------------------------------------------------------------------------
-- Tracer setup
--------------------------------------------------------------------------------

#if !MIN_VERSION_contra_tracer(0,2,1)
-- | Makes a `Tracer` from a function. Only needed for "contra-tracer" 0.1.
-- (A `Tracer m a` receives trace events. You make one from a function
-- `a -> m ()` that says what to do with each event).
--
-- In "contra-tracer" 0.1 the `Tracer` constructor takes that function, so this
-- is the constructor under another name. In 0.2 the constructor changed and no
-- longer accepts a function. 0.2.1 added its own `mkTracer` for this. Defining
-- `mkTracer` here for 0.1 keeps the code that builds tracers the same for every
-- version.
--
-- The type has no `Applicative m` constraint (unlike the `mkTracer` in 0.2.1):
-- the 0.1 constructor does not need it and an unused constraint is a warning,
-- which `-Werror` will turn it into a build failure.
mkTracer :: (a -> m ()) -> Tracer m a
mkTracer = Tracer
#endif

-- | Create configured tracers from the tx-centrifuge config file. If the file
-- contains a @TraceOptions@ section, those settings are used. Otherwise falls
-- back to a sensible default (stdout, machine format, severity Debug).
setupTracers :: FilePath -> IO Tracers
setupTracers configFile = do
#if MIN_VERSION_trace_dispatcher(2,13,0)
  -- 2.13 reads from a ConfigSource rather than straight from a path.
  let configSource = Logging.FromFile configFile
#else
  let configSource = configFile
#endif
  -- Read it. The annotation is what tells `try` which exceptions to catch; on
  -- a handler's pattern it would need ScopedTypeVariables.
  eitherTraceConfig <- try (Logging.readConfiguration configSource)
    :: IO (Either SomeException Logging.TraceConfig)
  -- A file that is missing, or that does not parse: use the default.
  let trConfig = case eitherTraceConfig of
        Left  _exception -> defaultTraceConfig
        Right fileConfig -> fileConfig
  configReflection <- Logging.emptyConfigReflection
  stdoutTrace      <- Logging.standardTracer
  let trForward = mempty
      mbTrEkg   = Nothing
  -- tx-centrifuge traces.
  ------------------------
  -- Builder (TxCentrifuge.Builder.NewTx).
  !builderTr <-    Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxCentrifuge", "Builder"]
  Logging.configureTracers configReflection trConfig [builderTr]
  -- Pipe (TxCentrifuge.Pipe.*).
  !pipeTr <-       Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxCentrifuge", "Pipe"]
  Logging.configureTracers configReflection trConfig [pipeTr]
  -- Recycler (TxCentrifuge.Recycler.*).
  !recyclerTr <-   Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxCentrifuge", "Recycler"]
  Logging.configureTracers configReflection trConfig [recyclerTr]
  -- Observer (TxCentrifuge.Observer.Announce).
  !observerTr <-   Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxCentrifuge", "Observer"]
  Logging.configureTracers configReflection trConfig [observerTr]
  -- TxSubmission (TxCentrifuge.TxSubmission.*).
  !txSubTraceTr <- Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxCentrifuge", "TxSubmission"]
  Logging.configureTracers configReflection trConfig [txSubTraceTr]
  -- ouroboros-network traces.
  ----------------------------
  -- TxSubmission2 (low-level protocol trace).
  !txSub2Trace <-  Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxSubmission2"]
  Logging.configureTracers configReflection trConfig [txSub2Trace]
  -- KeepAlive.
  !keepAliveTr <-  Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["KeepAlive"]
  Logging.configureTracers configReflection trConfig [keepAliveTr]
  pure Tracers
    { trBuilder       = mkTracer $ Logging.traceWith builderTr
    , trPipe          = mkTracer $ Logging.traceWith pipeTr
    , trRecycler      = mkTracer $ Logging.traceWith recyclerTr
    , trObserver      = mkTracer $ Logging.traceWith observerTr
    , trTxSubmission  = mkTracer $ Logging.traceWith txSubTraceTr
    , trTxSubmission2 = mkTracer $ Logging.traceWith txSub2Trace
    , trKeepAlive     = mkTracer $ Logging.traceWith keepAliveTr
    }

-- | Default config: stdout machine format, severity Debug for all namespaces.
defaultTraceConfig :: Logging.TraceConfig
defaultTraceConfig = Logging.emptyTraceConfig
  { Logging.tcOptions = Map.fromList
      [ ( []
        , [ Logging.ConfSeverity
              (Logging.SeverityF (Just Logging.Debug))
          , Logging.ConfBackend
              [Logging.Stdout Logging.MachineFormat]
          ]
        )
      ]
  }

--------------------------------------------------------------------------------
-- Builder trace rendering
--------------------------------------------------------------------------------

-- | Namespace: @TxCentrifuge.Builder.NewTx@. The outer prefix
-- @[\"TxCentrifuge\", \"Builder\"]@ is set when creating the tracer via
-- 'Logging.mkCardanoTracer' in 'setupTracers'.
instance Logging.MetaTrace BuilderTrace where
  namespaceFor BuilderNewTx{} = Logging.Namespace [] ["NewTx"]
  namespaceFor BuilderInputsDropped{} = Logging.Namespace [] ["InputsDropped"]
  severityFor (Logging.Namespace _ ["NewTx"]) _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["InputsDropped"]) _ = Just Logging.Warning
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["NewTx"]) = Just
    "A new transaction was built from input UTxOs, producing output UTxOs."
  documentFor (Logging.Namespace _ ["InputsDropped"]) = Just
    "A dust batch was dropped: its total input value did not cover the fee, so no valid change output could be produced. The inputs are abandoned, not recycled."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["NewTx"]
    , Logging.Namespace [] ["InputsDropped"]
    ]

-- | Machine-readable ('forMachine') and human-readable ('forHuman') rendering
-- of 'BuilderTrace' messages.
--
-- Machine format ('Logging.DNormal'): both of the transaction's names, the
-- @txId@ that a ranking block's body gives it and the @txHash@ that an
-- endorser block lists it under.
--
-- @
-- { \"builder\": \"workload-name\"
-- , \"txId\": \"\<64-char hex\>\"
-- , \"txHash\": \"\<64-char hex\>\"
-- }
-- @
--
-- Machine format ('Logging.DDetailed'): adds the consumed and produced UTxO
-- references as strings.
--
-- @
-- { \"builder\": \"workload-name\"
-- , \"txId\": \"\<64-char hex\>\"
-- , \"txHash\": \"\<64-char hex\>\"
-- , \"inputs\": [\"\<txid\>#\<ix\>\", ...]
-- , \"outputs\": [\"\<txid\>#\<ix\>\", ...]
-- }
-- @
--
-- Machine format ('Logging.DMaximum'): renders each fund in full (a JSON
-- object with its @\"utxo\"@ reference and @\"lovelace\"@ value) and adds the
-- @\"destinations\"@ addresses the transaction pays to (round-robin by
-- output index).
--
-- @
-- { \"builder\": \"workload-name\"
-- , \"txId\": \"\<64-char hex\>\"
-- , \"txHash\": \"\<64-char hex\>\"
-- , \"destinations\": [\"addr...\", ...]
-- , \"inputs\": [{\"utxo\": \"\<txid\>#\<ix\>\", \"lovelace\": 1000000}, ...]
-- , \"outputs\": [{\"utxo\": \"\<txid\>#\<ix\>\", \"lovelace\": 500000}, ...]
-- }
-- @
--
-- Human format:
--
-- @
-- NewTx [workload-name] \<txid\> inputs=[...] outputs=[...]
-- @
instance Logging.LogFormatting BuilderTrace where
  forMachine dtal (BuilderNewTx name txKey dests inputs outputs) = mconcat $
       [ "builder" .= name
       , renderTxKey txKey
       ]
    ++ [ "destinations" .= map Api.serialiseAddress dests
       | dtal >= Logging.DMaximum
       ]
    ++ [ "inputs"  .= map (renderFund dtal) inputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "outputs" .= map (renderFund dtal) outputs
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (BuilderInputsDropped name inputs reason) = mconcat $
       [ "builder" .= name
       , "reason"  .= reason
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DDetailed
       ]
  forHuman (BuilderNewTx name txKey _dest inputs outputs) =
       "NewTx [" <> Text.pack name <> "] "
    <> Hash.hashToTextAsHex (Chain.txKeyHash txKey)
    <> " inputs=["  <> renderFundTxIns inputs  <> "]"
    <> " outputs=[" <> renderFundTxIns outputs <> "]"
  forHuman (BuilderInputsDropped name inputs reason) =
       "InputsDropped [" <> Text.pack name <> "] "
    <> "reason=" <> Text.pack reason
    <> " inputs=[" <> renderFundTxIns inputs <> "]"

-- | Render a payload key as machine-format fields: @txHash@ always, @txId@
-- when the key has one (see 'Chain.TxKey'). Both are 32 bytes of hex, so the
-- field is the only thing that says which is which.
renderTxKey :: Chain.TxKey -> Object
renderTxKey txKey = mconcat $
     [ "txHash" .= String (Hash.hashToTextAsHex (Chain.txKeyHash txKey)) ]
  ++ [ "txId"   .= String (Api.serialiseToRawBytesHexText txId)
     | Just txId <- [Chain.txKeyId txKey]
     ]

-- | Render a single fund for 'forMachine' output.
--
-- * Below 'Logging.DMaximum': just the UTxO reference as a string
--   (@\"\<txid\>#\<ix\>\"@).
-- * 'Logging.DMaximum': a JSON object with @\"utxo\"@ and @\"lovelace\"@ fields.
renderFund :: Logging.DetailLevel -> Fund.Fund -> Value
renderFund dtal fund
  | dtal >= Logging.DMaximum =
      object [ "utxo"     .= Api.renderTxIn (Fund.fundTxIn fund)
             , "lovelace" .= Fund.fundLovelace fund
             ]
  | otherwise =
      String (Api.renderTxIn (Fund.fundTxIn fund))

-- | Render a list of funds as comma-separated @\"\<txid\>#\<ix\>\"@ references.
renderFundTxIns :: [Fund.Fund] -> Text.Text
renderFundTxIns = Text.intercalate "," . map (Api.renderTxIn . Fund.fundTxIn)

--------------------------------------------------------------------------------
-- Pipe trace rendering
--------------------------------------------------------------------------------

-- | Namespaces: @TxCentrifuge.Pipe.{InputsEnqueued, InputsDequeued,
-- PayloadEnqueued, PayloadDequeued}@. Outer prefix
-- @[\"TxCentrifuge\", \"Pipe\"]@ is set in 'setupTracers'.
instance Logging.MetaTrace PipeTrace where
  namespaceFor PipeInputsEnqueued{}  = Logging.Namespace [] ["InputsEnqueued"]
  namespaceFor PipeInputsDequeued{}  = Logging.Namespace [] ["InputsDequeued"]
  namespaceFor PipePayloadEnqueued{} = Logging.Namespace [] ["PayloadEnqueued"]
  namespaceFor PipePayloadDequeued{} = Logging.Namespace [] ["PayloadDequeued"]
  severityFor (Logging.Namespace _ ["InputsEnqueued"])  _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["InputsDequeued"])  _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["PayloadEnqueued"]) _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["PayloadDequeued"]) _ = Just Logging.Info
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["InputsEnqueued"]) = Just
    "Inputs were added to the input queue (count at DDetailed, data at DMaximum)."
  documentFor (Logging.Namespace _ ["InputsDequeued"]) = Just
    "Inputs were removed from the input queue for the builder (count at DDetailed, data at DMaximum)."
  documentFor (Logging.Namespace _ ["PayloadEnqueued"]) = Just
    "A payload was added to the payload queue (its txId is included at DMaximum)."
  documentFor (Logging.Namespace _ ["PayloadDequeued"]) = Just
    "A payload was removed from the payload queue for a worker (txId at DMaximum)."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["InputsEnqueued"]
    , Logging.Namespace [] ["InputsDequeued"]
    , Logging.Namespace [] ["PayloadEnqueued"]
    , Logging.Namespace [] ["PayloadDequeued"]
    ]

-- | Machine format: every Pipe event always carries @pipe@ + @depth@ (the
-- namespace already says which queue). Input events add @count@ at
-- 'Logging.DDetailed' and above and an @inputs@ array of rendered funds at
-- 'Logging.DMaximum' (see 'renderFund'). Payload events add the payload's key
-- at 'Logging.DMaximum' (see 'renderTxKey' for the fields it lands in). Human
-- format shows just the depth (and, for inputs, the count).
instance Logging.LogFormatting PipeTrace where
  forMachine dtal (PipeInputsEnqueued name depth inputs) = mconcat $
       [ "pipe"  .= name
       , "depth" .= depth
       ]
    ++ [ "count"  .= length inputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (PipeInputsDequeued name depth inputs) = mconcat $
       [ "pipe"  .= name
       , "depth" .= depth
       ]
    ++ [ "count"  .= length inputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (PipePayloadEnqueued name depth txKey) = mconcat $
       [ "pipe"  .= name
       , "depth" .= depth
       ]
    ++ [ renderTxKey txKey
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (PipePayloadDequeued name depth txKey) = mconcat $
       [ "pipe"  .= name
       , "depth" .= depth
       ]
    ++ [ renderTxKey txKey
       | dtal >= Logging.DMaximum
       ]
  forHuman (PipeInputsEnqueued name depth inputs) =
       "InputsEnqueued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)
    <> " count=" <> Text.pack (show (length inputs))
  forHuman (PipeInputsDequeued name depth inputs) =
       "InputsDequeued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)
    <> " count=" <> Text.pack (show (length inputs))
  forHuman (PipePayloadEnqueued name depth _txKey) =
       "PayloadEnqueued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)
  forHuman (PipePayloadDequeued name depth _txKey) =
       "PayloadDequeued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)

--------------------------------------------------------------------------------
-- Recycler trace rendering
--------------------------------------------------------------------------------

-- | Namespaces: @TxCentrifuge.Recycler.{AddToBacklog, AddToPipe, Reset}@.
-- Outer prefix @[\"TxCentrifuge\", \"Recycler\"]@ is set in 'setupTracers'.
instance Logging.MetaTrace RecyclerTrace where
  namespaceFor RecyclerAddToBacklog{} = Logging.Namespace [] ["AddToBacklog"]
  namespaceFor RecyclerAddToPipe{}    = Logging.Namespace [] ["AddToPipe"]
  namespaceFor RecyclerReset{}        = Logging.Namespace [] ["Reset"]
  severityFor (Logging.Namespace _ ["AddToBacklog"]) _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["AddToPipe"])    _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["Reset"])        _ = Just Logging.Warning
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["AddToBacklog"]) = Just $
    "The recycler added a payload's entry to its backlog: the key and both "
    <> "input sets, held until a release picks one. The backlog count is "
    <> "the number of held payloads, not a pipe queue depth."
  documentFor (Logging.Namespace _ ["AddToPipe"]) = Just
    "The recycler added a payload's recycled inputs back onto a pipe's input queue (count at DDetailed, full inputs at DMaximum)."
  documentFor (Logging.Namespace _ ["Reset"]) = Just $
    "The recycler reset a pipe for a builder's recovery: the queued inputs "
    <> "and the queued payloads built from them were dropped, and the input "
    <> "queue was reseeded with fresh inputs (counts at DDetailed, full "
    <> "inputs at DMaximum)."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["AddToBacklog"]
    , Logging.Namespace [] ["AddToPipe"]
    , Logging.Namespace [] ["Reset"]
    ]

-- | Machine format: every event emits @recycler@ and @backlog@ (the
-- resulting backlog size, not a queue depth). 'RecyclerAddToBacklog'
-- carries the added entry: @consumed_count@ and @outputs_count@ at
-- 'Logging.DDetailed', the payload's key (see 'renderTxKey') and the
-- @consumed@ and
-- @outputs@ fund arrays (see 'renderFund') at 'Logging.DMaximum'.
-- 'RecyclerAddToPipe' (which also emits @pipe@) adds a @count@ at
-- 'Logging.DDetailed' and an @inputs@ array of the recycled funds at
-- 'Logging.DMaximum'. 'RecyclerReset' (also with @pipe@) adds @count@ for
-- the fresh inputs, @dropped_inputs_count@ and @dropped_payloads_count@ at
-- 'Logging.DDetailed', plus the @inputs@ (fresh), @dropped_inputs@ and
-- @dropped_payloads@ (payload keys as hex) arrays at 'Logging.DMaximum'.
-- Human format shows the backlog size and the counts.
instance Logging.LogFormatting RecyclerTrace where
  forMachine dtal (RecyclerAddToBacklog recyclerName backlog txKey consumed outputs) =
    mconcat $
       [ "recycler" .= recyclerName
       , "backlog"  .= backlog
       ]
    ++ [ "consumed_count" .= length consumed
       | dtal >= Logging.DDetailed
       ]
    ++ [ "outputs_count" .= length outputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ renderTxKey txKey
       | dtal >= Logging.DMaximum
       ]
    ++ [ "consumed" .= map (renderFund dtal) consumed
       | dtal >= Logging.DMaximum
       ]
    ++ [ "outputs" .= map (renderFund dtal) outputs
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (RecyclerAddToPipe recyclerName pipeName backlog inputs) = mconcat $
       [ "recycler" .= recyclerName
       , "pipe"     .= pipeName
       , "backlog"  .= backlog
       ]
    ++ [ "count"  .= length inputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (RecyclerReset recyclerName pipeName backlog droppedPayloads droppedInputs fresh) =
    mconcat $
       [ "recycler" .= recyclerName
       , "pipe"     .= pipeName
       , "backlog"  .= backlog
       ]
    ++ [ "count" .= length fresh
       | dtal >= Logging.DDetailed
       ]
    ++ [ "dropped_inputs_count" .= length droppedInputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "dropped_payloads_count" .= length droppedPayloads
       | dtal >= Logging.DDetailed
       ]
    ++ [ "inputs" .= map (renderFund dtal) fresh
       | dtal >= Logging.DMaximum
       ]
    ++ [ "dropped_inputs" .= map (renderFund dtal) droppedInputs
       | dtal >= Logging.DMaximum
       ]
    ++ [ "dropped_payloads" .=
           map (String . Hash.hashToTextAsHex . Chain.txKeyHash) droppedPayloads
       | dtal >= Logging.DMaximum
       ]
  forHuman (RecyclerAddToBacklog recyclerName backlog _txKey consumed outputs) =
       "AddToBacklog [" <> Text.pack recyclerName <> "]"
    <> " backlog=" <> Text.pack (show backlog)
    <> " consumed_count=" <> Text.pack (show (length consumed))
    <> " outputs_count=" <> Text.pack (show (length outputs))
  forHuman (RecyclerAddToPipe recyclerName pipeName backlog inputs) =
       "AddToPipe [" <> Text.pack recyclerName <> "]"
    <> " pipe=" <> Text.pack pipeName
    <> " backlog=" <> Text.pack (show backlog)
    <> " count=" <> Text.pack (show (length inputs))
  forHuman (RecyclerReset recyclerName pipeName backlog droppedPayloads droppedInputs fresh) =
       "Reset [" <> Text.pack recyclerName <> "]"
    <> " pipe=" <> Text.pack pipeName
    <> " backlog=" <> Text.pack (show backlog)
    <> " count=" <> Text.pack (show (length fresh))
    <> " dropped_inputs_count=" <> Text.pack (show (length droppedInputs))
    <> " dropped_payloads_count=" <> Text.pack (show (length droppedPayloads))

--------------------------------------------------------------------------------
-- Observer trace rendering
--------------------------------------------------------------------------------

-- | Namespace: @TxCentrifuge.Observer.Announce@. Outer prefix
-- @[\"TxCentrifuge\", \"Observer\"]@ is set in 'setupTracers'.
instance Logging.MetaTrace ObserverTrace where
  namespaceFor ObserverAnnounce{} = Logging.Namespace [] ["Announce"]
  severityFor (Logging.Namespace _ ["Announce"]) _ = Just Logging.Info
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["Announce"]) = Just
    "The observer saw a transaction confirmed or orphaned (rolled back)."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["Announce"]
    ]

instance Logging.LogFormatting ObserverTrace where
  forMachine _ (ObserverAnnounce observer txKey isOrphan) = mconcat
    [ "observer" .= observer
    , renderTxKey txKey
    , "isOrphan" .= isOrphan
    ]
  forHuman (ObserverAnnounce observer txKey isOrphan) =
       "Announce [" <> Text.pack observer <> "]"
    <> " txHash=" <> Hash.hashToTextAsHex (Chain.txKeyHash txKey)
    <> (if isOrphan then " (orphan)" else " (confirmed)")

--------------------------------------------------------------------------------
-- TxSubmission trace rendering
--------------------------------------------------------------------------------

-- | Namespace: @TxCentrifuge.TxSubmission.*@. The outer prefix is set via
-- 'Logging.mkCardanoTracer' in 'setupTracers'.
instance Logging.MetaTrace TxSubmission where
  namespaceFor RequestTxIds{} = Logging.Namespace [] ["RequestTxIds"]
  namespaceFor ReplyTxIds{}   = Logging.Namespace [] ["ReplyTxIds"]
  namespaceFor RequestTxs{}   = Logging.Namespace [] ["RequestTxs"]
  namespaceFor ReplyTxs{}     = Logging.Namespace [] ["ReplyTxs"]
  severityFor (Logging.Namespace _ ["RequestTxIds"]) _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["ReplyTxIds"])   _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["RequestTxs"])   _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["ReplyTxs"])     _ = Just Logging.Info
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["RequestTxIds"]) = Just
    "Node requested tx id announcements (blocking or non-blocking)."
  documentFor (Logging.Namespace _ ["ReplyTxIds"]) = Just
    "We replied with tx id announcements and sizes."
  documentFor (Logging.Namespace _ ["RequestTxs"]) = Just
    "Node requested full transactions by TxId."
  documentFor (Logging.Namespace _ ["ReplyTxs"]) = Just
    "We sent the requested transactions."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["RequestTxIds"]
    , Logging.Namespace [] ["ReplyTxIds"]
    , Logging.Namespace [] ["RequestTxs"]
    , Logging.Namespace [] ["ReplyTxs"]
    ]

-- | Machine-readable and human-readable rendering. All TxId lists are omitted
-- below 'Logging.DDetailed' to avoid the cost of hex-encoding every transaction
-- identifier on every protocol round-trip.
--
-- Machine format ('Logging.DNormal'):
--
-- @
-- { \"target\": \"n\", \"ack\": 0, \"req\": 3 }
-- { \"target\": \"n\" }
-- { \"target\": \"n\" }
-- { \"target\": \"n\" }
-- @
--
-- Machine format ('Logging.DDetailed' and above):
--
-- @
-- { \"target\": \"n\", \"ack\": 0, \"req\": 3, \"unacked\": [\"ab..\"] }
-- { \"target\": \"n\", \"ack\": 0, \"req\": 3, \"txs\": [{\"id\":\"ab..\",\"size\":9}], \"unacked\": [\"ab..\"] }
-- { \"target\": \"n\", \"txIds\": [\"ab..\"] }
-- { \"target\": \"n\", \"txs\": [{\"id\":\"ab..\",\"size\":9}], \"requested\": [\"ab..\"] }
-- @
instance Logging.LogFormatting TxSubmission where
  forMachine dtal (RequestTxIds target unacked ack req) = mconcat $
       [ "target"    .= target
       , "ack"       .= ack
       , "req"       .= req
       ]
    ++ [ "unacked"   .= map Api.serialiseToRawBytesHexText unacked
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (ReplyTxIds target ack req unacked announced) = mconcat $
       [ "target"    .= target ]
    ++ [ "ack"       .= ack
       | dtal >= Logging.DDetailed
       ]
    ++ [ "req"       .= req
       | dtal >= Logging.DDetailed
       ]
    ++ [ "txs"       .= map
                          (\(txId,txSize) ->
                            object
                              [ "id"   .= Api.serialiseToRawBytesHexText txId
                              , "size" .= txSize
                              ]
                          )
                          announced
       | dtal >= Logging.DDetailed
       ]
    ++ [ "unacked"   .= map Api.serialiseToRawBytesHexText unacked
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (RequestTxs target txIds) = mconcat $
       [ "target"    .= target ]
    ++ [ "txIds"     .= map Api.serialiseToRawBytesHexText txIds
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (ReplyTxs target requested sent) = mconcat $
       [ "target"    .= target ]
    ++ [ "txs"       .= map
                          (\(txId,txSize) ->
                            object
                              [ "id"   .= Api.serialiseToRawBytesHexText txId
                              , "size" .= txSize
                              ]
                          )
                          sent
       | dtal >= Logging.DDetailed
       ]
    ++ [ "requested" .= map Api.serialiseToRawBytesHexText requested
       | dtal >= Logging.DDetailed
       ]
  forHuman (RequestTxIds target _unacked ack req) =
       "RequestTxIds [" <> Text.pack target <> "]"
    <> " ack=" <> Text.pack (show ack)
    <> " req=" <> Text.pack (show req)
  forHuman (ReplyTxIds target ack req _unacked _announced) =
       "ReplyTxIds [" <> Text.pack target <> "]"
    <> " ack=" <> Text.pack (show ack)
    <> " req=" <> Text.pack (show req)
  forHuman (RequestTxs target _txIds) =
       "RequestTxs [" <> Text.pack target <> "]"
  forHuman (ReplyTxs target _requested _sent) =
       "ReplyTxs [" <> Text.pack target <> "]"

--------------------------------------------------------------------------------
-- TxSubmission2 protocol rendering
--------------------------------------------------------------------------------

-- Instances copied from cardano-node's "NodeToNode.hs".

instance (Show txid, Show tx)
      => Logging.LogFormatting (AnyMessage (STX.TxSubmission2 txid tx)) where
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgInit) =
    mconcat
      [ "kind" .= String "MsgInit"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgRequestTxIds {}) =
    mconcat
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxIds _)) =
    mconcat
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgRequestTxs txids)) =
    mconcat
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (Text.pack $ show stok)
      , "txIds" .= String (Text.pack $ show txids)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxs txs)) =
    mconcat
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (Text.pack $ show stok)
      , "txs" .= String (Text.pack $ show txs)
      ]
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgDone) =
    mconcat
      [ "kind" .= String "MsgDone"
      , "agency" .= String (Text.pack $ show stok)
      ]

instance Logging.MetaTrace (AnyMessage (STX.TxSubmission2 txid tx)) where
    namespaceFor (AnyMessageAndAgency _stok STX.MsgInit {}) =
      Logging.Namespace [] ["MsgInit"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgRequestTxIds {}) =
      Logging.Namespace [] ["RequestTxIds"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgReplyTxIds {}) =
      Logging.Namespace [] ["ReplyTxIds"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgRequestTxs {}) =
      Logging.Namespace [] ["RequestTxs"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgReplyTxs {}) =
      Logging.Namespace [] ["ReplyTxs"]
    namespaceFor (AnyMessageAndAgency _stok STX.MsgDone {}) =
      Logging.Namespace [] ["Done"]

    severityFor (Logging.Namespace _ ["MsgInit"]) _ = Just Logging.Debug
    severityFor (Logging.Namespace _ ["RequestTxIds"]) _ = Just Logging.Debug
    severityFor (Logging.Namespace _ ["ReplyTxIds"]) _ = Just Logging.Debug
    severityFor (Logging.Namespace _ ["RequestTxs"]) _ = Just Logging.Debug
    severityFor (Logging.Namespace _ ["ReplyTxs"]) _ = Just Logging.Debug
    severityFor (Logging.Namespace _ ["Done"]) _ = Just Logging.Debug
    severityFor _ _ = Nothing

    documentFor (Logging.Namespace _ ["MsgInit"]) = Just
        "Client side hello message."
    documentFor (Logging.Namespace _ ["RequestTxIds"]) = Just $ mconcat
      [ "Request a non-empty list of transaction identifiers from the client, "
      , "and confirm a number of outstanding transaction identifiers. "
      , "\n "
      , "With 'TokBlocking' this is a a blocking operation: the response will "
      , "always have at least one transaction identifier, and it does not expect "
      , "a prompt response: there is no timeout. This covers the case when there "
      , "is nothing else to do but wait. For example this covers leaf nodes that "
      , "rarely, if ever, create and submit a transaction. "
      , "\n "
      , "With 'TokNonBlocking' this is a non-blocking operation: the response "
      , "may be an empty list and this does expect a prompt response. This "
      , "covers high throughput use cases where we wish to pipeline, by "
      , "interleaving requests for additional transaction identifiers with "
      , "requests for transactions, which requires these requests not block. "
      , "\n "
      , "The request gives the maximum number of transaction identifiers that "
      , "can be accepted in the response. This must be greater than zero in the "
      , "'TokBlocking' case. In the 'TokNonBlocking' case either the numbers "
      , "acknowledged or the number requested must be non-zero. In either case, "
      , "the number requested must not put the total outstanding over the fixed "
      , "protocol limit. "
      , "\n"
      , "The request also gives the number of outstanding transaction "
      , "identifiers that can now be acknowledged. The actual transactions "
      , "to acknowledge are known to the peer based on the FIFO order in which "
      , "they were provided. "
      , "\n "
      , "There is no choice about when to use the blocking case versus the "
      , "non-blocking case, it depends on whether there are any remaining "
      , "unacknowledged transactions (after taking into account the ones "
      , "acknowledged in this message): "
      , "\n "
      , "* The blocking case must be used when there are zero remaining "
      , "  unacknowledged transactions. "
      , "\n "
      , "* The non-blocking case must be used when there are non-zero remaining "
      , "  unacknowledged transactions."
      ]
    documentFor (Logging.Namespace _ ["ReplyTxIds"]) = Just $ mconcat
      [ "Reply with a list of transaction identifiers for available "
      , "transactions, along with the size of each transaction. "
      , "\n "
      , "The list must not be longer than the maximum number requested. "
      , "\n "
      , "In the 'StTxIds' 'StBlocking' state the list must be non-empty while "
      , "in the 'StTxIds' 'StNonBlocking' state the list may be empty. "
      , "\n "
      , "These transactions are added to the notional FIFO of outstanding "
      , "transaction identifiers for the protocol. "
      , "\n "
      , "The order in which these transaction identifiers are returned must be "
      , "the order in which they are submitted to the mempool, to preserve "
      , "dependent transactions."
      ]
    documentFor (Logging.Namespace _ ["RequestTxs"]) = Just $ mconcat
      [ "Request one or more transactions corresponding to the given  "
      , "transaction identifiers.  "
      , "\n "
      , "While it is the responsibility of the replying peer to keep within "
      , "pipelining in-flight limits, the sender must also cooperate by keeping "
      , "the total requested across all in-flight requests within the limits. "
      , "\n"
      , "It is an error to ask for transaction identifiers that were not "
      , "previously announced (via 'MsgReplyTxIds'). "
      , "\n"
      , "It is an error to ask for transaction identifiers that are not "
      , "outstanding or that were already asked for."
      ]
    documentFor (Logging.Namespace _ ["ReplyTxs"]) = Just $ mconcat
      [ "Reply with the requested transactions, or implicitly discard."
      , "\n"
      , "Transactions can become invalid between the time the transaction "
      , "identifier was sent and the transaction being requested. Invalid "
      , "(including committed) transactions do not need to be sent."
      , "\n"
      , "Any transaction identifiers requested but not provided in this reply "
      , "should be considered as if this peer had never announced them. (Note "
      , "that this is no guarantee that the transaction is invalid, it may still "
      , "be valid and available from another peer)."
      ]
    documentFor (Logging.Namespace _ ["Done"]) = Just $ mconcat
      [ "Termination message, initiated by the client when the server is "
      , "making a blocking call for more transaction identifiers."
      ]
    documentFor _ = Nothing

    allNamespaces = [
        Logging.Namespace [] ["MsgInit"]
      , Logging.Namespace [] ["RequestTxIds"]
      , Logging.Namespace [] ["ReplyTxIds"]
      , Logging.Namespace [] ["RequestTxs"]
      , Logging.Namespace [] ["ReplyTxs"]
      , Logging.Namespace [] ["Done"]
      ]

--------------------------------------------------------------------------------
-- KeepAlive protocol rendering
--------------------------------------------------------------------------------

-- Instances copied from cardano-node's "NodeToNode.hs".

instance Logging.LogFormatting (AnyMessage KA.KeepAlive) where
  forMachine _dtal (AnyMessageAndAgency stok KA.MsgKeepAlive {}) =
    mconcat
      [ "kind" .= String "KeepAlive"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok KA.MsgKeepAliveResponse {}) =
    mconcat
      [ "kind" .= String "KeepAliveResponse"
      , "agency" .= String (Text.pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok KA.MsgDone) =
    mconcat
      [ "kind" .= String "Done"
      , "agency" .= String (Text.pack $ show stok)
      ]

instance Logging.MetaTrace (AnyMessage KA.KeepAlive) where
    namespaceFor (AnyMessageAndAgency _stok KA.MsgKeepAlive {}) =
      Logging.Namespace [] ["KeepAlive"]
    namespaceFor (AnyMessageAndAgency _stok KA.MsgKeepAliveResponse {}) =
      Logging.Namespace [] ["KeepAliveResponse"]
    namespaceFor (AnyMessageAndAgency _stok KA.MsgDone) =
      Logging.Namespace [] ["Done"]

    severityFor (Logging.Namespace _ ["KeepAlive"]) _ = Just Logging.Debug
    severityFor (Logging.Namespace _ ["KeepAliveResponse"]) _ = Just Logging.Debug
    severityFor (Logging.Namespace _ ["Done"]) _ = Just Logging.Debug
    severityFor _ _ = Nothing

    documentFor (Logging.Namespace _ ["KeepAlive"]) = Just
        "Client side message to keep the connection alive."
    documentFor (Logging.Namespace _ ["KeepAliveResponse"]) = Just $ mconcat
      [ "Server side response to a previous client KeepAlive message."
      ]
    documentFor (Logging.Namespace _ ["Done"]) = Just $ mconcat
      [ "Termination message, initiated by the client."
      ]
    documentFor _ = Nothing

    allNamespaces = [
        Logging.Namespace [] ["KeepAlive"]
      , Logging.Namespace [] ["KeepAliveResponse"]
      , Logging.Namespace [] ["Done"]
      ]

