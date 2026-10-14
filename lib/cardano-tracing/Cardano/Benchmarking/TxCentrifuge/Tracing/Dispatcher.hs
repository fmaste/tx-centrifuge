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
import Numeric.Natural (Natural)
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
-- Fields by detail level
--------------------------------------------------------------------------------

-- Every 'forMachine' below, field by field and level by level. "-" is absent.
-- Shapes: str a string, int a number, bool a boolean, hex 64 hex characters
-- (a TxId or a TxHash), [ref] an array of "<txid>#<ix>" strings, [obj] an
-- array of {"utxo","lovelace"} objects, [idsz] an array of {"id","size"},
-- [addr] an array of bech32 addresses. A fund array is [ref] at DDetailed and
-- [obj] at DMaximum, which is 'renderFund' following the level it is given.
--
--   Builder.NewTx (Info)                         Min    Nor    Det    Max
--     builder                                    str    str    str    str
--     txHash, txId                               -      hex    hex    hex
--     inputs, outputs                            -      -      [ref]  [obj]
--     destinations                               -      -      -      [addr]
--
--   Builder.InputsDropped (Warning)              Min    Nor    Det    Max
--     builder                                    str    str    str    str
--     reason                                     -      str    str    str
--     inputs                                     -      -      [ref]  [obj]
--
--   Pipe.Inputs.Enqueued (Info)                  Min    Nor    Det    Max
--   Pipe.Inputs.Dequeued (Info)
--     pipe                                       str    str    str    str
--     depth                                      -      int    int    int
--     count                                      -      int    int    int
--     inputs                                     -      -      [ref]  [obj]
--
--   Pipe.Payload.Enqueued (Info)                 Min    Nor    Det    Max
--   Pipe.Payload.Dequeued (Info)
--     pipe                                       str    str    str    str
--     depth                                      -      int    int    int
--     txHash, txId                               -      -      hex    hex
--
--   Recycler.Backlog.Held (Info)                 Min    Nor    Det    Max
--     recycler                                   str    str    str    str
--     backlog                                    -      int    int    int
--     consumed_count, outputs_count              -      int    int    int
--     txHash, txId                               -      -      hex    hex
--     consumed, outputs                          -      -      [ref]  [obj]
--
--   Recycler.Backlog.Released.Confirmed (Info)   Min    Nor    Det    Max
--   Recycler.Backlog.Released.Orphaned (Warning)
--     recycler, pipe                             str    str    str    str
--     backlog                                    -      int    int    int
--     count                                      -      int    int    int
--     txHash, txId                               -      -      hex    hex
--     blockNo, slotNo                            -      -      int    int
--     inputs                                     -      -      [ref]  [obj]
--
--   Recycler.Backlog.Released.Optimistic (Info)  Min    Nor    Det    Max
--     recycler, pipe                             str    str    str    str
--     backlog                                    -      int    int    int
--     count                                      -      int    int    int
--     txHash, txId                               -      -      hex    hex
--     inputs                                     -      -      [ref]  [obj]
--
--   Recycler.Reset (Warning)                     Min    Nor    Det    Max
--     recycler, pipe                             str    str    str    str
--     backlog (always 0)                         -      int    int    int
--     fresh_count                                -      int    int    int
--     dropped_inputs_count                       -      int    int    int
--     dropped_payloads_count                     -      int    int    int
--     fresh, dropped_inputs                      -      -      [ref]  [obj]
--     dropped_payloads                           -      -      [hex]  [hex]
--
--   Observer.Announce.Confirmed (Info)           Min    Nor    Det    Max
--   Observer.Announce.Orphaned (Warning)
--     observer                                   str    str    str    str
--     txHash, txId                               -      hex    hex    hex
--     blockNo, slotNo                            -      -      int    int
--
--   TxSubmission.RequestTxIds (Info)             Min    Nor    Det    Max
--     target                                     str    str    str    str
--     ack, req                                   -      int    int    int
--     unacked                                    -      -      -      [hex]
--
--   TxSubmission.ReplyTxIds (Info)               Min    Nor    Det    Max
--     target                                     str    str    str    str
--     ack, req (the request's)                   -      int    int    int
--     txs                                        -      -      [idsz] [idsz]
--     unacked                                    -      -      -      [hex]
--
--   TxSubmission.RequestTxs (Info)               Min    Nor    Det    Max
--     target                                     str    str    str    str
--     txIds                                      -      -      -      [hex]
--
--   TxSubmission.ReplyTxs (Info)                 Min    Nor    Det    Max
--     target                                     str    str    str    str
--     txs                                        -      -      [idsz] [idsz]
--     requested                                  -      -      -      [hex]
--
-- The two protocol traces copied from cardano-node do not gate on the level:
-- TxSubmission2.{MsgInit, RequestTxIds, ReplyTxIds, RequestTxs, ReplyTxs,
-- Done} and KeepAlive.{KeepAlive, KeepAliveResponse, Done} (all Debug) carry
-- str "kind" and str "agency" at every level, plus a str "txIds" on
-- MsgRequestTxs and a str "txs" on MsgReplyTxs, both Haskell 'show' output
-- rather than JSON arrays.
--
-- A txId is absent at every level for a ref read from an endorser block,
-- which lists hashes and no bodies. A blockNo and slotNo are absent for a ref
-- nothing has read off the chain, which is every ref a builder makes.

--------------------------------------------------------------------------------
-- Shared rendering
--------------------------------------------------------------------------------

-- | Render a payload key as machine-format fields: @txHash@ always, @txId@
-- when the key has one (see 'Chain.TxRef'). Both are 32 bytes of hex, so the
-- field is the only thing that says which is which.
renderTxRef :: Chain.TxRef -> Object
renderTxRef txRef = mconcat $
     [ "txHash" .= String (Hash.hashToTextAsHex (Chain.txRefHash txRef)) ]
  ++ [ "txId"   .= String (Api.serialiseToRawBytesHexText txId)
     | Just txId <- [Chain.txRefId txRef]
     ]

-- | Render a key's chain position as machine-format fields, when it has one
-- (see 'Chain.txRefBlock'): the ranking block it was read in, by height and
-- slot. A key nothing has seen on the chain renders no fields.
renderTxRefBlock :: Chain.TxRef -> Object
renderTxRefBlock txRef = mconcat
  [ mconcat [ "blockNo" .= blockNo, "slotNo" .= slotNo ]
  | Just (blockNo, slotNo) <- [Chain.txRefBlock txRef]
  ]

-- | A key's chain position for 'forHuman', empty when it has none.
renderTxRefBlockHuman :: Chain.TxRef -> Text.Text
renderTxRefBlockHuman txRef = mconcat
  [    " blockNo=" <> Text.pack (show blockNo)
    <> " slotNo="  <> Text.pack (show slotNo)
  | Just (blockNo, slotNo) <- [Chain.txRefBlock txRef]
  ]

-- | The machine fields every @Recycler.Backlog.Released.*@ event carries: they
-- differ only in the namespace that says why the payload was released, and in
-- whether its key has a block (see 'renderTxRefBlock').
renderRelease
  :: Logging.DetailLevel
  -> String
  -> String
  -> Natural
  -> Chain.TxRef
  -> [Fund.Fund]
  -> Object
renderRelease dtal name pipe backlog txRef inputs = mconcat $
     [ "recycler" .= name
     , "pipe"     .= pipe
     ]
  ++ [ "backlog"  .= backlog
     | dtal >= Logging.DNormal
     ]
  ++ [ "count"    .= length inputs
     | dtal >= Logging.DNormal
     ]
  ++ [ renderTxRef txRef <> renderTxRefBlock txRef
     | dtal >= Logging.DDetailed
     ]
  ++ [ "inputs"   .= map (renderFund dtal) inputs
     | dtal >= Logging.DDetailed
     ]

-- | The human line every release shares, labelled by why it happened.
renderReleaseHuman
  :: Text.Text -> String -> String -> Natural -> [Fund.Fund] -> Text.Text
renderReleaseHuman label name pipe backlog inputs =
     label <> " [" <> Text.pack name <> "]"
  <> " pipe=" <> Text.pack pipe
  <> " backlog=" <> Text.pack (show backlog)
  <> " count=" <> Text.pack (show (length inputs))

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
-- Machine format ('Logging.DMinimal'): the builder only, which for
-- 'BuilderInputsDropped' is the whole line.
--
-- Machine format ('Logging.DNormal'): adds both of the transaction's
-- identifiers, the @txId@ that a ranking block's body gives it and the
-- @txHash@ that an endorser block lists it under (and, for
-- 'BuilderInputsDropped', the @reason@).
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
  forMachine dtal (BuilderNewTx name txRef dests inputs outputs) = mconcat $
       [ "builder" .= name ]
    ++ [ renderTxRef txRef
       | dtal >= Logging.DNormal
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
       [ "builder" .= name ]
    ++ [ "reason"  .= reason
       | dtal >= Logging.DNormal
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DDetailed
       ]
  forHuman (BuilderNewTx name txRef _dest inputs outputs) =
       "NewTx [" <> Text.pack name <> "] "
    <> Hash.hashToTextAsHex (Chain.txRefHash txRef)
    <> " inputs=["  <> renderFundTxIns inputs  <> "]"
    <> " outputs=[" <> renderFundTxIns outputs <> "]"
  forHuman (BuilderInputsDropped name inputs reason) =
       "InputsDropped [" <> Text.pack name <> "] "
    <> "reason=" <> Text.pack reason
    <> " inputs=[" <> renderFundTxIns inputs <> "]"

--------------------------------------------------------------------------------
-- Pipe trace rendering
--------------------------------------------------------------------------------

-- | Namespaces: @TxCentrifuge.Pipe.Inputs.{Enqueued, Dequeued}@ and
-- @TxCentrifuge.Pipe.Payload.{Enqueued, Dequeued}@. Outer prefix
-- @[\"TxCentrifuge\", \"Pipe\"]@ is set in 'setupTracers'. Grouped by queue so
-- that the input events, which the startup load makes one of per fund, can be
-- configured apart from the payload ones.
instance Logging.MetaTrace PipeTrace where
  namespaceFor PipeInputsEnqueued{}  = Logging.Namespace [] ["Inputs","Enqueued"]
  namespaceFor PipeInputsDequeued{}  = Logging.Namespace [] ["Inputs","Dequeued"]
  namespaceFor PipePayloadEnqueued{} = Logging.Namespace [] ["Payload","Enqueued"]
  namespaceFor PipePayloadDequeued{} = Logging.Namespace [] ["Payload","Dequeued"]
  severityFor (Logging.Namespace _ ["Inputs","Enqueued"])  _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["Inputs","Dequeued"])  _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["Payload","Enqueued"]) _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["Payload","Dequeued"]) _ = Just Logging.Info
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["Inputs","Enqueued"]) = Just
    "Inputs were added to the input queue (count from DNormal, the funds from DDetailed)."
  documentFor (Logging.Namespace _ ["Inputs","Dequeued"]) = Just
    "Inputs were removed from the input queue for the builder (count from DNormal, the funds from DDetailed)."
  documentFor (Logging.Namespace _ ["Payload","Enqueued"]) = Just
    "A payload was added to the payload queue (its key from DDetailed)."
  documentFor (Logging.Namespace _ ["Payload","Dequeued"]) = Just
    "A payload was removed from the payload queue for a worker (its key from DDetailed)."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["Inputs","Enqueued"]
    , Logging.Namespace [] ["Inputs","Dequeued"]
    , Logging.Namespace [] ["Payload","Enqueued"]
    , Logging.Namespace [] ["Payload","Dequeued"]
    ]

-- | Machine format: every Pipe event carries @pipe@ at every level and
-- @depth@ from 'Logging.DNormal' (the namespace already says which queue).
-- Input events add @count@ from 'Logging.DNormal' too, and an @inputs@ array
-- from 'Logging.DDetailed', its funds
-- as references there and as objects at 'Logging.DMaximum' (see 'renderFund').
-- Payload events add the payload's key from 'Logging.DDetailed', the level at
-- which the input events name their funds (see 'renderTxRef' for the fields it
-- lands in). Human format shows just the depth (and, for inputs, the count).
instance Logging.LogFormatting PipeTrace where
  forMachine dtal (PipeInputsEnqueued name depth inputs) = mconcat $
       [ "pipe"  .= name ]
    ++ [ "depth" .= depth
       | dtal >= Logging.DNormal
       ]
    ++ [ "count"  .= length inputs
       | dtal >= Logging.DNormal
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (PipeInputsDequeued name depth inputs) = mconcat $
       [ "pipe"  .= name ]
    ++ [ "depth" .= depth
       | dtal >= Logging.DNormal
       ]
    ++ [ "count"  .= length inputs
       | dtal >= Logging.DNormal
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (PipePayloadEnqueued name depth txRef) = mconcat $
       [ "pipe"  .= name ]
    ++ [ "depth" .= depth
       | dtal >= Logging.DNormal
       ]
    ++ [ renderTxRef txRef
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (PipePayloadDequeued name depth txRef) = mconcat $
       [ "pipe"  .= name ]
    ++ [ "depth" .= depth
       | dtal >= Logging.DNormal
       ]
    ++ [ renderTxRef txRef
       | dtal >= Logging.DDetailed
       ]
  forHuman (PipeInputsEnqueued name depth inputs) =
       "InputsEnqueued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)
    <> " count=" <> Text.pack (show (length inputs))
  forHuman (PipeInputsDequeued name depth inputs) =
       "InputsDequeued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)
    <> " count=" <> Text.pack (show (length inputs))
  forHuman (PipePayloadEnqueued name depth _txRef) =
       "PayloadEnqueued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)
  forHuman (PipePayloadDequeued name depth _txRef) =
       "PayloadDequeued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)

--------------------------------------------------------------------------------
-- Recycler trace rendering
--------------------------------------------------------------------------------

-- | Namespaces: @TxCentrifuge.Recycler.Backlog.Held@,
-- @TxCentrifuge.Recycler.Backlog.Released.{Confirmed, Orphaned, Optimistic}@
-- and @TxCentrifuge.Recycler.Reset@. Outer prefix
-- @[\"TxCentrifuge\", \"Recycler\"]@ is set in 'setupTracers'.
--
-- An entry enters the backlog under @Held@ and leaves under one of the
-- releases, which name why, so @Recycler.Backlog.Released@ is one prefix for
-- all three. Every release is for a key the backlog held, which makes these
-- this generator's own transactions, where @Observer.Announce@ is the whole
-- chain. 'RecyclerReset' stays outside the group: it drops the pipe's queued
-- inputs and payloads too, and clears the backlog wholesale rather than
-- releasing one entry.
instance Logging.MetaTrace RecyclerTrace where
  namespaceFor RecyclerHeld{} = Logging.Namespace [] ["Backlog","Held"]
  namespaceFor RecyclerReleasedConfirmed{} =
    Logging.Namespace [] ["Backlog","Released","Confirmed"]
  namespaceFor RecyclerReleasedOrphaned{} =
    Logging.Namespace [] ["Backlog","Released","Orphaned"]
  namespaceFor RecyclerReleasedOptimistic{} =
    Logging.Namespace [] ["Backlog","Released","Optimistic"]
  namespaceFor RecyclerReset{} = Logging.Namespace [] ["Reset"]
  severityFor (Logging.Namespace _ ["Backlog","Held"]) _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["Backlog","Released","Confirmed"]) _ =
    Just Logging.Info
  severityFor (Logging.Namespace _ ["Backlog","Released","Orphaned"]) _ =
    Just Logging.Warning
  severityFor (Logging.Namespace _ ["Backlog","Released","Optimistic"]) _ =
    Just Logging.Info
  severityFor (Logging.Namespace _ ["Reset"]) _ = Just Logging.Warning
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["Backlog","Held"]) = Just $
    "A payload's entry entered the recycler's backlog: the key and both "
    <> "input sets, held until a release picks one. The backlog count is "
    <> "the number of held payloads, not a pipe queue depth."
  documentFor (Logging.Namespace _ ["Backlog","Released","Confirmed"]) = Just $
    "The chain confirmed a held payload, so its outputs went back onto a "
    <> "pipe's input queue. The key carries the block it was confirmed in "
    <> "(from DDetailed). Own traffic: the backlog held this key."
  documentFor (Logging.Namespace _ ["Backlog","Released","Orphaned"]) = Just $
    "A held payload's block was rolled back, so its consumed inputs went "
    <> "back instead of its outputs. The key carries the rolled-back block."
  documentFor (Logging.Namespace _ ["Backlog","Released","Optimistic"]) = Just $
    "A held payload was released with no chain verdict, which is on_build "
    <> "and on_pull: its outputs went back straight away and its key has no "
    <> "block."
  documentFor (Logging.Namespace _ ["Reset"]) = Just $
    "The recycler reset a pipe for a builder's recovery: the queued inputs "
    <> "and the queued payloads built from them were dropped, and the input "
    <> "queue was reseeded with fresh inputs (counts from DNormal, the "
    <> "arrays from DDetailed)."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["Backlog","Held"]
    , Logging.Namespace [] ["Backlog","Released","Confirmed"]
    , Logging.Namespace [] ["Backlog","Released","Orphaned"]
    , Logging.Namespace [] ["Backlog","Released","Optimistic"]
    , Logging.Namespace [] ["Reset"]
    ]

-- | Machine format: every event emits @recycler@ at every level and @backlog@
-- (the resulting backlog size, not a queue depth) from 'Logging.DNormal', then
-- the same ladder as everywhere else: counts from 'Logging.DNormal', arrays
-- from 'Logging.DDetailed' with their funds as references there and as objects
-- at 'Logging.DMaximum' (see 'renderFund'), payload keys from
-- 'Logging.DDetailed' (see 'renderTxRef').
--
-- * 'RecyclerHeld': @consumed_count@ and @outputs_count@, then the @consumed@
--   and @outputs@ arrays, then the held payload's key.
-- * The three releases (also @pipe@): @count@, then the released payload's key
--   and, when it has one, the block it was read in (see 'renderTxRefBlock'),
--   then the @inputs@ array. One renderer serves all three ('renderRelease'):
--   they differ in their namespace, and in whether that block is there.
-- * 'RecyclerReset' (also @pipe@): @fresh_count@, @dropped_inputs_count@ and
--   @dropped_payloads_count@, then the @fresh@, @dropped_inputs@ and
--   @dropped_payloads@ arrays, the last being payload keys as hex.
--
-- Human format shows the backlog size and the counts.
instance Logging.LogFormatting RecyclerTrace where
  forMachine dtal (RecyclerHeld recyclerName backlog txRef consumed outputs) =
    mconcat $
       [ "recycler" .= recyclerName ]
    ++ [ "backlog"  .= backlog
       | dtal >= Logging.DNormal
       ]
    ++ [ "consumed_count" .= length consumed
       | dtal >= Logging.DNormal
       ]
    ++ [ "outputs_count" .= length outputs
       | dtal >= Logging.DNormal
       ]
    ++ [ renderTxRef txRef
       | dtal >= Logging.DDetailed
       ]
    ++ [ "consumed" .= map (renderFund dtal) consumed
       | dtal >= Logging.DDetailed
       ]
    ++ [ "outputs" .= map (renderFund dtal) outputs
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (RecyclerReleasedConfirmed name pipe backlog txRef inputs) =
    renderRelease dtal name pipe backlog txRef inputs
  forMachine dtal (RecyclerReleasedOrphaned name pipe backlog txRef inputs) =
    renderRelease dtal name pipe backlog txRef inputs
  forMachine dtal (RecyclerReleasedOptimistic name pipe backlog txRef inputs) =
    renderRelease dtal name pipe backlog txRef inputs
  forMachine dtal (RecyclerReset recyclerName pipeName backlog droppedPayloads droppedInputs fresh) =
    mconcat $
       [ "recycler" .= recyclerName
       , "pipe"     .= pipeName
       ]
    ++ [ "backlog"  .= backlog
       | dtal >= Logging.DNormal
       ]
    ++ [ "fresh_count" .= length fresh
       | dtal >= Logging.DNormal
       ]
    ++ [ "dropped_inputs_count" .= length droppedInputs
       | dtal >= Logging.DNormal
       ]
    ++ [ "dropped_payloads_count" .= length droppedPayloads
       | dtal >= Logging.DNormal
       ]
    ++ [ "fresh" .= map (renderFund dtal) fresh
       | dtal >= Logging.DDetailed
       ]
    ++ [ "dropped_inputs" .= map (renderFund dtal) droppedInputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "dropped_payloads" .=
           map (String . Hash.hashToTextAsHex . Chain.txRefHash) droppedPayloads
       | dtal >= Logging.DDetailed
       ]
  forHuman (RecyclerHeld recyclerName backlog _txRef consumed outputs) =
       "Held [" <> Text.pack recyclerName <> "]"
    <> " backlog=" <> Text.pack (show backlog)
    <> " consumed_count=" <> Text.pack (show (length consumed))
    <> " outputs_count=" <> Text.pack (show (length outputs))
  forHuman (RecyclerReleasedConfirmed name pipe backlog _txRef inputs) =
    renderReleaseHuman "Confirmed" name pipe backlog inputs
  forHuman (RecyclerReleasedOrphaned name pipe backlog _txRef inputs) =
    renderReleaseHuman "Orphaned" name pipe backlog inputs
  forHuman (RecyclerReleasedOptimistic name pipe backlog _txRef inputs) =
    renderReleaseHuman "Optimistic" name pipe backlog inputs
  forHuman (RecyclerReset recyclerName pipeName backlog droppedPayloads droppedInputs fresh) =
       "Reset [" <> Text.pack recyclerName <> "]"
    <> " pipe=" <> Text.pack pipeName
    <> " backlog=" <> Text.pack (show backlog)
    <> " fresh_count=" <> Text.pack (show (length fresh))
    <> " dropped_inputs_count=" <> Text.pack (show (length droppedInputs))
    <> " dropped_payloads_count=" <> Text.pack (show (length droppedPayloads))

--------------------------------------------------------------------------------
-- Observer trace rendering
--------------------------------------------------------------------------------

-- | Namespaces: @TxCentrifuge.Observer.Announce.{Confirmed, Orphaned}@. Outer
-- prefix @[\"TxCentrifuge\", \"Observer\"]@ is set in 'setupTracers'. Two
-- namespaces so the flood (one confirmation per chain transaction per
-- observer) can be silenced while the rare orphan is kept.
instance Logging.MetaTrace ObserverTrace where
  namespaceFor ObserverConfirmed{} = Logging.Namespace [] ["Announce","Confirmed"]
  namespaceFor ObserverOrphaned{}  = Logging.Namespace [] ["Announce","Orphaned"]
  severityFor (Logging.Namespace _ ["Announce","Confirmed"]) _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["Announce","Orphaned"])  _ = Just Logging.Warning
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["Announce","Confirmed"]) = Just
    "The observer saw a transaction confirmed."
  documentFor (Logging.Namespace _ ["Announce","Orphaned"]) = Just
    "The observer saw a transaction orphaned: it was in a block that was rolled back and did not reappear within the confirmation window."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["Announce","Confirmed"]
    , Logging.Namespace [] ["Announce","Orphaned"]
    ]

instance Logging.LogFormatting ObserverTrace where
  forMachine dtal (ObserverConfirmed observer txRef) = mconcat $
       [ "observer" .= observer ]
    ++ [ renderTxRef txRef
       | dtal >= Logging.DNormal
       ]
    ++ [ renderTxRefBlock txRef
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (ObserverOrphaned observer txRef) = mconcat $
       [ "observer" .= observer ]
    ++ [ renderTxRef txRef
       | dtal >= Logging.DNormal
       ]
    ++ [ renderTxRefBlock txRef
       | dtal >= Logging.DDetailed
       ]
  forHuman (ObserverConfirmed observer txRef) =
       "Confirmed [" <> Text.pack observer <> "]"
    <> " txHash=" <> Hash.hashToTextAsHex (Chain.txRefHash txRef)
    <> renderTxRefBlockHuman txRef
  forHuman (ObserverOrphaned observer txRef) =
       "Orphaned [" <> Text.pack observer <> "]"
    <> " txHash=" <> Hash.hashToTextAsHex (Chain.txRefHash txRef)
    <> renderTxRefBlockHuman txRef

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
    "Node requested tx id announcements (blocking or non-blocking). ACK and REQ from DNormal, the unacked ids at DMaximum."
  documentFor (Logging.Namespace _ ["ReplyTxIds"]) = Just
    "We replied with tx id announcements and sizes. The answered request's ACK and REQ from DNormal, the announced pairs from DDetailed, the unacked ids at DMaximum."
  documentFor (Logging.Namespace _ ["RequestTxs"]) = Just
    "Node requested full transactions by TxId. The requested ids at DMaximum."
  documentFor (Logging.Namespace _ ["ReplyTxs"]) = Just
    "We sent the requested transactions. The sent pairs from DDetailed, the requested ids at DMaximum."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["RequestTxIds"]
    , Logging.Namespace [] ["ReplyTxIds"]
    , Logging.Namespace [] ["RequestTxs"]
    , Logging.Namespace [] ["ReplyTxs"]
    ]

-- | Machine-readable and human-readable rendering. The bare identifier lists
-- are omitted below 'Logging.DMaximum': hex-encoding every identifier on every
-- protocol round-trip is what costs here, and the sizes a report needs come
-- with the @txs@ pairs one level down.
--
-- Machine format ('Logging.DMinimal'): the target alone, for all four.
--
-- Machine format ('Logging.DNormal'): adds the ACK and REQ counts, which only
-- the TxId messages carry. On 'ReplyTxIds' they are not new numbers: they are
-- the ACK and REQ of the @MsgRequestTxIds@ this reply answers, echoed, so REQ
-- against the @txs@ count below is the demand this round-trip did not meet.
--
-- @
-- { \"target\": \"n\", \"ack\": 0, \"req\": 3 }
-- { \"target\": \"n\", \"ack\": 0, \"req\": 3 }
-- { \"target\": \"n\" }
-- { \"target\": \"n\" }
-- @
--
-- Machine format ('Logging.DDetailed'): adds what we announced or sent, with
-- the sizes, so 'ReplyTxIds' and 'ReplyTxs' only.
--
-- @
-- { .., \"txs\": [{\"id\":\"ab..\",\"size\":9}] }
-- @
--
-- Machine format ('Logging.DMaximum'): adds the identifier lists, @unacked@ on
-- both TxId messages, @txIds@ on 'RequestTxs' and @requested@ on 'ReplyTxs'.
--
-- @
-- { .., \"unacked\": [\"ab..\"] }
-- @
instance Logging.LogFormatting TxSubmission where
  forMachine dtal (RequestTxIds target unacked ack req) = mconcat $
       [ "target"    .= target ]
    ++ [ "ack"       .= ack
       | dtal >= Logging.DNormal
       ]
    ++ [ "req"       .= req
       | dtal >= Logging.DNormal
       ]
    ++ [ "unacked"   .= map Api.serialiseToRawBytesHexText unacked
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (ReplyTxIds target ack req unacked announced) = mconcat $
       [ "target"    .= target ]
    ++ [ "ack"       .= ack
       | dtal >= Logging.DNormal
       ]
    ++ [ "req"       .= req
       | dtal >= Logging.DNormal
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
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (RequestTxs target txIds) = mconcat $
       [ "target"    .= target ]
    ++ [ "txIds"     .= map Api.serialiseToRawBytesHexText txIds
       | dtal >= Logging.DMaximum
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
       | dtal >= Logging.DMaximum
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

