# Tx Centrifuge & Pull-Fiction

`tx-centrifuge` is a high-performance load generator for Cardano, built on top of the protocol-agnostic **Pull-Fiction** library.

Unlike traditional load generators that "push" data at a fixed rate, this system is designed for **pull-based protocols**. It does not generate load by itself; instead, it acts as a **policer** that reacts to requests from downstream consumers, admitting or delaying them to enforce a configured rate ceiling.

### Minimal Configuration Example

A basic configuration defines how to load initial resources, how to build payloads, the desired rate, and where to send the results:

```json
{
  "initial_inputs": { "source": "genesis-funds" },
  "input_sources": {
    "genesis-funds": {
      "type": "genesis_utxo_keys",
      "params": {
        "signing_keys_file": "funds.jsonl"
      }
    }
  },
  "builder": {
    "type": "value",
    "params": { "fee": 1000000 },
    "recycle": { "type": "on_pull" }
  },
  "rate_limit": { "type": "token_bucket", "params": { "tps": 10 } },
  "workloads": {
    "group-A": {
      "targets": {
        "node-1": { "addr": "127.0.0.1", "port": 30000 },
        "node-2": { "addr": "127.0.0.1", "port": 30001 }
      }
    }
  },
  "nodeConfig": "node-config.json"
}
```

## Core Concepts: The Pull-Fiction Engine

The underlying `pull-fiction` library implements a reactive rate-limiting strategy. It only produces data when a consumer asks for it, and only as fast as the rate limiter allows.

### Architecture

```
                                    ┌───────────────┐
                                    │ Initial UTxOs │
                                    └───────┬───────┘
                                            │
                                            ▼
┌─────────────────────────────────────────────────────────────────────────────────────┐
│                               Input Queue (unbounded)                               │
│              one queue for every workload: any builder takes from it,               │
│                        every recycler returns outputs to it                         │
└─────────────┬─────────────────────────────┬─────────────────────────────┬───────────┘
              │                             │                             │
              ▼                             ▼                             ▼
┌─────────────────────────┐   ┌─────────────────────────┐   ┌─────────────────────────┐
│       Workload A        │   │       Workload B        │   │       Workload N        │
├─────────────────────────┤   ├─────────────────────────┤   ├─────────────────────────┤
│                         │   │                         │   │                         │
│   ┌─────────────────┐   │   │   ┌─────────────────┐   │   │   ┌─────────────────┐   │
│   │     Builder     │   │   │   │     Builder     │   │   │   │     Builder     │   │
│   │ (build & sign)  │   │   │   │ (build & sign)  │   │   │   │ (build & sign)  │   │
│   └────────┬────────┘   │   │   └────────┬────────┘   │   │   └────────┬────────┘   │
│            │            │   │            │            │   │            │            │
│            ▼            │   │            ▼            │   │            ▼            │
│   ┌─────────────────┐   │   │   ┌─────────────────┐   │   │   ┌─────────────────┐   │
│   │  Payload Queue  │   │   │   │  Payload Queue  │   │   │   │  Payload Queue  │   │
│   │   (bounded)     │   │   │   │   (bounded)     │   │   │   │   (bounded)     │   │
│   └────────┬────────┘   │   │   └────────┬────────┘   │   │   └────────┬────────┘   │
│            │            │   │            │            │   │            │            │
│      ┌─────┴─────┐      │   │      ┌─────┴─────┐      │   │      ┌─────┴─────┐      │
│      ▼           ▼      │   │      ▼           ▼      │   │      ▼           ▼      │
│  ┌────────┐ ┌────────┐  │   │  ┌────────┐ ┌────────┐  │   │  ┌────────┐ ┌────────┐  │
│  │Worker 1│ │Worker 2│  │   │  │Worker 1│ │Worker 2│  │   │  │Worker 1│ │Worker 2│  │
│  └───┬────┘ └───┬────┘  │   │  └───┬────┘ └───┬────┘  │   │  └───┬────┘ └───┬────┘  │
│      │          │       │   │      │          │       │   │      │          │       │
│      ▼          ▼       │   │      ▼          ▼       │   │      ▼          ▼       │
│  NodeToNode     ...     │   │  NodeToNode     ...     │   │  NodeToNode     ...     │
│  (multiplexed)          │   │  (multiplexed)          │   │  (multiplexed)          │
│      │          │       │   │      │          │       │   │      │          │       │
│      ▼          ▼       │   │      ▼          ▼       │   │      ▼          ▼       │
│  ┌──────┐   ┌──────┐    │   │  ┌──────┐   ┌──────┐    │   │  ┌──────┐   ┌──────┐    │
│  │Node 1│   │Node 2│    │   │  │Node 3│   │Node 4│    │   │  │Node 5│   │Node 6│    │
│  └──────┘   └──────┘    │   │  └──────┘   └──────┘    │   │  └──────┘   └──────┘    │
│                         │   │                         │   │                         │
│ outputs ──► Input Queue │   │ outputs ──► Input Queue │   │ outputs ──► Input Queue │
│                         │   │                         │   │                         │
└─────────────────────────┘   └─────────────────────────┘   └─────────────────────────┘
```

**Pipeline flow:**
1. **Initial UTxOs** are loaded into one **Input Queue** (unbounded), shared by every workload
2. Any workload can take any input, since a fund carries the signing key that spends it and so belongs to no workload. Give each workload its own queue instead with [`"scope": "per_workload"`](#input-scope-scope)
3. A **Builder** (one per workload) pulls inputs, assembles and signs transactions, and pushes `(tx, outputs)` to the **Payload Queue** (bounded — sole source of backpressure)
4. **Workers** (one per target) pull from the Payload Queue via rate-limited fetchers
5. Workers connect to Cardano nodes via a **multiplexed NodeToNode** connection running **TxSubmission2** and **KeepAlive** mini-protocols (optionally **ChainSync** + **BlockFetch** for confirmation-based recycling)
6. **Outputs are recycled** back to the Input Queue according to the configured `recycle` strategy, enabling indefinite-duration runs

### Reactive Rate Limiting

- **Downstream Driven**: Load is only dispensed in response to an explicit pull from a target. If the target doesn't ask, the engine stays idle.
- **Ceiling Enforcement**: The rate limiter enforces a tokens-per-second (TPS) ceiling. Even if a consumer pulls aggressively, the engine ensures the dispensed items never exceed the configured limit.
- **Fairness**: Token slots are claimed in a single atomic STM transaction, providing FIFO-fair scheduling across multiple workers sharing the same limiter.

### Workloads and Targets

The configuration is organized into a hierarchy that defines the concurrency model:

- **Target**: A single network endpoint (e.g., a Cardano node). Each target has a dedicated **Worker thread** that manages the network connection and handles requests.
- **Workload**: A logical grouping of targets.
  - All targets within a workload share the same **Builder thread** and the same **Payload Queue**.
  - **Transaction Profiles**: Each workload can define its own `builder` configuration. This allows you to generate different "profiles" of transactions (e.g., different sizes, complexities, or fees) for different groups of nodes.
  - **Isolation**: By using multiple workloads, you can isolate different groups of targets. For example, one workload could simulate high-volume "small" transactions for one group of nodes, while another generates "heavy" transactions for another.

### Pipeline Architecture

The engine operates as a decoupled production pipeline using generic `input` and `payload` types:
1.  **Initial Inputs**: Starting resources are bulk-loaded into the Input Queue at startup, one queue shared by every workload unless the config asks for one per workload (see [Input scope](#input-scope-scope)).
2.  **Input Queue (Unbounded)**: Holds available inputs. Every builder on it takes from it and every recycler returns to it.
3.  **Builder (One per Workload)**: A dedicated thread that pulls inputs, produces a payload, and handles recycling according to the configured strategy (see [Resource Recycling](#resource-recycling)). It pushes the payload to the payload queue.
4.  **Payload Queue (Bounded)**: The sole source of **backpressure**. The builder blocks here if consumers are slower than the production rate.
5.  **Workers (One per Target)**: Threads that manage the consumer connection. They pull from the payload queue via a rate-limited fetcher.

### Resource Recycling

To enable indefinite-duration runs with finite resources, inputs must be returned to the `Input Queue` (the shared one, or the workload's own under `"scope": "per_workload"`). The `recycle` field on the `builder` selects when this happens. There are three strategies:

1.  **`on_build`** — Resources return to the `Input Queue` as soon as the payload is constructed: the build itself counts as the confirmation, so the recycler releases the outputs right away (an `AddToBacklog` and an `AddToPipe` trace per payload). This is the highest-throughput mode but assumes the payload will be successfully processed downstream.
    ```json
    "recycle": { "type": "on_build" }
    ```
2.  **`on_pull`** — The recycler remembers the payload's resources when it is built and returns them to the `Input Queue` when a worker **dequeues** the payload from the pipe (triggered by a downstream request), on its own thread. Recycling happens on dequeue, not on downstream acknowledgement.

    > **TODO:** rename the config value `on_pull` to `on_dequeue` — the strategy recycles when the payload is *dequeued* from the pipe, not on a TxSubmission "pull". Kept as `on_pull` for backward compatibility (the code type is already `RecycleOnDequeue`).

    ```json
    "recycle": { "type": "on_pull" }
    ```
3.  **`on_confirm`** — Resources stay in the recycler's backlog until an **observer** confirms the transaction on-chain at the configured confirmation depth. The builder enqueues the payload without any inputs; a background recycler async reads confirmations from the observer's broadcast channel and recycles matching inputs. This is the safest mode for long-running benchmarks where mempool eviction is a concern.
    ```json
    "recycle": { "type": "on_confirm", "params": "my-observer" }
    ```

#### Rollback recovery (`recovery`)

Any builder with a `recycle` strategy may additionally carry a **`recovery`**, a sibling of `recycle` in the builder object, naming the observer whose orphan events trigger it and the input source that rebuilds the queued inputs:
```json
"recycle": { "type": "on_build" },
"recovery": { "observer": "my-observer", "source": "my-node-utxo" }
```
For `on_confirm`, `observer` may be omitted and defaults to the confirm observer; `on_build` and `on_pull` require it (they have no observer to default to):
```json
"recycle": { "type": "on_confirm", "params": "my-observer" },
"recovery": { "source": "my-node-utxo" }
```
The `source` must name a `utxo_query` entry in the top-level [`input_sources`](#input-sources-input_sources) pool (a static file source cannot reflect the chain after a rollback):
```json
"input_sources": {
  "my-node-utxo": { "type": "utxo_query", "params": { "socket_path": "node.socket" } }
}
```
With more than one workload configured, a `recovery` requires `"initial_inputs"` `"scope": "per_workload"` (see [Input scope](#input-scope-scope)), because its reset empties one input queue and refills it from one builder's destination addresses: on the default shared queue that would discard every other workload's queued inputs. Setting both is a startup error. A single workload needs no such setting, being alone on the queue, which is the topology the optimistic strategies want anyway. With `on_confirm` there is a second way out: drop the `recovery` and keep the shared queue, since an `on_confirm` orphan without a recovery already returns that transaction's own consumed inputs to the queue. What you lose is the flush that covers a confirm followed by a rollback, where the payload has already been released and its recycled outputs stay queued.

With a `recovery`, an orphan event from the named observer no longer recovers just that transaction's consumed inputs: the recycler drops every queued input (potentially poisoned by the rollback), drops every queued payload built from those inputs (equally stale: they either descend from orphaned transactions or double-spend the lineage the reseed restarts), and reseeds the input queue from chain truth, querying the UTxOs currently at the builder's destination address through the source's NodeToClient `socket_path`. The observer and the source are independent: any observer type works, and the source's socket may point at a different node than the one the observer follows. The observer stays an independent shared entity: builders with and without a `recovery` can name the same observer, each with its own pipeline, and every observer connection runs once no matter how many builders or purposes reference it. A subscription delivers the observer's full stream, and the workload takes what applies to its strategy (for `on_confirm`, a different `recovery` observer's confirms also count as confirmations). An explicitly named observer is always subscribed, so explicitly repeating the confirm observer creates a second subscription with each event delivered twice: duplicate orphans are ignored or rerun an idempotent reseed, and duplicate confirms are ignored (the payload is no longer held). The socket is probed once at startup, so a wrong `socket_path` fails immediately rather than at the first rollback. At runtime a failed query is reported and retried every second until it succeeds; the builder keeps building from the stale queue meanwhile (payloads delivered in that window fail harmlessly downstream, the rest are flushed by the reset) and the reset replaces the queue once the query lands, traced as `TxCentrifuge.Recycler.Reset`. One rollback triggers one recovery. Under `on_confirm` the reset is additionally **keyed**: the recycler applies it only while the orphaned transaction is still held awaiting its confirm, so an orphan of a *foreign* transaction (the observer stream is unfiltered) or a duplicate delivery is ignored and no reset happens — foreign traffic cannot reset an `on_confirm` recovery, and a rollback's orphan burst collapses at that gate (at one recovery query per orphan event). Under `on_build` and `on_pull` the orphaned transaction was already released at build or dequeue, so the reset cannot be gated: it applies on any settled orphan, foreign or not, and the orphan events that accumulate while the recovery runs are discarded.

This is what makes the optimistic strategies (`on_build`, `on_pull`) safe to run with few funds (down to a single one): transactions are allowed to fail in the window between submission and confirmation, and the recovery resets the input queue to chain truth when a rollback invalidates them, instead of provisioning enough funds to keep every in-flight transaction valid.

**Topology note**: an optimistic builder's transactions form dependency chains (each spends the previous one's output), so point each optimistic workload at a **single node**, one workload per node, each with its own signing key, observer and socket. Spreading one optimistic builder across many targets both races transaction diffusion between nodes (a child can reach a node before its parent) and lets a fork on a non-observed node go undetected.

## Configuration

### Node Configuration (`nodeConfig`)

The top-level `nodeConfig` field is the path to the Cardano node's configuration file (e.g., `node-config.json`). The generator reads the consensus protocol from it and derives the network it runs against (mainnet or a testnet magic) from that protocol. This is the single source of the network: it is used to build addresses, submit transactions, and query the node for UTxOs, so no network magic is configured anywhere else.

### Input Sources (`input_sources`)

The top-level `input_sources` pool defines named ways to obtain UTxO funds, as `type` + `params` entries like observers. Sources are referenced by name from [`initial_inputs`](#initial-inputs-initial_inputs) (the startup load) and from builder [`recovery`](#rollback-recovery-recovery) entries (rebuilding a builder's queued inputs after a rollback). Validation enforces that every referenced source is defined and that every defined source is referenced. Observer and input source names live in independent namespaces (every reference field is typed: `source` resolves against `input_sources`, `observer` against `observers`), so an observer and a source may share a name.

The `type` field selects the source. Two variants are available:

**`genesis_utxo_keys`** — load funds from a fund file. `params`:
- **`signing_keys_file`**: Path to the fund file (e.g. `funds.jsonl`) containing the actual fund data. Two shapes are accepted, a JSON array or JSONL, see below.
- **`stride`** (optional, default `1`): the gap between the positions one pass reads, in the sense the word has in cache and memory-access work, where a stride is the distance between consecutive accesses. `1` reads the file in file order.
- **`passes`** (optional, default `1`): how many of each stride's positions are read. Must not exceed `stride`, or a fund would be read twice and its UTxO spent twice, which is a startup error.

Pass `p` (counting from zero) reads the funds whose position is `p` modulo the stride, and the passes run in order. So the pair selects **which** funds enter the queues and **in what order**:

| `stride` / `passes` | funds loaded | order |
|---------------------|--------------|-------|
| `1` / `1` (default) | all | file order |
| `s` / `1` | every `s`-th, so `1/s` of the file | one sweep, evenly spread over the whole file |
| `s` / `s` | all | `s` sweeps, each spanning the whole file |
| `s` / `p` (`p < s`) | `p/s` of the file | `p` sweeps, each spanning the whole file |

Two uses:

- **Use a fraction of a big file, without using only its start.** A queue is FIFO, so a run that spends fewer funds than it loaded spends a *prefix* of what was loaded. `stride: 13, passes: 1` instead loads an even thirteenth of the file, and skips the rest without decoding: a skipped line costs a line read, where a loaded one costs a JSON decode, a signing key and an address. On a file of 6.8M funds that is roughly 18 seconds instead of 239.
- **Spend the whole file, but never two neighbours in a row.** `stride: 64, passes: 64` loads every fund in 64 sweeps. With `scope: "per_workload"` and `Q` input queues, the funds are handed out round-robin by load position, so one queue's consecutive funds end up `stride × Q` lines apart: with 51 workloads and `stride: 64`, 3264 lines apart, against 51 apart by default. The cost over the default is one extra scan of the file per pass, under a second per gigabyte each.

A `--dry-run` reports how many funds the fold produced, which is the quickest check that a stride and pass count select what you meant.

Note what this does not change: a fund file of genesis UTxOs has a TxId derived from each entry's own key, so the node's UTxO set sees those funds scattered whatever order they are loaded in. The stride controls which funds are used, in what order, and how far apart in the file consecutive spends sit.

#### Fund file entry types

The file holds fund objects, as a JSON array or one per line (see
[below](#one-object-per-line-for-very-large-files)). Each object carries its key
in one of two fields, plus a required `lovelace` and an optional `tx_in`:

| Field             | Required       | Description |
|-------------------|----------------|-------------|
| `signing_key`     | one of the two | Path of a `.skey` file |
| `skey_cbor_hex`   | one of the two | The key itself: the `cborHex` of such a file |
| `lovelace`        | Yes            | Lovelace amount, one number, not a multi-asset `Value` |
| `tx_in`           | No             | Explicit UTxO reference in `"txid#ix"` format |

#### `signing_key` or `skey_cbor_hex`

A path, read at startup:
```json
{ "signing_key": "utxo1.skey", "lovelace": 9000000000 }
```

Or the key itself, which keeps the fund file self-contained with no keys to ship
beside it and no file to open per entry:
```json
{ "skey_cbor_hex": "5820a1b2...", "lovelace": 9000000000 }
```

Exactly one of the two. Both missing is an error, both present is an error, and
neither is guessed from the text: the field name says which one an entry
carries.

`skey_cbor_hex` is the 68 hex characters of a `.skey` file's `cborHex` field, the
CBOR of the 32-byte ed25519 key, so it starts with `5820`. The file's `type` is
not needed: both accepted key types, `PaymentSigningKeyShelley_ed25519` and
`GenesisUTxOSigningKey_ed25519`, hold the same key and serialise it the same,
and the genesis one is cast to a payment key, so the same hex decodes to the
same key either way. Anything else, an extended `..._ed25519_bip32` key for
one, fails to decode and says so.

Mixing both forms in one file is fine. A path is read once per entry that names
it, so a file of a million such entries opens a million files, while an inline
key needs none: that is the shape to use at that size.

Fields an entry does not use are ignored, so a file that carries more per entry,
an address for instance, loads as it is with no pass to strip them.

When `tx_in` is **present**, the fund uses the explicit UTxO reference:
```json
{ "signing_key": "payment.skey", "lovelace": 1000000, "tx_in": "df6...#0" }
```

When `tx_in` is **omitted**, the fund is treated as a genesis UTxO: the `TxId` is derived deterministically from the signing key's verification key hash via `genesisUTxOPseudoTxIn`, and the `TxIx` is always 0.
```json
{ "signing_key": "genesis.skey", "lovelace": 1500000000000 }
```

#### One object per line, for very large files

The loader accepts two shapes and tells them apart by the first character that
is not whitespace:

| First character | Shape | How it is read |
|-----------------|-------|----------------|
| `[` | a JSON array of fund objects | decoded whole |
| anything else | **one fund object per line** | streamed, a line at a time |

```
{"skey_cbor_hex":"5820a1b2...","lovelace":9000000000}
{"skey_cbor_hex":"5820c3d4...","lovelace":9000000000}
```

Both shapes produce the same funds; only the memory profile differs. An array
needs the file, a JSON tree of it and the funds in memory at once, which is
fine for the thousands of entries a testnet hands out. One object per line
keeps memory at the funds plus the single line in hand, whatever the number of
entries, which is what millions need. Every line has to be one fund object, a
blank one included, so line N is fund N, and an error names the line it is on.

#### `utxo_query`

Discover funds **on chain** instead of from a file. The generator queries a node (over its NodeToClient socket) for the UTxOs currently at one or more addresses. The addresses come from the use site: `initial_inputs` supplies signing keys in its `params`, a builder `recovery` queries the builder's own destination address.

`params`:
- **`socket_path`**: Path to the local node's NodeToClient socket.

The query era is detected from the node at runtime, so it follows the chain across the Shelley-based eras cardano-api supports (Shelley through Conway today).

### Initial Inputs (`initial_inputs`)

The generator requires a set of initial UTxOs, loaded through a named input source at startup. `initial_inputs` always has a **`source`** (the name of the `input_sources` entry to load from); whether it also has a **`params`** depends on that source's type. There are exactly two combinations:

**With a `genesis_utxo_keys` source** — the source is self-contained (its file holds everything), so `initial_inputs` is just the reference. Adding `params` here is a startup error:

```json
"initial_inputs": { "source": "genesis-funds" },
"input_sources": {
  "genesis-funds": {
    "type": "genesis_utxo_keys",
    "params": { "signing_keys_file": "funds.jsonl" }
  }
}
```

**With a `utxo_query` source** — the source only knows *how* to query (the socket); this use site must say *which addresses*, so `params` with **`signing_keys`** is required (omitting it is a startup error). Each listed `.skey`'s derived address is queried, and every UTxO found there becomes an initial fund tagged with that key (so it can be spent):

```json
"initial_inputs": {
  "source": "node-utxo",
  "params": { "signing_keys": ["dest.skey"] }
},
"input_sources": {
  "node-utxo": {
    "type": "utxo_query",
    "params": { "socket_path": "/run/node/node.sock" }
  }
}
```

(A builder `recovery` using the same `utxo_query` source carries no such params: it always queries the builder's own destination address.)

Loading through a `utxo_query` source makes restarts **stateless**: each builder recycles its outputs back to its `destination_signing_key` address, so pointing the `signing_keys` at those same keys re-discovers whatever a previous run left on chain. If no UTxOs are found at any queried address, the generator exits with an error rather than starting. Fund the address of at least one configured `signing_keys` entry and restart.

#### Input scope (`scope`)

`initial_inputs` takes an optional **`scope`** that says how many input queues the loaded inputs are split into:

| `scope` | Input queues | Who draws from each |
|---------|--------------|---------------------|
| `shared` (default) | One for all the workloads, holding every input | All of them: every workload takes from that queue and every recycler enqueues back into it |
| `per_workload` | One per workload, the inputs dealt round-robin over them | Only its own workload: it takes from that queue and recycles back into it |

Inputs are fungible, since a fund carries the signing key needed to spend it, so any workload can use any fund. That is why `shared` is the default: a split has to be sized for the workload that keeps the most inputs in flight, which is its rate times the confirmation latency of the cluster under test, and a config file cannot know that figure. Get it wrong and one workload starves while the inputs it needs sit in queues it cannot reach.

Pick `per_workload` when a run needs each workload's input supply to be independent of the others, and whenever a builder uses a [`recovery`](#rollback-recovery-recovery) (required, see below).

What `shared` means in practice:

- The builders compete for the same inputs. One transaction takes a whole batch, so while the queue is low a workload with a large `inputs_per_tx` keeps waiting whenever workloads with a small one can still build.
- `TxCentrifuge.Pipe.Inputs{Enqueued,Dequeued}` report the shared queue, so their `pipe` field is `@shared` instead of a workload name and their `depth` is that queue's depth. The payload events (`PayloadEnqueued`, `PayloadDequeued`) keep the workload's name, since a payload queue is never shared. The startup load arrives as one run of `InputsEnqueued` under `@shared`, a shared queue being filled once.
- A builder [`recovery`](#rollback-recovery-recovery) is a validation error once a second workload exists, so a config that uses one must then set `"scope": "per_workload"`. A recovery drops every input queued for the workload that triggered it and reseeds the queue from that workload's own UTxO query, which on a shared queue would discard the inputs of all the others and replace them with the funds of one.
- Fund sizes mix. A workload with several `outputs_per_tx` splits the value of each fund it spends, and under `shared` those smaller funds reach every other workload too, so all of them converge on the same fund sizes.
- `on_exhaustion: error` fires when the run as a whole runs out of inputs, not when one workload's share does.

```json
"initial_inputs": { "source": "genesis-funds", "scope": "per_workload" }
```

### Rate Limiting (`rate_limit`)

The `rate_limit` field can be set at the **top level** or at the **workload level** (but not both — setting it at both levels is a validation error). If omitted entirely, targets run **unlimited** (no rate ceiling).

The `scope` determines the granularity of the TPS ceiling. Available scopes depend on where the rate limit is defined:

**Top-level scopes:**
- **`shared`** (default): A single rate limiter shared by all targets across all workloads. The configured TPS is the aggregate ceiling.
- **`per_workload`**: Each workload gets its own independent rate limiter at the full configured TPS (shared by its targets).
- **`per_target`**: Every target gets its own independent rate limiter at the full configured TPS. E.g., 10 TPS with 50 targets = 500 TPS aggregate.

**Workload-level scopes:**
- **`shared`** (default): One rate limiter shared by all targets in the workload. The configured TPS is the aggregate ceiling for the workload.
- **`per_target`**: Every target in the workload gets its own independent rate limiter at the full configured TPS.

### Cascading Defaults

Most configuration fields can be set at multiple levels. The most specific value wins:

- **`builder`**: workload > top-level. Setting it at **both** levels is a validation error. At least one must be set (no default).
- **`rate_limit`**: workload > top-level > **unlimited**. Setting it at **both** levels is a validation error.
- **`max_batch_size`**: target > workload > top-level > **0 (unlimited)**.
- **`on_exhaustion`**: target > workload > top-level > **`block`**.

Workload, target, observer, and input source names must be non-empty, must not start with `@`, and must not contain `.` or `/` (`@` and `.` are reserved for internal rate-limiter cache keys and for the shared input queue's name `@shared`, `/` for the forwarder pool's wiring-path keys).

### Batching and Flow Control

- **`max_batch_size`**: Limits the number of items (e.g., transactions) the generator will announce to a target in a single protocol request. **0 means unlimited** (use whatever the node requests). Defaults to 0.
  - This acts as a safety cap: even if a target's protocol allows for 500 items, a `max_batch_size` of 100 ensures the generator doesn't commit too much capacity to a single connection at once.
  - This helps distribute the available "payload queue" more evenly across multiple targets and prevents a single aggressive node from starving others.
- **`on_exhaustion`**:
  - `block`: The worker thread waits until the builder produces a new payload.
  - `error`: The generator fails immediately if the builder cannot keep up with the requested TPS.

### Startup Delay (`startup_delay_seconds`)

An optional **top-level** field that delays the start of transmission. Waits this many seconds before the workers open their connections to the targets but after the builders are spawned and filling the payload queues.

- **Value**: a non-negative integer number of **seconds**.
- **Default**: `0` (also the effect when the field is absent or `null`). Workers connect as soon as they are spawned (no delay, no log line).
- **Scope**: top level only. It applies to the whole run and does **not** cascade to workloads or targets.

```json
{
  "rate_limit": { "type": "token_bucket", "params": { "tps": 100000 } },
  "startup_delay_seconds": 300,
  "workloads": { "...": {} }
}
```

### Tracing

The generator emits structured traces through `trace-dispatcher` (the same
library the node uses). Trace settings live in the **same config file** you pass
to `tx-centrifuge`, under a `TraceOptions` object keyed by namespace: `""` is the
root default and every other key overrides one namespace. When `TraceOptions` is
absent, a default applies that writes every namespace to **stdout** in machine
(JSON) format at `Debug` severity, so all traces are on out of the box.

```json
{
  "rate_limit": { "type": "token_bucket", "params": { "tps": 100000 } },
  "workloads": { "...": {} },

  "TraceOptions": {
    "": {
      "severity": "Info",
      "detail": "DNormal",
      "backends": ["Stdout MachineFormat"]
    },
    "TxCentrifuge.Pipe": { "severity": "Silence" },
    "TxCentrifuge.Recycler": { "maxFrequency": 1.0 },
    "TxCentrifuge.Observer": { "severity": "Silence" },
    "TxCentrifuge.Builder.NewTx": { "detail": "DMaximum" },
    "TxCentrifuge.TxSubmission": { "detail": "DDetailed" },
    "TxSubmission2": { "severity": "Silence" },
    "KeepAlive": { "severity": "Silence" }
  }
}
```

Per namespace you can set `severity` (the minimum level to emit, or `Silence` to
drop it), `detail` (`DMinimal`, `DNormal`, `DDetailed`, `DMaximum`), `backends`
(such as `Stdout MachineFormat`, `Stdout HumanFormatColoured`, or `Forwarder`),
and `maxFrequency` (a cap in messages per second). The application-level traces
(severity `Info` unless noted) are:

| Namespace | What it reports | Detail levels |
| :--- | :--- | :--- |
| `TxCentrifuge.Builder.NewTx` | A new transaction was built from input UTxOs, producing output UTxOs. Carries the builder name. Fires once per built transaction. | `DNormal` adds the transaction's `txHash` and `txId`. `DDetailed` adds the `inputs`/`outputs` as UTxO reference strings. `DMaximum` renders each fund in full (`utxo` + `lovelace`) and adds the `destinations` the tx pays to. |
| `TxCentrifuge.Pipe.Inputs.Enqueued` | Inputs were added to a pipe's input queue (initial funds or recycled ones). Carries the input queue's name (see [`scope`](#input-scope-scope)). | `DNormal` adds the resulting queue `depth` and a `count` of the inputs. `DDetailed` adds the `inputs` array as `"<txid>#<ix>"` references, which `DMaximum` renders as objects with `utxo` and `lovelace`. |
| `TxCentrifuge.Pipe.Inputs.Dequeued` | Inputs were taken off the input queue for the builder. Carries the input queue's name (see [`scope`](#input-scope-scope)). | `DNormal` adds the resulting queue `depth` and a `count` of the inputs. `DDetailed` adds the `inputs` array as `"<txid>#<ix>"` references, which `DMaximum` renders as objects with `utxo` and `lovelace`. |
| `TxCentrifuge.Pipe.Payload.Enqueued` | A payload was added to the bounded payload queue. Carries the pipe name. | `DNormal` adds the resulting queue `depth`. `DDetailed` adds the payload's `txHash` and `txId` (payload events carry no `count`, one payload per event). |
| `TxCentrifuge.Pipe.Payload.Dequeued` | A payload was pulled off the payload queue by a worker. Carries the pipe name. | `DNormal` adds the observed queue `depth`. `DDetailed` adds the payload's `txHash` and `txId` (payload events carry no `count`, one payload per event). |
| `TxCentrifuge.Recycler.Backlog.Held` | A payload's entry entered the recycler's backlog: its key and its two input sets, held until a release picks one. Carries the recycler name. | `DNormal` adds the resulting `backlog` (the number of payloads held but not yet released), a `consumed_count` and an `outputs_count`. `DDetailed` adds the payload's `txHash` and `txId` and the `consumed` and `outputs` arrays as references, which `DMaximum` renders as objects with `utxo` and `lovelace`. |
| `TxCentrifuge.Recycler.Backlog.Released.Confirmed` | The chain confirmed a held payload, so its outputs went back onto a pipe's input queue (via `Pipe.addInputs`), closing the loop. **This generator's own transactions only**: a release happens only for a key the backlog held, which is why no filter is needed here and why `Observer.Announce.Confirmed` can be silenced outright. Carries the recycler name and the input queue's name. | `DNormal` adds the resulting `backlog` and a `count` of the recycled inputs. `DDetailed` adds the released payload's `txHash` and `txId`, the `blockNo` and `slotNo` it was confirmed in, and the `inputs` array as references (objects at `DMaximum`). |
| `TxCentrifuge.Recycler.Backlog.Released.Orphaned` | The same for a held payload whose block was rolled back: its consumed inputs went back instead of its outputs, and `blockNo`/`slotNo` are the rolled-back block's. Only `on_confirm` releases that set. Severity `Warning`. | Same fields as `Confirmed`. |
| `TxCentrifuge.Recycler.Backlog.Released.Optimistic` | The same for a release with no chain verdict at all, which is `on_build` (at build) and `on_pull` (at dequeue). | Same fields as `Confirmed`, without `blockNo` and `slotNo`: nothing saw the transaction on the chain. |
| `TxCentrifuge.Recycler.Reset` | The recycler reset a pipe for a builder's `recovery`: the queued inputs and the queued payloads built from them were dropped, and the input queue was reseeded from chain truth. Carries the recycler name and the input queue's name. Severity `Warning`. | `DNormal` adds the resulting `backlog` (always 0, a reset clears the backlog), a `fresh_count`, a `dropped_inputs_count` and a `dropped_payloads_count`. `DDetailed` adds the `fresh` and `dropped_inputs` arrays as references (objects at `DMaximum`) and the `dropped_payloads` array of payload hashes. |
| `TxCentrifuge.Observer.Announce.Confirmed` | The observer saw a transaction confirmed, carrying the observer name and the transaction's identifiers. Fires only for confirmation-based workloads, and there it is currently **unfiltered**: it reports every transaction in the confirmed blocks its node sees, not just this generator's, so one observer's volume is roughly the whole chain's throughput and a run with one observer per node multiplies that by the number of nodes. For this generator's own transactions only, use `Recycler.Backlog.Released.*` instead and set this to `Silence`. | `observer` at every level, the identifiers from `DNormal`, the `blockNo` and `slotNo` it was seen in from `DDetailed`. |
| `TxCentrifuge.Observer.Announce.Orphaned` | The same for a transaction that was in a rolled-back block and did not reappear within the confirmation window. Rare, and a `Warning`: separate from `Confirmed` so the flood can be silenced while this is kept. | Same fields as `Confirmed`, where `blockNo` and `slotNo` are the rolled-back block's. |
| `TxCentrifuge.TxSubmission.RequestTxIds` | The node requested TxId announcements (blocking or non-blocking). Carries the `target` node. | `DNormal` adds the ACK and REQ counts. `DMaximum` adds the list of currently unacked TxIds. |
| `TxCentrifuge.TxSubmission.ReplyTxIds` | We replied with TxId announcements and their sizes. Carries the `target`. | `DNormal` adds the ACK and REQ counts of the request it answers, echoed from it, so REQ against the announced count is unmet demand. `DDetailed` adds the announced TxIds with their sizes. `DMaximum` adds the updated unacked list. |
| `TxCentrifuge.TxSubmission.RequestTxs` | The node requested full transactions by TxId. Carries the `target`. | `DMaximum` adds the list of requested TxIds. |
| `TxCentrifuge.TxSubmission.ReplyTxs` | We sent the requested transactions. Carries the `target`. | `DDetailed` adds the sent TxIds with their sizes. `DMaximum` adds the list the node requested. |

Every `TxSubmission` trace carries a `target` field naming the remote node, so
submission activity can be attributed per target. The `Pipe`, `Recycler`, and
`Observer` traces fire on roughly every transaction. Each `Pipe` event carries
its queue `depth` and each `Recycler` event its `backlog` count (the number of
payloads held but not yet released) on every event and at every detail level.
Raising `detail` to `DDetailed` adds counts of the items involved (input and
recycler events only, since pipe payload events carry no count), and `DMaximum`
adds the items themselves (the funds, or a payload's `txId`). At
high TPS that is a firehose, so you will usually want to silence them or cap them
with `maxFrequency`. Because a `maxFrequency` cap samples a namespace at a fixed
rate, setting it on these depth-carrying traces gives a periodic depth readout
(the equivalent of a periodic queue-depth tracer) straight from config, with no
code change. The `Observer` trace is unfiltered today (see the table above), so
it too runs at roughly chain throughput. Below the application traces, the raw
`ouroboros-network`
protocol is traced verbosely under the `TxSubmission2` and `KeepAlive`
namespaces.

## Cardano Implementation (`tx-centrifuge`)

### Value Builder Parameters

These parameters define the **transaction profile** for a workload:
- `inputs_per_tx` / `outputs_per_tx`: Controls the transaction structure (size and complexity).
- `fee`: Fixed Lovelace fee per transaction.
- `destination_signing_key` (optional): Path to a `.skey` file (a payment or genesis UTxO key, like the `signing_key` entries in the fund file) whose address receives every output this builder produces and which spends the recycled UTxOs. When omitted, a built-in per-builder key is derived instead. Supplying your own key lets you fund and inspect a known address, which is printed to stderr at startup. Note that with a single destination every recycled input shares one key, so steady-state transactions carry **one witness** regardless of `inputs_per_tx` (smaller than the initial batch built from distinct genesis keys).
- `destination_signing_keys` (optional): Like `destination_signing_key` but a **list** of `.skey` paths. Outputs are distributed round-robin **by output index within each transaction**: output `i` pays to key `i mod n`, where `n` is the number of keys in the list, and each recycled fund keeps its destination's key. This makes multi-witness steady-state transactions *possible*, not guaranteed: a batch's witness count is however many distinct keys its inputs happen to carry, and that depends on queue order. With `inputs_per_tx = 2`, `outputs_per_tx = 2` and two keys, each transaction's A/B output pair re-enters the queue together, so batches typically alternate keys and carry two witnesses; but nothing enforces the alignment — a `recovery` reseed returns funds grouped by address (same-key stretches until the interleave re-forms), an `outputs_per_tx` that is not a multiple of `n` leaves same-key neighbours in the queue, and with `outputs_per_tx = 1` the index restarts at 0 every transaction so keys beyond the first are never used at all. Verify the achieved witness mix in the traces (tx sizes, or `Builder.NewTx` at `DMaximum`) rather than assuming it. All addresses are announced at startup (one line per builder), and a builder `recovery` queries all of them. Setting both the singular and the plural form is a startup error.

A two-witness builder for a 2-in/2-out profile:

```json
"builder": {
  "type": "value",
  "params": {
    "inputs_per_tx": 2,
    "outputs_per_tx": 2,
    "fee": 1000000,
    "destination_signing_keys": ["dest-a.skey", "dest-b.skey"]
  },
  "recycle": { "type": "on_confirm", "params": "my-observer" }
}
```

Each transaction pays output 0 to `dest-a`'s address and output 1 to
`dest-b`'s; the recycled pair re-enters the input queue together, so the next
2-input batch typically spends one UTxO per key (typically, not guaranteed;
see the parameter description above for how the alignment can break). With a
single destination (or the derived built-in key) the same builder's
steady-state transactions always collapse to one witness and shrink
accordingly (e.g. 270 vs 371 bytes for 2-in/2-out). Startup announces:

```
Builder group-A: destination addresses addr_test1..., addr_test1...
```
- `recycle` (optional): Controls when output UTxOs are returned to the input queue. See [Resource Recycling](#resource-recycling) for the three strategies (`on_build`, `on_pull`, `on_confirm`). When omitted, outputs are **not recycled** — the generator consumes initial funds and eventually exhausts them.
- `recovery` (optional): Rollback recovery for this builder, resetting its input queue from an input source when an observer reports one of its transactions orphaned. With more than one workload, requires [`"scope": "per_workload"`](#input-scope-scope). Requires `recycle`. See [Rollback recovery](#rollback-recovery-recovery).

## Usage

```bash
tx-centrifuge config.json            # run the generator
tx-centrifuge --dry-run config.json  # validate config + funds, then exit 0
```

With `--dry-run`, tx-centrifuge performs all startup validation (config parsing, consensus protocol setup, and on-chain fund discovery) and exits before creating any pipelines or generating traffic. Use it to check a config and its funding (for example after a restart or fresh funding) without producing load.

## Detailed Examples

### 1. High-Throughput (On-Build Recycling)

Optimized for maximum TPS using simple 1-in/1-out transactions. Outputs are recycled immediately after building (`on_build`), before the transaction enters the payload queue.

**`config.json` snippet:**
```json
{
  "initial_inputs": { "source": "genesis-funds" },
  "input_sources": {
    "genesis-funds": {
      "type": "genesis_utxo_keys",
      "params": {
        "signing_keys_file": "funds.1.jsonl"
      }
    }
  },
  "builder": {
    "type": "value",
    "params": {
      "inputs_per_tx": 1,
      "outputs_per_tx": 1,
      "fee": 1000000
    },
    "recycle": { "type": "on_build" }
  },
  "rate_limit": {
    "type": "token_bucket",
    "scope": "shared",
    "params": { "tps": 1000 }
  },
  "workloads": {
    "simulation": {
      "targets": {
        "node-0": { "addr": "127.0.0.1", "port": 30000 }
      }
    }
  },
  "nodeConfig": "node-config.json"
}
```

**`funds.1.jsonl` snippet:**
```
{"signing_key": "utxo1.skey", "lovelace": 1500000000000}
{"signing_key": "utxo2.skey", "lovelace": 1500000000000}
```

### 2. Large Transactions (Target-Specific Limits)

Uses complex transactions with independent rate limits for each target connection. Outputs are recycled on fetch (`on_pull`).

**`config.json` snippet:**
```json
{
  "initial_inputs": { "source": "genesis-funds" },
  "input_sources": {
    "genesis-funds": {
      "type": "genesis_utxo_keys",
      "params": {
        "signing_keys_file": "funds.2.jsonl"
      }
    }
  },
  "builder": {
    "type": "value",
    "params": {
      "inputs_per_tx": 5,
      "outputs_per_tx": 5,
      "fee": 2000000
    },
    "recycle": { "type": "on_pull" }
  },
  "rate_limit": {
    "type": "token_bucket",
    "scope": "per_target",
    "params": { "tps": 5 }
  },
  "max_batch_size": 50,
  "on_exhaustion": "block",
  "workloads": {
    "heavy-load": {
      "targets": {
        "edge-node": { "addr": "192.168.1.10", "port": 30001 }
      }
    }
  },
  "nodeConfig": "node-config.json"
}
```

**`funds.2.jsonl` snippet:**
```
{"signing_key": "utxo1.skey", "lovelace": 1000000000}
{"signing_key": "utxo2.skey", "lovelace": 1000000000}
{"signing_key": "utxo3.skey", "lovelace": 1000000000}
```

### 3. Confirmation-Based Recycling (On-Confirm with Observer)

Uses an observer that follows the chain to track when submitted transactions are confirmed on-chain. Outputs are only recycled back to the input queue after the transaction reaches a configured confirmation depth, protecting against mempool eviction and short rollbacks. Rolled-back transactions are held in limbo and orphaned (original inputs recycled) if they do not reappear within 2×`confirmation_depth` blocks.

Two observer types are supported:

#### `nodetonode` — N2N ChainSync + BlockFetch

Connects to a remote node over TCP. The observer follows the chain via ChainSync (headers) and fetches block bodies via BlockFetch to extract transaction IDs.

```json
"observers": {
  "chain-follower": {
    "type": "nodetonode",
    "params": {
      "addr": "127.0.0.1",
      "port": 30000,
      "confirmation_depth": 2
    }
  }
}
```

| Field                | Type   | Description |
|----------------------|--------|-------------|
| `addr`               | string | IP address of the node |
| `port`               | int    | Node-to-node port |
| `confirmation_depth` | int    | Blocks to wait before confirming (0 = immediate) |

#### `nodetoclient` — N2C LocalChainSync

Connects to the local node over a Unix domain socket. The observer follows the chain via LocalChainSync, which delivers full blocks directly, with no separate BlockFetch needed. It is the simpler of the two under Leios, because the node inlines a certified endorser block's transactions into the block it serves, so nothing Leios-specific is needed on this side.

```json
"observers": {
  "local-follower": {
    "type": "nodetoclient",
    "params": {
      "socket_path": "/tmp/node.socket",
      "confirmation_depth": 2
    }
  }
}
```

| Field                | Type   | Description |
|----------------------|--------|-------------|
| `socket_path`        | string | Path to the node's local Unix domain socket |
| `confirmation_depth` | int    | Blocks to wait before confirming (0 = immediate) |

#### Choosing between the two

Both emit the same `TxRef` events on the same broadcast channel type, so the downstream recycler and workloads are unaffected by the choice. A transaction is reported by the **Blake2b-256 hash of its serialised bytes**, which is the identifier every source has, with its **transaction id** alongside whenever it was read from a block body, and with the **block it was read in** so that whoever traces the ref can say where it landed. Only transactions read from an endorser block come back without an id, which is the `nodetonode` observer under Leios, since an endorser block lists hashes and no ids. The builder references the observer by name regardless of type:

```json
"recycle": { "type": "on_confirm", "params": "local-follower" }
```

| Concern              | `nodetonode`                                    | `nodetoclient`                |
|----------------------|-------------------------------------------------|-------------------------------|
| Transport            | TCP (any reachable node)                        | Unix socket (co-located node) |
| Under Leios          | Fetches each certified endorser block itself    | Node serves merged blocks     |
| Protocols on the mux | ChainSync + BlockFetch + KeepAlive + LeiosFetch | ChainSync only                |
| Requires local node  | No                                              | Yes                           |
| Sees the chain       | As diffused                                     | As the local node resolved it |

The `nodetonode` observer's LeiosFetch client is built only with the `leios-prototype` flag, since the mini-protocol exists only in the consensus that branch pins. Without it that observer sees ranking blocks alone, which on a Leios chain means it never sees the transactions a certifying block puts there.

#### Full example (`nodetoclient`)

```json
{
  "initial_inputs": { "source": "genesis-funds" },
  "input_sources": {
    "genesis-funds": {
      "type": "genesis_utxo_keys",
      "params": {
        "signing_keys_file": "funds.3.jsonl"
      }
    }
  },
  "observers": {
    "local-follower": {
      "type": "nodetoclient",
      "params": {
        "socket_path": "/tmp/node.socket",
        "confirmation_depth": 2
      }
    }
  },
  "builder": {
    "type": "value",
    "params": {
      "inputs_per_tx": 2,
      "outputs_per_tx": 2,
      "fee": 1000000
    },
    "recycle": { "type": "on_confirm", "params": "local-follower" }
  },
  "rate_limit": {
    "type": "token_bucket",
    "scope": "shared",
    "params": { "tps": 50 }
  },
  "max_batch_size": 500,
  "on_exhaustion": "error",
  "workloads": {
    "confirmed-load": {
      "targets": {
        "node-0": { "addr": "127.0.0.1", "port": 30000 }
      }
    }
  },
  "nodeConfig": "node-config.json"
}
```

**`funds.3.jsonl` snippet:**
```
{"signing_key": "utxo1.skey", "lovelace": 1500000000000}
{"signing_key": "utxo2.skey", "lovelace": 1500000000000}
{"signing_key": "utxo3.skey", "lovelace": 1500000000000}
```

With `on_confirm`, the generator needs enough initial funds to cover the in-flight transactions between submission and confirmation. At 50 TPS with a 2-block confirmation depth (~40 seconds on a 20-second slot), roughly 2000 transactions will be pending at any time, so the initial fund pool should have at least that many UTxOs.

## Internals

### Package Structure

```
tx-centrifuge/
├── tx-centrifuge.cabal          # Package definition
├── app/
│   └── Main.hs                  # Executable entry point
├── lib/
│   ├── pull-fiction/            # Domain-independent load generation library
│   │   └── PullFiction/
│   │       ├── Config/
│   │       │   ├── Raw.hs           # JSON parsing (no validation)
│   │       │   ├── Validated.hs     # Validation + cascading defaults
│   │       │   └── Runtime.hs       # Resolves config into named pools + limiters
│   │       ├── Clock.hs             # Monotonic time source
│   │       ├── WorkloadRunner.hs    # Rate-limited per-target workers
│   │       └── Internal/
│   │           ├── Pipe.hs          # Generic input + payload queue pair
│   │           ├── RateLimiter.hs   # GCRA token bucket
│   │           └── Recycler.hs      # Closed-loop input recycling (worker, strategy-free)
│   │
│   ├── cardano/                 # Cardano-specific library
│   │   └── Cardano/Benchmarking/TxCentrifuge/
│   │       ├── Chain.hs             # The chain as the generator sees it:
│   │       │                        # transaction identity, block types,
│   │       │                        # extraction, the consensus boundary
│   │       ├── Fund.hs              # UTxO/fund loading from JSON
│   │       ├── NodeToClient.hs      # Multiplexed N2C connection (local socket)
│   │       ├── NodeToClient/
│   │       │   ├── TxIdSync.hs      # LocalChainSync tx confirmation (full blocks)
│   │       │   ├── TxSubmission.hs  # LocalTxSubmission client
│   │       │   └── UTxOQuery.hs     # LocalStateQuery UTxO discovery
│   │       ├── NodeToNode.hs        # Multiplexed N2N connection (TCP)
│   │       ├── NodeToNode/
│   │       │   ├── KeepAlive.hs     # KeepAlive mini-protocol client
│   │       │   ├── TxIdSync.hs      # ChainSync + BlockFetch tx confirmation
│   │       │   └── TxSubmission.hs  # TxSubmission2 mini-protocol client
│   │       ├── TxAssembly.hs        # Transaction building and signing
│   │       └── Tracing.hs           # Trace events + contra-tracer bundle
│   │
│   └── cardano-tracing/         # trace-dispatcher rendering of those traces
│       └── Cardano/Benchmarking/TxCentrifuge/Tracing/
│           └── Dispatcher.hs        # LogFormatting/MetaTrace + tracer setup
│
├── test/                        # One test suite per library
│   ├── cardano/                 # cardano unit tests
│   ├── pull-fiction/            # pull-fiction unit tests
│   └── pull-fiction-harness/    # Shared test harness (private library)
│       └── Test/PullFiction/
│           └── Harness.hs
│
└── bench/
    └── pull-fiction/            # pull-fiction benchmarks
        └── Bench.hs
```

### Data Flow

```
Raw JSON → Validated Config → Runtime (STM queues, rate limiters, builder asyncs)
                                        │
         ┌──────────────────────────────┘
         ↓
   [Builder Async]         per workload
     reads TQueue(inputs) → buildTx → TBQueue(payloads, 8192 cap)
         ↓
   [Worker Asyncs]         per target
     GCRA rate-limited fetch → TxSubmission2 pull protocol → cardano-node
         ↓
   [Recycler]              closed-loop
     outputs → back to TQueue(inputs)
```
