{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------

-- | The chain as this generator sees it: how a transaction is identified, what
-- a block is, how its transactions are read out of it, which era it is in, and
-- the crossing to the @ouroboros-consensus@ types the mini-protocols speak.
--
-- It is also the translation layer between @cardano-api@ and
-- @ouroboros-consensus@ types, which is why those crossings live here.
--
-- The benchmarking tool uses @cardano-api@ types internally (@TxId@, @Api.Tx@)
-- because they are era-agnostic and simple to work with. The
-- Ouroboros mini-protocols (TxSubmission2, ChainSync, BlockFetch) speak
-- @ouroboros-consensus@ types (@GenTx@, @GenTxId@, @CardanoBlock@), which are
-- era-indexed sum types with one constructor per era. This module is the
-- single translation point between the two:
--
-- * Outbound (submitting): @Api.Tx@ → @GenTx@ via 'toGenTx'.
-- * Inbound (node replies): @GenTxId@ → 'TxId' via 'fromGenTxId'.
-- * Inbound (chain-following): @CardanoBlock@ → @['TxRef']@ via
--   'extractTxRefs', which also reaches into @cardano-ledger@ to unwrap raw
--   block bodies (ideally @cardano-api@ would provide this, but it currently
--   does not).
--
-- Centralising all boundary crossings here keeps the rest of the codebase
-- unaware of the consensus type machinery.
--
-- == Two kinds of block
--
-- Everything named @Block@ or @CardanoBlock@ here is a __ranking block__ (RB):
-- the block Praos produces, which carries the chain and its transaction bodies.
-- That is the only kind of block before Leios, and the only one ChainSync and
-- BlockFetch ever deliver.
--
-- Leios adds a second kind, the __endorser block__ (EB), which is not on the
-- chain and carries no transaction bodies, only their hashes. A ranking block
-- may announce an EB and the next ranking block may certify it; one that carries
-- a certificate is a __CertRB__, and the same block usually certifies the
-- previous EB while announcing the next. The functions below name which kind
-- they mean; a bare "block" is a ranking block.
module Cardano.Benchmarking.TxCentrifuge.Chain
  ( -- * Transaction identity.
    -- | Two hashes identify a transaction and they hash different things, so
    -- both come from here rather than one from @cardano-api@ and one from this
    -- module.
    --
    -- * 'TxId' hashes the transaction /body/ alone: its inputs, outputs, fee,
    --   validity interval, certificates and the hashes of its auxiliary parts,
    --   but none of the witnesses. It is what the ledger names a transaction
    --   by, so it is half of every @TxIn@ (an output is @(TxId, TxIx)@) and
    --   what a recycled fund has to carry. Signing the same body differently
    --   leaves it unchanged.
    --
    -- * 'TxHash' hashes the /whole serialised transaction/: the body, the
    --   witnesses and the auxiliary data, the exact bytes that were submitted
    --   and diffused. Two transactions with one body and different witnesses
    --   share a 'TxId' and differ here.
    --
    -- An endorser block is a list of @(TxHash, size)@ references and carries no
    -- bodies, so it commits to the bytes a peer will serve and there is no
    -- 'TxId' in it. Nor can one be derived: that needs the body, which is what
    -- the EB deliberately leaves out. Hence 'TxRef' is the hash plus the id
    -- when its source had one, which is why a ref read from an endorser block
    -- has no 'txRefId'.
    TxId, TxHash
    -- A transaction as the source that gave it to us knows it. Only 'Built' is
    -- exported, the shape with no chain position, so anything may make one off
    -- chain; the chain-read shapes are made in this module alone, which is
    -- what makes a ref's block worth trusting.
  , TxRef (Built)
  , txRefHash, txRefId, txRefBlock
    -- * Block type.
  , CardanoBlock
  , BlockNo
  , SlotNo
    -- * Era check.
  , eraMismatch
    -- * Transaction extraction.
  , extractTxRefs
  , hashTxBytes
    -- * Protocol boundary.
  , toGenTx
  , fromGenTxId
#ifdef LEIOS_PROTOTYPE
    -- * Leios endorser blocks.
    -- The Leios types themselves are not re-exported: callers that name them
    -- import "LeiosDemoTypes" qualified, as this module does.
  , leiosAnnouncedPoint
  , leiosCarriesCert
  , leiosEbTxRefs
#endif
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Foldable (toList)
import Data.Functor.Const (Const (..))
import Data.Ord (comparing)
----------------
-- bytestring --
----------------
import Data.ByteString qualified as BS
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
--------------------------
-- cardano-crypto-class --
--------------------------
import Cardano.Crypto.Hash qualified as Hash
--------------------
-- cardano-ledger --
--------------------
import Cardano.Ledger.Binary.Plain qualified as Plain
import Cardano.Ledger.Block qualified as LedgerBlock
import Cardano.Ledger.Core qualified as Core
---------------------------------
-- ouroboros-consensus:cardano --
---------------------------------
import Ouroboros.Consensus.Cardano.Block qualified as Cardano
import Ouroboros.Consensus.Shelley.Eras qualified as Eras
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.Mempool qualified as Mempool
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import Ouroboros.Consensus.Block qualified as Block
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as SupportsMempool
#ifdef LEIOS_PROTOTYPE
import LeiosDemoTypes qualified as Leios
import Ouroboros.Consensus.Storage.LedgerDB.Forker qualified as Forker
#endif
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Era (Era, era, eraName)

--------------------------------------------------------------------------------
-- Transaction identity.
--------------------------------------------------------------------------------

-- | @cardano-api@\'s transaction id, Blake2b-256 of the transaction body
-- alone. Aliased here so that both identifiers have one home, see the section
-- header for what each one hashes.
type TxId = Api.TxId

-- | Blake2b-256 of the whole serialised transaction, which is how an endorser
-- block lists the transactions it carries. @LeiosDemoTypes@ calls it a
-- @TxHash@, see the section header for how it differs from 'TxId'.
type TxHash = Hash.Hash Hash.Blake2b_256 BS.ByteString

-- | A transaction as one source knows it: always its 'TxHash', and whatever
-- else that source had.
--
-- The hash and not the id, because it is the only identifier every source has:
-- an endorser block lists hashes and carries no bodies, so there is no id in
-- it and none can be derived, that needing the body the EB leaves out.
--
-- Three sources, so three shapes. Each names what it knows instead of leaving
-- a reader to work out which combinations occur.
data TxRef
  = -- | Built here ('TxAssembly.buildTx'): both identifiers, and no block,
    -- nothing having seen the transaction anywhere yet.
    Built !TxHash !TxId
    -- | Read from a ranking block's body ('extractTxRefs'): both identifiers
    -- and the block it was read in.
  | InBody !TxHash !TxId !BlockNo !SlotNo
    -- | Read from an endorser (Leios) block ('leiosEbTxRefs'): the hash, and
    -- the /certifying/ ranking block, which is where an EB's transactions
    -- enter the chain.
  | InEb !TxHash !BlockNo !SlotNo
  deriving Show

-- | The hash, which every shape has.
txRefHash :: TxRef -> TxHash
txRefHash = \case
  Built  txHash _     -> txHash
  InBody txHash _ _ _ -> txHash
  InEb   txHash _ _   -> txHash

-- | The transaction id, when the source had the body it is derived from.
txRefId :: TxRef -> Maybe TxId
txRefId = \case
  Built  _ txId     -> Just txId
  InBody _ txId _ _ -> Just txId
  InEb   _ _ _      -> Nothing

-- | The ranking block this was read in, when it was read from the chain.
txRefBlock :: TxRef -> Maybe (BlockNo, SlotNo)
txRefBlock = \case
  Built  _ _                -> Nothing
  InBody _ _ blockNo slotNo -> Just (blockNo, slotNo)
  InEb   _   blockNo slotNo -> Just (blockNo, slotNo)

-- | By 'txRefHash' alone, the only field every shape has. The hash is taken
-- over the serialised transaction, which contains the body, so equal hashes
-- mean the same transaction: a ref read from an endorser block and one read
-- from a body are equal when they are that transaction, which is what lets one
-- backlog lookup serve both.
--
-- So '==' ignores everything the source happened to know beyond the hash. That
-- is what makes a 'Built' ref equal to the 'InBody' or 'InEb' ref that settles
-- it, which is the match the recycler's backlog is keyed on, and why 'Ord'
-- below orders on what is always present.
instance Eq TxRef where
  a == b = txRefHash a == txRefHash b

instance Ord TxRef where
  compare = comparing txRefHash

--------------------------------------------------------------------------------
-- Block type.
--------------------------------------------------------------------------------

-- | The Cardano ranking block (RB) type used throughout the benchmarking tool.
-- An endorser block is a @LeiosEb@ and is not this type.
type CardanoBlock = Cardano.CardanoBlock Eras.StandardCrypto

-- | A block's height and slot, aliased from @ouroboros-consensus@ so that a
-- caller naming a block's coordinates takes them from here along with
-- 'CardanoBlock' and 'TxRef'.
type BlockNo = Block.BlockNo

-- | See 'BlockNo'.
type SlotNo = Block.SlotNo

--------------------------------------------------------------------------------
-- Era check.
--------------------------------------------------------------------------------

-- | Why a chain in this block\'s era cannot be observed by this generator, or
-- 'Nothing' when the two agree.
--
-- An era decodes the era before it for backwards compatibility, so a
-- transaction built one era behind is accepted and included, but it is stored
-- and re-encoded in the era in force. Its 'TxId' survives that, the body\'s
-- bytes being kept as they arrived, and its 'TxHash' does not: the chain then
-- reports a hash the generator never computed, no observed key matches a
-- payload, and nothing is ever recycled while the observer keeps announcing.
-- Worth reporting on the first block rather than leaving in a log.
eraMismatch :: CardanoBlock -> Maybe String
eraMismatch block
  | blockEra == eraName = Nothing
  | otherwise           = Just $
         "the chain is in the " ++ blockEra ++ " era while this generator"
      ++ " builds " ++ eraName ++ " transactions, whose hashes the chain"
      ++ " will not report (see Cardano.Benchmarking.TxCentrifuge.Era)"
  where
    blockEra = case block of
      Cardano.BlockByron    _ -> "Byron"
      Cardano.BlockShelley  _ -> "Shelley"
      Cardano.BlockAllegra  _ -> "Allegra"
      Cardano.BlockMary     _ -> "Mary"
      Cardano.BlockAlonzo   _ -> "Alonzo"
      Cardano.BlockBabbage  _ -> "Babbage"
      Cardano.BlockConway   _ -> "Conway"
      Cardano.BlockDijkstra _ -> "Dijkstra"

--------------------------------------------------------------------------------
-- Transaction extraction.
--------------------------------------------------------------------------------

-- | The 'TxRef' of every transaction in a ranking block\'s body, in order.
-- Both identifiers, the body carrying the transactions themselves. What a
-- chain follower reports.
--
-- A certifying ranking block (CertRB) has an empty body, so this finds nothing
-- for it: its transactions are the certified endorser block\'s, reached through
-- 'leiosAnnouncedPoint' and a LeiosFetch request.
--
-- The 'TxHash' is taken over the transaction re-encoded from the body. That
-- reproduces the submitted bytes for anything this generator built, as both
-- sides serialise the same ledger value with the same encoder, which is also
-- the encoder a forging node hashes an endorser block entry with.
extractTxRefs :: CardanoBlock -> [TxRef]
extractTxRefs block =
  -- This block's coordinates, read once and given to every ref it yields.
  let blockNo = Block.blockNo   block
      slotNo  = Block.blockSlot block
  in case block of
       -- Byron era: skip (different tx format, not relevant for benchmarking).
       Cardano.BlockByron _ -> []
       -- Shelley-based eras.
       Cardano.BlockShelley  blk -> extractFromShelleyBlock blockNo slotNo blk
       Cardano.BlockAllegra  blk -> extractFromShelleyBlock blockNo slotNo blk
       Cardano.BlockMary     blk -> extractFromShelleyBlock blockNo slotNo blk
       Cardano.BlockAlonzo   blk -> extractFromShelleyBlock blockNo slotNo blk
       Cardano.BlockBabbage  blk -> extractFromShelleyBlock blockNo slotNo blk
       Cardano.BlockConway   blk -> extractFromShelleyBlock blockNo slotNo blk
       Cardano.BlockDijkstra blk -> extractFromShelleyBlock blockNo slotNo blk

-- | 'extractTxRefs' for a Shelley-based ranking block.
--
-- "Shelley-based" is the block /shape/, not the era. @ShelleyBlock proto era@
-- is the format introduced at Shelley and still used by every later era, so
-- @ShelleyBlock (Praos c) DijkstraEra@ is a Dijkstra block. 'CardanoBlock' is a
-- sum over the era list whose first arm is Byron and whose other seven arms are
-- all @ShelleyBlock@s, which is why one function serves them all: they share
-- the body shape this reads.
--
-- The two axes are unrelated. Shelley-based says which format and which ledger
-- era\'s rules a block follows; ranking versus endorser says what role it plays
-- under Leios. Every block on the Praos chain is a ranking block, Byron ones
-- included, and only Dijkstra ranking blocks can carry Leios fields.
extractFromShelleyBlock
  :: Core.EraBlockBody ledgerEra
  => BlockNo
  -> SlotNo
  -> Shelley.ShelleyBlock proto ledgerEra
  -> [TxRef]
-- The key is built inline rather than through a @where@ binding: as a binding
-- it is generalised over the transaction's level, and its inferred context then
-- carries a constraint on a type family application that Haskell2010 cannot
-- write. Inline, the level is whatever 'Core.txSeqBlockBodyL' yields and
-- nothing is generalised.
--
-- Every 'InBody' field is strict, which is what keeps a ref from retaining the
-- transaction and the block it was read from: a ref can sit in an observer's
-- limbo for twice the confirmation depth, and an unevaluated id or height
-- would hold that whole decoded block alive for as long.
extractFromShelleyBlock blockNo slotNo shelleyBlock =
  case Shelley.shelleyBlockRaw shelleyBlock of
    LedgerBlock.Block _ body ->
      let txSeq = getConst (Core.txSeqBlockBodyL Const body)
      in [ InBody
             (hashTxBytes (Plain.serialize' tx))
             (Api.fromShelleyTxId (Core.txIdTx tx))
             blockNo
             slotNo
         | tx <- toList txSeq
         ]

-- | Hash a transaction\'s serialised bytes into a 'TxHash'.
--
-- The preimage is the transaction\'s own CBOR, the bytes a mempool submission
-- carries. An EB entry is built by hashing @toCBOR@ of the ledger transaction
-- under @Blake2b_256@ and recording @BS.length@ of those same bytes, and
-- cardano-api\'s 'Api.serialiseToCBOR' is @Plain.serialize'@ of that very
-- value, so the two agree byte for byte. The recorded length is what makes
-- the size a usable cross-check should the hashes ever stop matching: an EB
-- entry whose size matches a submitted transaction while its hash does not
-- means the preimage changed.
hashTxBytes :: BS.ByteString -> TxHash
hashTxBytes = Hash.hashWith id

--------------------------------------------------------------------------------
-- Protocol boundary.
--------------------------------------------------------------------------------

-- | Convert a cardano-api transaction to the consensus 'Mempool.GenTx' type.
-- This is the single point where we cross from cardano-api types to
-- ouroboros-consensus types.
toGenTx :: Api.Tx Era -> SupportsMempool.GenTx CardanoBlock
toGenTx tx = Api.toConsensusGenTx $ Api.TxInMode era tx

-- | Convert a consensus 'Mempool.GenTxId' to a 'TxId'.
--
-- All Shelley-based eras use the same 'Mempool.ShelleyTxId' wrapper, so a
-- single 'Api.fromShelleyTxId' covers every post-Byron era.
fromGenTxId :: SupportsMempool.GenTxId CardanoBlock -> TxId
fromGenTxId (Cardano.GenTxIdByron _) =
  error "fromGenTxId: Byron transactions not supported"
fromGenTxId (Cardano.GenTxIdShelley  (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdAllegra  (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdMary     (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdAlonzo   (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdBabbage  (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdConway   (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i
fromGenTxId (Cardano.GenTxIdDijkstra (Mempool.ShelleyTxId i)) =
  Api.fromShelleyTxId i

#ifdef LEIOS_PROTOTYPE

--------------------------------------------------------------------------------
-- Leios endorser blocks.
--------------------------------------------------------------------------------

-- | The endorser block (EB) this ranking block's header announces, if any.
--
-- An announced EB is not yet part of the chain. Only the /next/ ranking block
-- can certify it, by carrying a Leios certificate ('leiosCarriesCert'), and
-- only then do the EB's transactions count. Its transactions are in no ranking
-- block's body either way, so 'extractTxRefs' never finds them: they are fetched
-- over the LeiosFetch mini-protocol.
--
-- Only the Dijkstra era announces anything; every earlier era returns 'Nothing',
-- so a pre-Leios chain costs one pattern match per ranking block.
leiosAnnouncedPoint :: CardanoBlock -> Maybe Leios.LeiosPoint
leiosAnnouncedPoint =
  fmap fst . Forker.headerLeiosAnnouncement . Block.getHeader

-- | Whether this ranking block carries a Leios certificate, making it a CertRB.
--
-- A CertRB certifies the endorser block that its /parent/ ranking block
-- announced ('leiosAnnouncedPoint'), and only a CertRB puts an EB's
-- transactions on the chain. It carries none of its own: a certifying block is
-- forged with an empty transaction sequence, so the EB is its only source.
-- Announcing alone does not empty a block.
--
-- A ranking block cannot say that an EB was /rejected/. The header's Leios
-- extension is two fields, @containsCert :: Bool@ and
-- @ebAnnouncement :: StrictMaybe EbAnnouncement@, so not certifying is just the
-- bit being unset. An announcement dies by being followed by a ranking block
-- that does not certify it, and the protocol state then drops it:
-- @praosStateLeiosAnnouncement@ is assigned from every new header, so a header
-- carrying no announcement clears it. Only the immediately-previous
-- announcement is ever certifiable.
leiosCarriesCert :: CardanoBlock -> Bool
leiosCarriesCert = Forker.headerContainsLeiosCert . Block.getHeader

-- | The transactions an endorser block lists, in order, as 'InEb' refs read in
-- the ranking block the caller passes: the one that certified this EB, which
-- is where its transactions enter the chain.
--
-- An EB carries hashes and sizes, never transaction bodies, so those hashes
-- are the whole content of one and the refs have no 'txRefId', which is the
-- difference from what 'extractTxRefs' returns for a body.
--
-- Upstream holds an entry\'s hash as @PackedBytes 32@, which is the
-- representation a 'TxHash' already is, so the conversion is total and carries
-- no cost. This is the only place a Leios type becomes one of ours: nothing
-- outside this module names @Leios.TxHash@, and 'InEb' is not exported, so
-- this is also the only place a ref can claim to have come from an EB.
leiosEbTxRefs :: Leios.LeiosEb -> BlockNo -> SlotNo -> [TxRef]
leiosEbTxRefs eb blockNo slotNo =
  [ InEb (Hash.hashFromPackedBytes bytes) blockNo slotNo
  | (Leios.MkTxHash bytes, _size) <- toList (Leios.leiosEbTxs eb)
  ]

#endif

