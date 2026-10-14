{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

--------------------------------------------------------------------------------

-- | Endorser block retrieval via LeiosFetch (NodeToNode).
--
-- Built only when the @leios-prototype@ flag is set: the protocol exists just in
-- the ouroboros-consensus that branch pins.
--
-- A Leios transaction is never in a Praos block body. One block announces an
-- endorser block (EB), its child certifies it by carrying a Leios certificate,
-- and the EB itself travels on this mini-protocol. Over the local socket the
-- node inlines a certified EB's transactions into the block it serves
-- (@resolveLeiosBlock@), which is why the NodeToClient observer sees Leios
-- transactions and the NodeToNode one, reading blocks as diffused, does not.
--
-- == What this does
--
-- "Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxIdSync" appends a 'Request'
-- for every confirmed block that /certified/ an EB. This client drains that
-- queue, one EB per round trip.
--
-- An announcement alone is never enough: if no child certifies it, that EB was
-- not part of the chain and is never requested, so a peer is never asked for an
-- EB it had no reason to acquire.
--
-- An EB lists transaction hashes and sizes, never bodies, so fetching one costs
-- a hash per transaction instead of a transaction.
--
-- == Why it fetches only after confirmation
--
-- The request goes out once the certifying block is @confirmationDepth@ blocks
-- deep, so the peer has had that many blocks to acquire the EB, and nothing has
-- to be remembered about EBs belonging to blocks that end up rolled back.
--
-- Fetching late is safe against garbage collection: a peer collects its Leios
-- DB at the immutable tip, thousands of blocks back, so a confirmed block's EB
-- is never collected before it is asked for.
--
-- == An empty EB is fatal
--
-- A peer that does not hold an EB answers with an empty one rather than an
-- error, and an EB is never legitimately empty: both Leios DB backends reject an
-- empty body as a programmer error. So an empty answer for an EB that a
-- confirmed block certified means that peer never acquired it.
--
-- A CertRB carries no transactions of its own, so that is not a few missing
-- transactions but every transaction of that block.
--
-- It is treated as fatal rather than skipped: dropping those transactions
-- would leak them from the recycling loop and distort a run in a way that looks
-- like a generator fault much later.
module Cardano.Benchmarking.TxCentrifuge.NodeToNode.LeiosFetch
  ( -- * Client type
    LeiosFetchClient
    -- * Requests
  , Request (..)
    -- * Protocol client
  , leiosFetchClient
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Exception (throwIO)
----------------
-- containers --
----------------
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import LeiosDemoOnlyTestFetch qualified as LF
import LeiosDemoTypes qualified as Leios
---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Chain qualified as Chain

--------------------------------------------------------------------------------

-- | LeiosFetch client, retrieving endorser blocks by point.
type LeiosFetchClient =
  LF.LeiosFetchClientPeer Leios.LeiosPoint Leios.LeiosEb Leios.LeiosTx IO ()

-- | One endorser block to fetch, tagged with the block that certified it.
--
-- The block number and slot are the certifying block's, not the announcing
-- one's: that is where the EB's transactions enter the chain, and they say which
-- block sent us looking for it.
data Request = Request
  { requestPoint   :: !Leios.LeiosPoint
  , requestBlockNo :: !Chain.BlockNo
  , requestSlotNo  :: !Chain.SlotNo
  }
  deriving Show

--------------------------------------------------------------------------------

-- | Drain the request queue, fetching one endorser block per round trip and
-- publishing the transactions each one lists as confirmed.
--
-- Blocks on an empty queue, so the mini-protocol sits idle until a confirmed
-- block certifies an EB. Never finishes on its own: the connection ending is
-- what stops it.
leiosFetchClient
  -- | Observer name, as in the config's @"observers"@ object. Only used to say
  -- which observer failed, since nothing here catches the exception.
  :: String
  -- | Request queue, appended to by 'TxIdSync' on confirmation.
  -> STM.TVar (Seq Request)
  -- | The observer's confirmation channel, the same one its ranking blocks are
  -- published on. An EB's transactions go on it as confirmed, at the block
  -- that certified the EB.
  -> STM.TChan (Either Chain.TxRef Chain.TxRef)
  -> LeiosFetchClient
leiosFetchClient observerName queue broadcast =
  LF.leiosFetchClientPeer nextJob
 where
  -- Both signatures are needed, not documentation: as `where` bindings these
  -- would be generalised, and the concrete protocol types would never reach the
  -- `MsgLeiosBlock` match.
  nextJob
    :: IO (Either ()
             (LF.SomeLeiosFetchJob
                Leios.LeiosPoint Leios.LeiosEb Leios.LeiosTx IO))
  nextJob = do
    request <- STM.atomically $ do
      pending <- STM.readTVar queue
      case Seq.viewl pending of
        Seq.EmptyL       -> STM.retry
        next Seq.:< rest -> do
          STM.writeTVar queue rest
          pure next
    pure $ Right $ LF.MkSomeLeiosFetchJob
      (LF.MsgLeiosBlockRequest (requestPoint request))
      (pure $ \case LF.MsgLeiosBlock eb -> received request eb)

  received :: Request -> Leios.LeiosEb -> IO ()
  received request eb = case Chain.leiosEbTxRefs eb blockNo slotNo of
    -- See the module header: an empty EB here means the peer never acquired it.
    [] -> fatal $ "peer has no endorser block " ++ ebDescription
    -- Published as confirmed, one at a time, so an EB confirms all of its own
    -- transactions or none.
    txRefs -> STM.atomically $ mapM_ publish txRefs
   where
    -- The block that certified this EB, which is where its transactions enter
    -- the chain (see 'Request').
    blockNo = requestBlockNo request
    slotNo  = requestSlotNo  request
    publish txRef = STM.writeTChan broadcast (Right txRef)
    ebDescription =
         Leios.prettyLeiosPoint (requestPoint request)
      ++ " certified by confirmed block " ++ show blockNo
      ++ " (slot " ++ show slotNo ++ ")"
    fatal reason =
      throwIO $ userError $ "observer " ++ observerName ++ ": " ++ reason

