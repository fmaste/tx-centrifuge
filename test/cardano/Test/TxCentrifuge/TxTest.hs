{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- cardano-api 11 deprecates Api.getTxBody in favour of UnsignedTx from
-- Cardano.Api.Experimental. This package stays on the stable API so one source
-- tree builds against cardano-node 10.7.1 through 11.1.1, and the node compiles
-- local packages with -Werror, so the warning has to go rather than be demoted.
{-# OPTIONS_GHC -Wno-deprecations #-}

--------------------------------------------------------------------------------

module Test.TxCentrifuge.TxTest
  ( txTests
  , testSetup
  , mkDummyFund
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import System.IO (hFlush, hPutStrLn, stderr)
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
-------------------------
-- cardano-ledger-core --
-------------------------
import Cardano.Ledger.Coin qualified as L
-----------
-- clock --
-----------
import System.Clock qualified as Clock
-----------
-- tasty --
-----------
import Test.Tasty qualified as Tasty
-----------------
-- tasty-hunit --
-----------------
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
---------------------
-- tx-centrifuge --
---------------------
import Cardano.Benchmarking.TxCentrifuge.Chain qualified as Chain
import Cardano.Benchmarking.TxCentrifuge.Era (Era, era)
import Cardano.Benchmarking.TxCentrifuge.Fund qualified as Fund
import Cardano.Benchmarking.TxCentrifuge.TxAssembly qualified as TxAssembly

--------------------------------------------------------------------------------

txTests :: Tasty.TestTree
txTests = Tasty.testGroup "node"
  [ HUnit.testCase "buildTx: simple 1-in-1-out transaction" $ do
      (signKey, addr) <- testSetup
      let fund = mkDummyFund signKey 0 10_000_000
          fee  = L.Coin 200_000
      case TxAssembly.buildTx era [(signKey, addr)] [fund] 1 fee of
        Left err ->
          HUnit.assertFailure $ "buildTx failed: " ++ show err
        Right (tx, txRef, outFunds) -> do
          -- One output fund recycled.
          length outFunds @?= 1
          -- The key's id is the body's, derived here independently.
          Chain.txRefId txRef @?= Just (Api.getTxId (Api.getTxBody tx))
          -- The key's hash is of the signed transaction's bytes, which is what
          -- an endorser block lists, not of its body.
          Chain.txRefHash txRef @?= Chain.hashTxBytes (Api.serialiseToCBOR tx)
          -- Output value = input - fee.
          Fund.fundLovelace (head outFunds) @?= (10_000_000 - 200_000)
          -- The recycled fund's TxIn references the new tx.
          Fund.fundTxIn (head outFunds)
            @?= Api.TxIn (Api.getTxId (Api.getTxBody tx)) (Api.TxIx 0)

  , HUnit.testCase "buildTx: 2-in-3-out transaction" $ do
      (signKey, addr) <- testSetup
      let fund1 = mkDummyFund signKey 0 5_000_000
          fund2 = mkDummyFund signKey 1 5_000_000
          fee   = L.Coin 200_000
      case TxAssembly.buildTx era [(signKey, addr)]
             [fund1, fund2] 3 fee of
        Left err ->
          HUnit.assertFailure $ "buildTx failed: " ++ show err
        Right (_tx, _txRef, outFunds) -> do
          -- Three output funds.
          length outFunds @?= 3
          -- Total output = total input - fee.
          let totalOut = sum (map Fund.fundLovelace outFunds)
          totalOut @?= (10_000_000 - 200_000)

  , HUnit.testCase "buildTx: insufficient funds" $ do
      (signKey, addr) <- testSetup
      let fund = mkDummyFund signKey 0 100_000
          fee  = L.Coin 200_000
      case TxAssembly.buildTx era [(signKey, addr)] [fund] 1 fee of
        Left _  -> pure () -- expected
        Right _ ->
          HUnit.assertFailure
            "buildTx should fail when funds < fee"

  , HUnit.testCase "buildTx: no input funds" $ do
      (signKey, addr) <- testSetup
      let fee = L.Coin 200_000
      case TxAssembly.buildTx era [(signKey, addr)] [] 1 fee of
        Left _  -> pure () -- expected
        Right _ ->
          HUnit.assertFailure
            "buildTx should fail with no inputs"

  , HUnit.testCase "buildTx: zero outputs" $ do
      (signKey, addr) <- testSetup
      let fund = mkDummyFund signKey 0 10_000_000
          fee  = L.Coin 200_000
      case TxAssembly.buildTx era [(signKey, addr)] [fund] 0 fee of
        Left _  -> pure () -- expected
        Right _ ->
          HUnit.assertFailure
            "buildTx should fail with 0 outputs"

  , HUnit.testCase
      "buildTx: signing throughput (single-threaded)" $ do
      (signKey, addr) <- testSetup
      -- Build N transactions sequentially and measure wall-clock
      -- time. This quantifies the single-threaded builder bottleneck.
      let n = 10_000 :: Int
          fee = L.Coin 200_000
          -- Use a large initial fund so recycling doesn't deplete it.
          initialFund = mkDummyFund signKey 0 1_000_000_000_000

      start <- Clock.getTime Clock.MonotonicRaw
      go n initialFund addr signKey fee
      end <- Clock.getTime Clock.MonotonicRaw

      let elapsedNs = Clock.toNanoSecs (end - start)
          elapsedS  = fromIntegral elapsedNs / 1e9 :: Double
          tps       = fromIntegral n / elapsedS

      hPutStrLn stderr ""
      hPutStrLn stderr
        "      --- Single-threaded buildTx throughput ---"
      hPutStrLn stderr $ "        txs built:  " ++ show n
      hPutStrLn stderr $ "        elapsed:    " ++ show elapsedS ++ " s"
      hPutStrLn stderr $
        "        throughput: " ++ show (round tps :: Int) ++ " tx/s"
      hFlush stderr

      -- Sanity: we should be able to sign at least 1000 tx/s on any
      -- reasonable hardware. This is not a hard performance target,
      -- just a smoke test that buildTx isn't catastrophically slow.
      HUnit.assertBool
        ("buildTx throughput too low: "
          ++ show (round tps :: Int) ++ " tx/s")
        (tps > 1000)
  ]
  where
    -- Build N txs sequentially, recycling the first output each time.
    go :: Int -> Fund.Fund
       -> Api.AddressInEra Era
       -> Api.SigningKey Api.PaymentKey -> L.Coin -> IO ()
    go 0 _ _ _ _ = pure ()
    go remaining fund addr signKey fee =
      case TxAssembly.buildTx era [(signKey, addr)] [fund] 1 fee of
        Left err ->
          error $ "throughput test: buildTx failed at iteration "
            ++ show remaining ++ ": " ++ show err
        Right (_, _, outFunds) ->
          go (remaining - 1) (head outFunds) addr signKey fee

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

-- | Create the common test fixtures: a fresh signing key and its address.
-- 'TxAssembly.buildTx' takes no protocol parameters yet (see the TODO there),
-- so none are loaded here.
testSetup
  :: IO ( Api.SigningKey Api.PaymentKey
        , Api.AddressInEra Era
        )
testSetup = do
  signKey <- Api.generateSigningKey Api.AsPaymentKey
  let networkId = Api.Testnet (Api.NetworkMagic 42)
      addr = Api.shelleyAddressInEra
        era
        $ Api.makeShelleyAddress networkId
            (Api.PaymentCredentialByKey
              (Api.verificationKeyHash
                (Api.getVerificationKey signKey)))
            Api.NoStakeAddress

  pure (signKey, addr)

-- | Create a dummy fund with a synthetic TxIn.  Uses the signing key's
-- verification key hash to derive a deterministic TxId (via
-- 'Api.genesisUTxOPseudoTxIn') and the caller-supplied @index@ as the
-- 'Api.TxIx'.  Each distinct @index@ produces a unique 'Api.TxIn', so
-- multi-input tests can create several funds from the same key without
-- accidentally producing duplicate inputs.
mkDummyFund :: Api.SigningKey Api.PaymentKey -> Word -> Integer -> Fund.Fund
mkDummyFund signKey index lovelace = Fund.Fund
  { Fund.fundTxIn =
      let Api.TxIn txId _ = Fund.genesisTxIn
            (Api.Testnet (Api.NetworkMagic 42))
            signKey
      in Api.TxIn txId (Api.TxIx index)
  , Fund.fundLovelace = lovelace
  , Fund.fundSignKey = signKey
  }
