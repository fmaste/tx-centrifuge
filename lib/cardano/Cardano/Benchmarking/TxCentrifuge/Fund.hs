{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.TxCentrifuge.Fund
  ( -- * A fund.
    Fund (..)
    -- * Reading funds.
  , foldFunds
    -- * Discovering funds on chain.
  , discoverFunds
  , discoverFundsAtAddresses
    -- * Utils.
  , genesisTxIn
  , deriveAddress
  , readSigningKey
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Bifunctor (first)
import Data.Char (isSpace)
import System.IO qualified as IO
import Text.Read (readMaybe)
-----------
-- aeson --
-----------
import Data.Aeson qualified as Aeson
import Data.Aeson ((.:), (.:?))
import Data.Aeson.Types qualified as Aeson
-------------------------
-- base16-bytestring --
-------------------------
import Data.ByteString.Base16 qualified as Base16
----------------
-- bytestring --
----------------
import Data.ByteString.Char8 qualified as BS8
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
----------
-- text --
----------
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Era (Era, era)
import Cardano.Benchmarking.TxCentrifuge.NodeToClient.UTxOQuery qualified as UTxOQuery

--------------------------------------------------------------------------------
-- A fund.
--------------------------------------------------------------------------------

-- | A spendable fund: a UTxO reference, its Lovelace value, and the signing key
-- required to spend it.
data Fund = Fund
  { -- | Reference of the UTxO this fund spends.
    fundTxIn :: !Api.TxIn
    -- | Lovelace amount. Not a @Value@: this is one number, not a
    -- multi-asset bundle.
  , fundLovelace :: !Integer
    -- | Key to spend this UTxO.
  , fundSignKey :: !(Api.SigningKey Api.PaymentKey)
  }

--------------------------------------------------------------------------------
-- Creating funds.
--------------------------------------------------------------------------------

-- | Internal: JSON-parseable fund entry, one object of the fund file:
--
-- @
-- { "tx_in": "txid#ix", "lovelace": 1000000, "signing_key":   "payment.skey" }
-- {                     "lovelace": 1000000, "skey_cbor_hex": "5820a1b2..." }
-- @
--
-- One constructor, not one per shape: every record selector of a sum type is
-- partial, which @-Wpartial-fields@ rejects, and the shape is the optional
-- @\"tx_in\"@ anyway.
data FundEntry = FundEntry
  { -- | @\"tx_in\"@, the UTxO this fund refers to. 'Nothing' when the field
    -- is absent: the fund is then a genesis UTxO and its 'Api.TxIn' is
    -- derived from the key by 'genesisTxIn' (always @TxIx 0@).
    entryTxIn :: !(Maybe Api.TxIn)
    -- | @\"lovelace\"@, the amount this UTxO holds.
  , entryLovelace :: !Integer
    -- | @\"signing_key\"@ or @\"skey_cbor_hex\"@, the key that spends it.
  , entryKeySource :: !SigningKeySource
  }

instance Aeson.FromJSON FundEntry where
  parseJSON = Aeson.withObject "Fund" $ \o -> do
    mbTxIn    <- traverse parseTxIn =<< o .:? "tx_in"
    lovelace  <- o .:  "lovelace"
    keySource <- parseSigningKeySource o
    pure FundEntry
      { entryTxIn      = mbTxIn
      , entryLovelace  = lovelace
      , entryKeySource = keySource
      }

-- | Where one signing key comes from: one field or the other, never both.
--
-- @
-- { "signing_key":   "utxo1.skey",  "lovelace": 9000000000 }
-- { "skey_cbor_hex": "5820a1b2...", "lovelace": 9000000000 }
-- @
--
-- Two field names rather than one field with two shapes, so an entry says
-- which one it carries and nothing is guessed from the text.
--
-- Positional, with no record selectors: being a sum type, any selector of it
-- would be partial and @-Wpartial-fields@ rejects those.
data SigningKeySource
  = -- | @\"signing_key\"@: path of a @.skey@ file, read at startup.
    SigningKeyFile !FilePath
  | -- | @\"skey_cbor_hex\"@: the key itself, decoded while the JSON was parsed.
    SigningKeyInline !(Api.SigningKey Api.PaymentKey)

-- | Parse the signing key of a fund entry: exactly one of @\"signing_key\"@
-- and @\"skey_cbor_hex\"@.
parseSigningKeySource :: Aeson.Object -> Aeson.Parser SigningKeySource
parseSigningKeySource o = do
  mbPath <- o .:? "signing_key"
  mbHex  <- o .:? "skey_cbor_hex"
  case (mbPath, mbHex) of
    (Just path, Nothing) -> pure (SigningKeyFile path)
    (Nothing, Just hex)  -> SigningKeyInline <$> parseCborHexKey hex
    (Nothing, Nothing)   -> fail $
      "needs \"signing_key\", the path of a .skey file, or \"skey_cbor_hex\","
      ++ " the key itself"
    (Just _,  Just _)    -> fail
      "has both \"signing_key\" and \"skey_cbor_hex\", give one"

-- | Decode a signing key from the hex of its CBOR, which is the @\"cborHex\"@
-- field of a @.skey@ file: a CBOR byte string wrapping the 32-byte ed25519
-- key, so 68 hex characters starting with @5820@.
--
-- The file\'s @\"type\"@ is not needed. Both types 'signingKeyTypes' accepts
-- hold the same key and serialise it the same, and the genesis one is cast to
-- a payment key anyway, so the same hex decodes to the same key either way.
-- Anything else, an extended key for one, fails to decode here.
parseCborHexKey :: T.Text -> Aeson.Parser (Api.SigningKey Api.PaymentKey)
parseCborHexKey hex = do
  bytes <- either (fail . ("skey_cbor_hex: " ++)) pure
             (Base16.decode (T.encodeUtf8 hex))
  case Api.deserialiseFromCBOR (Api.AsSigningKey Api.AsPaymentKey) bytes of
    Right key -> pure key
    Left err  -> fail $
      "skey_cbor_hex: " ++ show err
      ++ " (expected the 68 hex characters of a .skey file's \"cborHex\","
      ++ " starting with \"5820\")"

-- | Parse @"txid#ix"@ format. Both parts are required.
parseTxIn :: T.Text -> Aeson.Parser Api.TxIn
parseTxIn text =
  let (txIdHex, rest) = T.breakOn "#" text
  in case T.uncons rest of
    Just ('#', ds) ->
      case Api.deserialiseFromRawBytesHex @Api.TxId (T.encodeUtf8 txIdHex) of
        Left err   -> fail $ "Invalid TxId: " ++ show err
        Right txId -> case readMaybe (T.unpack ds) of
          Nothing -> fail $ "Invalid TxIx: expected an integer, got " ++ show ds
          Just ix -> pure $ Api.TxIn txId (Api.TxIx ix)
    _ -> fail "Invalid TxIn: expected \"txid#ix\" format"

-- | Fold over the funds of a fund file, one fund at a time.
--
-- Two shapes are accepted, told apart by the first character that is not
-- whitespace:
--
-- * @[@: a JSON array of fund objects, decoded whole, see 'foldFundsArray'.
--   The shape for the thousands of entries a testnet hands out.
--
-- * anything else: JSONL, one fund object per line, which streams, see
--   'foldFundsLines'. The shape for millions of entries, where decoding an
--   array whole would need the file, a JSON tree of it and every fund in
--   memory at once.
--
-- Each object names a @.skey@ file in @"signing_key"@ or carries the key
-- itself in @"skey_cbor_hex"@ (see 'SigningKeySource'). A path is read once
-- per entry that names it, so a file of a million such entries opens a million
-- files, while an inline key needs none. Fields other than those the entry
-- uses are ignored.
--
-- For key-only entries (no @"tx_in"@), the genesis UTxO pseudo-TxIn is derived
-- from the signing key using the provided 'Api.NetworkId'.
--
-- What the fold holds is the caller\'s business and the only thing that grows:
-- this keeps one line and one fund at a time. The accumulator is forced at
-- every step, so a lazy one cannot pile up thunks behind the fold.
foldFunds
  :: Api.NetworkId
  -> FilePath
  -> (acc -> Fund -> IO acc)
  -> acc
  -> IO (Either String acc)
foldFunds networkId path step acc0 = do
  firstChar <- IO.withFile path IO.ReadMode firstNonSpace
  case firstChar of
    Just '[' -> foldFundsArray networkId path step acc0
    _        -> foldFundsLines networkId path step acc0
  where
    -- Consumes whitespace only, from a handle that is closed right after.
    firstNonSpace h = do
      eof <- IO.hIsEOF h
      if eof
        then pure Nothing
        else do
          c <- IO.hLookAhead h
          if isSpace c
            then IO.hGetChar h >> firstNonSpace h
            else pure (Just c)

-- | Fold over a whole JSON array of fund objects.
foldFundsArray
  :: Api.NetworkId
  -> FilePath
  -> (acc -> Fund -> IO acc)
  -> acc
  -> IO (Either String acc)
foldFundsArray networkId path step acc0 = do
  result <- Aeson.eitherDecodeFileStrict' path
  case result of
    Left err -> pure $ Left err
    Right (entries :: [FundEntry]) -> go acc0 entries
  where
    go !acc [] = pure $ Right acc
    go !acc (entry : rest) = do
      eFund <- entryToFund networkId entry
      case eFund of
        Left err   -> pure $ Left err
        Right fund -> step acc fund >>= \acc' -> go acc' rest

-- | Fold over a JSONL file of fund objects, one line at a time.
--
-- Line N is fund N: every line has to be one fund object, a blank one
-- included, so nothing shifts the correspondence between a line number and the
-- fund it describes. An error names its line and stops the read, the handle
-- closing with 'IO.withFile'.
--
-- Nothing is held but the accumulator: one line is read, parsed, turned into a
-- 'Fund' and handed to @step@ before the next line is read, so no thunk is
-- left holding a line alive (see 'forceFund', which is what makes that true).
foldFundsLines
  :: Api.NetworkId
  -> FilePath
  -> (acc -> Fund -> IO acc)
  -> acc
  -> IO (Either String acc)
foldFundsLines networkId path step acc0 =
  IO.withFile path IO.ReadMode $ \handle -> go handle (1 :: Int) acc0
  where
    go handle !lineNo !acc = do
      eof <- IO.hIsEOF handle
      if eof
        then pure $ Right acc
        else do
          line <- BS8.hGetLine handle
          case Aeson.eitherDecodeStrict' line of
            Left err -> pure $ Left $ atLine lineNo err
            Right entry -> do
              eFund <- entryToFund networkId entry
              case eFund of
                Left err -> pure $ Left $ atLine lineNo err
                Right fund ->
                  step acc fund >>= \acc' -> go handle (lineNo + 1) acc'
    atLine n err = path ++ ":" ++ show n ++ ": " ++ err

-- | Convert one entry into a fund: take the key it carries inline, or read the
-- file it names, then build the fund around it.
--
-- The fund comes back forced ('forceFund'), which is what lets a fold hand it
-- on and drop the line it was parsed from.
entryToFund :: Api.NetworkId -> FundEntry -> IO (Either String Fund)
entryToFund networkId entry = do
  eKey <- entryKey
  pure $ case eKey of
    Left err  -> Left err
    -- The bang runs `forceFund` here, before the fund reaches a cons cell.
    Right key -> let !fund = mkFund key in Right fund
  where

    entryKey :: IO (Either String (Api.SigningKey Api.PaymentKey))
    entryKey = case entryKeySource entry of
      SigningKeyInline key   -> pure $ Right key
      SigningKeyFile keyPath ->
        first (\err -> "Failed to load signing key " ++ keyPath ++ ": " ++ err)
          <$> readSigningKey keyPath

    mkFund :: Api.SigningKey Api.PaymentKey -> Fund
    mkFund key = forceFund $ Fund txIn (entryLovelace entry) key
      where
        txIn = case entryTxIn entry of
          Just given -> given
          -- No "tx_in": a genesis UTxO, whose TxIn the key derives.
          Nothing    -> genesisTxIn networkId key

-- | A 'Fund' with everything inside it evaluated, so that holding one holds
-- nothing else.
--
-- The record's three fields are strict, which on its own is not enough. An
-- 'Api.TxIn' holds its 'Api.TxId' lazily and for an entry with no @"tx_in"@
-- that 'Api.TxId' is an unevaluated hash of the key, while below
-- 'Api.SigningKey' sits cardano-crypto-class's own representation, which a
-- 'seq' reaches only the first constructor of. Left unevaluated, either one
-- keeps the input line it came from alive, and at millions of entries that is
-- the difference between a bounded load and a space leak.
--
-- The key is forced by serialising it and dropping the bytes, 32 of them per
-- fund and garbage immediately: it costs one small allocation but holds
-- whatever the representation below turns out to be.
forceFund :: Fund -> Fund
forceFund fund@(Fund (Api.TxIn txId txIx) _ key) =
        txId
  `seq` txIx
  `seq` Api.serialiseToRawBytes key
  `seq` fund

-- | Discover starting funds on chain. For each signing key, derive the address
-- it controls, ask the local node for the UTxOs there, and return one 'Fund'
-- per UTxO, tagged with the key that owns it so the spending transaction can
-- always be signed. 'Right' with the (possibly empty) funds, or 'Left' if a
-- key cannot be read or the query fails.
--
-- Addresses are deduplicated first: two key files can control the same address,
-- and the node returns each UTxO once, so attributing a UTxO to more than one
-- key would build duplicate references that double spend the same output.
--
-- Point the keys at each builder's destination address (see
-- 'destination_signing_key'): the recycling loop keeps a builder's outputs
-- there, so discovery re-picks-up exactly the funds a previous run left, making
-- restart stateless.
discoverFunds
  :: Api.NetworkId
  -> FilePath   -- ^ NodeToClient socket path of the local node to query.
  -> [FilePath] -- ^ Signing key files to discover funds under.
  -> IO (Either String [Fund])
discoverFunds networkId socketPath keyPaths = do
  -- Read each key and derive the address it controls.
  eKeyAddrs <- mapM readKeyAddr keyPaths
  case sequence eKeyAddrs of
    Left err        -> pure (Left err)
    Right keyAddrs0 -> do
      -- Deduplicate by address so each on-chain UTxO is attributed to exactly
      -- one key. Keys deriving the same address share a key hash, so any one of
      -- them signs for it.
      let keyAddrs =
            Map.elems $ Map.fromList
              [ (addr, (skey, addr)) | (skey, addr) <- keyAddrs0 ]
      discoverFundsAtAddresses networkId socketPath keyAddrs
  where
    readKeyAddr path = do
      eKey <- readSigningKey path
      pure $ case eKey of
        Left e     -> Left $ "signing key (" ++ path ++ "): " ++ e
        Right skey -> Right (skey, deriveAddress networkId skey)

-- | Discover the funds currently at the given addresses, each paired with the
-- signing key that can spend there (both already in memory, unlike
-- 'discoverFunds', which reads keys from disk and dedupes before delegating
-- here). Queries the local node for the UTxOs at the addresses and returns
-- one 'Fund' per UTxO, tagged with its address's key so the spending
-- transaction can be signed. 'Right' with the (possibly empty) funds, or
-- 'Left' if the query fails.
--
-- Also the recovery counterpart to 'discoverFunds': after a chain rollback
-- invalidates a builder's queued inputs, reseed from ground truth by
-- re-querying the builder's own destination address.
discoverFundsAtAddresses
  :: Api.NetworkId
  -- | NodeToClient socket path of the local node to query.
  -> FilePath
  -- | Keys paired with the address each controls.
  -> [(Api.SigningKey Api.PaymentKey, Api.AddressInEra Era)]
  -> IO (Either String [Fund])
discoverFundsAtAddresses networkId socketPath keyAddrs = do
  eUtxos <-
    UTxOQuery.queryUTxOsAtAddresses socketPath networkId (map snd keyAddrs)
  pure $ case eUtxos of
    Left err          -> Left err
    Right utxosByAddr -> Right
      [ Fund { fundTxIn = txin, fundLovelace = val, fundSignKey = skey }
      | (skey, addr) <- keyAddrs
      , (txin, val)  <- Map.findWithDefault [] addr utxosByAddr
      ]

--------------------------------------------------------------------------------
-- Utils.
--------------------------------------------------------------------------------

-- | Derive the genesis UTxO pseudo-TxIn from a payment signing key.
-- Casts to 'Api.GenesisUTxOKey' to compute the key hash expected by
-- 'Api.genesisUTxOPseudoTxIn'.
genesisTxIn :: Api.NetworkId -> Api.SigningKey Api.PaymentKey -> Api.TxIn
genesisTxIn networkId
  = Api.genesisUTxOPseudoTxIn networkId
  . Api.verificationKeyHash
  . Api.getVerificationKey
  . castToGenesisUTxOKey

-- | Cast a 'Api.PaymentKey' signing key to a 'Api.GenesisUTxOKey' signing key.
-- Both key types use the same underlying ed25519 representation; this cast
-- enables computing the genesis UTxO pseudo-TxIn via
-- 'Api.genesisUTxOPseudoTxIn'.
castToGenesisUTxOKey
  :: Api.SigningKey Api.PaymentKey
  -> Api.SigningKey Api.GenesisUTxOKey
castToGenesisUTxOKey (Api.PaymentSigningKey skey) =
  Api.GenesisUTxOSigningKey skey

-- | Derive the enterprise (no-stake) Shelley address controlled by a payment
-- signing key under the given network.
deriveAddress
  :: Api.NetworkId
  -> Api.SigningKey Api.PaymentKey
  -> Api.AddressInEra Era
deriveAddress networkId signingKey =
  Api.shelleyAddressInEra era $
    Api.makeShelleyAddress networkId
      (Api.PaymentCredentialByKey
        (Api.verificationKeyHash
          (Api.getVerificationKey signingKey)))
      Api.NoStakeAddress

-- | The signing key types accepted wherever a key is given, by path or inline.
-- The names are the @\"type\"@ of the text envelope, exactly:
--
-- * @PaymentSigningKeyShelley_ed25519@
-- * @GenesisUTxOSigningKey_ed25519@
--
-- Genesis UTxO keys are cast to payment keys, both being the same ed25519 key.
signingKeyTypes
  :: [Api.FromSomeType Api.HasTextEnvelope (Api.SigningKey Api.PaymentKey)]
signingKeyTypes =
  [ Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey) id
  , Api.FromSomeType
      (Api.AsSigningKey Api.AsGenesisUTxOKey)
      Api.castSigningKey
  ]

-- | Read a signing key from a text envelope file, of either type in
-- 'signingKeyTypes'.
readSigningKey :: FilePath -> IO (Either String (Api.SigningKey Api.PaymentKey))
readSigningKey fp = do
  result <- Api.readFileTextEnvelopeAnyOf signingKeyTypes (Api.File fp)
  pure $ first show result

