{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

-- | The Shelley-based era this generator builds and submits transactions in.
--
-- One definition for the whole package: every era-specific type is written in
-- terms of 'Era' and every cardano-api builder is passed 'era', so the era
-- moves in one place.
--
-- == Why it has to match the cluster's era
--
-- It is not only about being accepted. A ledger era decodes the era before it
-- for backwards compatibility, so a transaction built one era behind is
-- submitted and included, but it is /stored and re-encoded in the era in
-- force/. Dijkstra, for one, writes a top-level transaction as
-- @[body, wits, aux]@ while Conway writes @[body, wits, isValid, aux]@, so the
-- bytes a Dijkstra node holds are not the bytes a Conway builder submitted.
--
-- An 'Api.TxId' survives that, being a hash of the transaction body, whose
-- bytes are kept as they arrived. A 'Cardano.Benchmarking.TxCentrifuge.Chain.TxHash'
-- does not, being a hash of the whole serialised transaction: the generator
-- would compute one hash and every endorser block and block body on the chain
-- would carry another, so nothing an observer reports would ever match a
-- payload and nothing would ever be recycled.
--
-- == TODO: choose the era at runtime
--
-- Fixed at compile time here, which is why a cluster in another era is only
-- caught by @Cardano.Benchmarking.TxCentrifuge.Chain.eraMismatch@ once the
-- first block arrives. Two ways out, neither taken:
--
-- 1. An existential, @Api.InAnyShelleyBasedEra Api.Tx@. The era is learnt at
--    startup (@Api.QueryCurrentEra@ over NodeToClient, or the era of the first
--    block observed), transactions are built in it, and the witness travels
--    inside the value, so the payload queue, the recycler and the traces never
--    name an era; each use site matches the box to bring the era back into
--    scope. The cost is that two separately unwrapped values cannot be shown
--    to be in the same era, so whatever the builder pairs (its destination
--    addresses and the transaction it signs) has to be unwrapped together,
--    restructuring @interpretBuilder@ and the builder loop around one match.
--
-- 2. An era type parameter. @Tracing.Tracers@ and the builder traces become
--    @Tracers era@ (they hold an @Api.AddressInEra era@), the builder record
--    and @Chain.toGenTx@ take the witness, and @main@ unwraps the era once so
--    everything below is polymorphic under an @Api.IsShelleyBasedEra era@
--    constraint. Mechanical, around six types gain a parameter, no existential
--    gymnastics, and the preferable of the two.
--
-- Neither removes the CPP below: @Api.DijkstraEra@ is a name the cardano-api of
-- the release pins does not have, so a line mentioning it stays conditional
-- whatever the pipeline does.
module Cardano.Benchmarking.TxCentrifuge.Era
  ( Era
  , era
  , eraName
  ) where

--------------------------------------------------------------------------------

-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api

--------------------------------------------------------------------------------

-- | A Leios cluster runs Dijkstra, which is where the Leios header fields and
-- the endorser blocks exist, so the @leios-prototype@ flag decides this. The
-- flag means the pinned cardano-node is that branch, and only that branch\'s
-- cardano-api has the era at all, so the two cannot be chosen separately.
#ifdef LEIOS_PROTOTYPE
type Era = Api.DijkstraEra
#else
type Era = Api.ConwayEra
#endif

-- | Value-level witness of 'Era', for cardano-api builders that take an
-- 'Api.ShelleyBasedEra' argument (such as
-- @Cardano.Benchmarking.TxCentrifuge.TxAssembly.buildTx@).
era :: Api.ShelleyBasedEra Era
#ifdef LEIOS_PROTOTYPE
era = Api.ShelleyBasedEraDijkstra
#else
era = Api.ShelleyBasedEraConway
#endif

-- | 'Era' named as a block of it is named, so that the era this generator
-- builds in can be compared against the era a chain is in at runtime (see
-- @Cardano.Benchmarking.TxCentrifuge.Chain.eraMismatch@). Spelled out because
-- naming another era\'s type is what the pins disagree about.
eraName :: String
#ifdef LEIOS_PROTOTYPE
eraName = "Dijkstra"
#else
eraName = "Conway"
#endif
