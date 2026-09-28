#!/bin/sh

echo "------------------------------------------------------------"
echo "Building with cardano-node 10.7.1 ..."
echo "------------------------------------------------------------"
echo ""
nix build --no-link --print-out-paths .#tx-centrifuge \
  --override-input cardano-node github:IntersectMBO/cardano-node/045bc187a36ef0cbd236db902b85dd8f202fb059
echo ""
echo "------------------------------------------------------------"
echo "Building with cardano-node 11.0.1 ..."
echo "------------------------------------------------------------"
echo ""
nix build --no-link --print-out-paths .#tx-centrifuge \
  --override-input cardano-node github:IntersectMBO/cardano-node/97036a66bcf8c89f687ae57a048eecc0389977ef
echo ""
echo "------------------------------------------------------------"
echo "Building with cardano-node 11.1.1 ..."
echo "------------------------------------------------------------"
echo ""
nix build --no-link --print-out-paths .#tx-centrifuge \
  --override-input cardano-node github:IntersectMBO/cardano-node/c2ebdc87dfe07706a83e52f219e712c60d1b0a56
echo ""
echo "------------------------------------------------------------"
echo "Building with cardano-node \"leios-protoype\" (2026-09-28) ..."
echo "------------------------------------------------------------"
echo ""
nix build --no-link --print-out-paths .#tx-centrifuge \
  --override-input cardano-node github:IntersectMBO/cardano-node/8bb6b68d80a8a4fb1904c48805be4f3a516593e
echo ""
echo "------------------------------------------------------------"
echo "Builiding as repo says ..."
echo "------------------------------------------------------------"
echo ""
nix build --no-link --print-out-paths .#tx-centrifuge

