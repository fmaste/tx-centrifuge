{-# LANGUAGE ImportQualifiedPost #-}

--------------------------------------------------------------------------------

module Test.PullFiction.PipelineTest
  ( pipelineTests
  ) where

--------------------------------------------------------------------------------

-----------
-- tasty --
-----------
import Test.Tasty qualified as Tasty
-----------------
-- tasty-hunit --
-----------------
import Test.Tasty.HUnit qualified as HUnit
---------------------
-- pull-fiction --
---------------------
import Paths_tx_centrifuge qualified as Paths
import Test.PullFiction.Harness qualified as Harness

--------------------------------------------------------------------------------

pipelineTests :: Double -> Tasty.TestTree
pipelineTests dur = Tasty.testGroup "pipeline"
  [
  -- Pipeline test 1: single-group, per-group input queue.
  -- -------------------------------------------------
  --
  -- 1 workload with 50 targets sharing one input queue. Recycled inputs
  -- return to the same queue. Exercises Runtime.resolve with 1 workload,
  -- verifying that closed-loop recycling delivers tokens to every target and
  -- inputs stay within the workload.

    HUnit.testCase
      ("Single-group pipeline (50 targets, " ++ Harness.formatDuration dur ++ ")") $ do
      path <- Paths.getDataFileName "data/config-per-target-0_2.json"
      Harness.runPipelineIsolationTest path 1 dur

  -- Pipeline test 2: multi-group, per-group input queues.
  -- ----------------------------------------------------
  --
  -- 50 workloads, each with 1 target at 1 TPS (50 TPS aggregate).
  -- The config asks for "initial_inputs": { "scope": "per_workload" }, so each
  -- workload has its own input queue; recycled inputs must return to the
  -- originating workload's queue and never leak to another group.
  --
  -- Inputs are tagged with (workloadIndex, inputIndex) tuples. If any worker
  -- ever observes an input with a foreign workload tag, the test fails
  -- immediately. This also exercises resolve's partition logic.

  , HUnit.testCase
      "Multi-group pipeline isolation (50 groups x 1 TPS, 10s)" $ do
      path <- Paths.getDataFileName "data/config-multi-group.json"
      Harness.runPipelineIsolationTest path 50 10

  -- Pipeline test 3: multi-group, one shared input queue.
  -- ----------------------------------------------------
  --
  -- The same shape as test 2 but with 3 workloads and
  -- "initial_inputs": { "scope": "shared" }, which puts every input in one
  -- queue instead of a queue per workload.
  --
  -- Inputs carry the same (workloadIndex, inputIndex) tags, so here a workload
  -- is expected to work on another workload's inputs: the test fails if no
  -- workload ever observes a foreign tag, the exact result the per-queue
  -- partition of test 2 gives.

  , HUnit.testCase
      "Shared input queue (3 groups x 1 TPS, 10s)" $ do
      path <- Paths.getDataFileName "data/config-shared-inputs.json"
      Harness.runSharedInputsTest path 3 10

  ]
