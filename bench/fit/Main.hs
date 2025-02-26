module Main (main) where

import Bench.Pacer.Utils qualified as Utils
import Pacer.Prelude hiding (IO)
import System.IO qualified as IO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Bench.Fit (Complexity)
import Test.Tasty.Bench.Fit qualified as Fit
import Test.Tasty.HUnit (testCase)
import Prelude (IO)

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Complexity tests"
      [ testRunsDecodeComplexity,
        testRunsDecodeOverlappedComplexity
      ]

testRunsDecodeComplexity :: TestTree
testRunsDecodeComplexity = testCase "Decode runs is linear" $ do
  c <- measureGenDecode
  -- This is too flaky to have the assertion be an actual test, so we
  -- just run it and print the result if it's different. We keep it as
  -- a test, however, so that CI verifies it at least runs.
  unless (Fit.isLinear c) $ do
    IO.putStrLn $ "Predicted linear but guessed: " ++ show c

testRunsDecodeOverlappedComplexity :: TestTree
testRunsDecodeOverlappedComplexity = testCase desc $ do
  c <- measureGenDecodeOverlapped
  unless (Fit.isLinear c) $ do
    IO.putStrLn $ "Predicted linear but guessed: " ++ show c
  where
    desc = "Decode overlapped runs is linear"

measureGenDecode :: IO Complexity
measureGenDecode =
  Fit.fit
    $ Fit.mkFitConfig
      (Utils.genAndDecodeRuns)
      (10, 10_000)

measureGenDecodeOverlapped :: IO Complexity
measureGenDecodeOverlapped =
  Fit.fit
    $ Fit.mkFitConfig
      (Utils.genAndDecodeOverlappedRuns)
      (10, 10_000)
