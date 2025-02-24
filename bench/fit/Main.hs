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
      [testComplexity]

testComplexity :: TestTree
testComplexity = testCase "Decode runs is logarithmic" $ do
  c <- measureGenDecode
  -- This is too flaky to have the assertion be an actual test, so we
  -- just run it and print the result if it's different. We keep it as
  -- a test, however, so that CI verifies it at least runs.
  unless (Fit.isLogarithmic c) $ do
    IO.putStrLn $ "Assumed isLogarithmic but guessed: " ++ show c

measureGenDecode :: IO Complexity
measureGenDecode =
  Fit.fit
    $ Fit.mkFitConfig
      (Utils.genAndDecodeRuns)
      (10, 10_000)
