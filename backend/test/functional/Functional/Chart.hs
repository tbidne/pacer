{-# LANGUAGE QuasiQuotes #-}

module Functional.Chart (tests) where

import Functional.Prelude
import Pacer.Chart qualified as Chart

tests :: TestTree
tests =
  testGroup
    "Pacer.Chart"
    [ testExampleChart,
      testSimple,
      testFilter,
      testFilterEmptyError,
      testDuplicateDateError
    ]

testExampleChart :: TestTree
testExampleChart = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = "Generates example",
          testName = [osp|testExampleChart|],
          runner =
            toStrictByteString
              <$> Chart.createChartsJsonBS
                (Just runsPath)
                (Just chartRequestsPath)
        }
    runsPath = [osp|data/input/example/runs.toml|]
    chartRequestsPath = [osp|data/input/example/chart-requests.toml|]

testSimple :: TestTree
testSimple = testChart "Simple example" [osp|testSimple|]

testFilter :: TestTree
testFilter = testChart "Filter example" [osp|testFilter|]

testFilterEmptyError :: TestTree
testFilterEmptyError = testChart desc [osp|testFilterEmptyError|]
  where
    desc = "Filter empty error"

testDuplicateDateError :: TestTree
testDuplicateDateError = testChart desc [osp|testDuplicateDateError|]
  where
    desc = "Duplicate date error"
