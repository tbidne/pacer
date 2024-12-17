{-# LANGUAGE QuasiQuotes #-}

module Functional.Chart (tests) where

import FileSystem.OsPath (unsafeDecode)
import Functional.Prelude

tests :: IO OsPath -> TestTree
tests getTestDir =
  testGroup
    "Pacer.Chart"
    [ testExampleChart getTestDir,
      testSimple getTestDir,
      testFilter getTestDir,
      testFilterEmptyError getTestDir,
      testDuplicateDateError getTestDir
    ]

testExampleChart :: IO OsPath -> TestTree
testExampleChart getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \testDir ->
            [ "chart",
              "--runs",
              runsPath,
              "--chart-requests",
              chartRequestsPath,
              "--json",
              unsafeDecode (mkJsonPath testDir)
            ],
          testDesc = "Generates example",
          testName = [osp|testExampleChart|],
          resultToBytes = \path _ -> readBinaryFileIO . mkJsonPath $ path
        }
    runsPath = unsafeDecode [osp|data/input/example/runs.toml|]
    chartRequestsPath = unsafeDecode [osp|data/input/example/chart-requests.toml|]
    mkJsonPath testDir = testDir </> [ospPathSep|testExampleChart_charts.json|]

testSimple :: IO OsPath -> TestTree
testSimple = testChart "Simple example" [osp|testSimple|]

testFilter :: IO OsPath -> TestTree
testFilter = testChart "Filter example" [osp|testFilter|]

testFilterEmptyError :: IO OsPath -> TestTree
testFilterEmptyError = testChart desc [osp|testFilterEmptyError|]
  where
    desc = "Filter empty error"

testDuplicateDateError :: IO OsPath -> TestTree
testDuplicateDateError = testChart desc [osp|testDuplicateDateError|]
  where
    desc = "Duplicate date error"
