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
      testFilterPreds getTestDir,
      testFilterEmptyError getTestDir,
      testDuplicateDateError getTestDir
    ]

testExampleChart :: IO OsPath -> TestTree
testExampleChart getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \_ ->
            [ "chart",
              "--data",
              dataDir,
              "--json"
            ],
          outFileName = Just [osp|charts.json|],
          testDesc = "Generates example",
          testName = [osp|testExampleChart|]
        }
    dataDir = unsafeDecode [osp|data/input/example/|]

testSimple :: IO OsPath -> TestTree
testSimple = testChart "Simple example" [osp|testSimple|]

testFilter :: IO OsPath -> TestTree
testFilter = testChart "Filter example" [osp|testFilter|]

testFilterPreds :: IO OsPath -> TestTree
testFilterPreds = testChart "Filter predicates example" [osp|testFilterPreds|]

testFilterEmptyError :: IO OsPath -> TestTree
testFilterEmptyError = testChart desc [osp|testFilterEmptyError|]
  where
    desc = "Filter empty error"

testDuplicateDateError :: IO OsPath -> TestTree
testDuplicateDateError = testChartPosix True desc [osp|testDuplicateDateError|]
  where
    desc = "Duplicate date error"
