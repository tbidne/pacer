{-# LANGUAGE QuasiQuotes #-}

module Functional.Pacer.Command.Chart (tests) where

import FileSystem.OsPath (unsafeDecode)
import Functional.Prelude

tests :: IO OsPath -> TestTree
tests getTestDir =
  testGroup
    "Pacer.Command.Chart"
    [ basicTests getTestDir,
      pathTests getTestDir
    ]

basicTests :: IO OsPath -> TestTree
basicTests getTestDir =
  testGroup
    "Basic"
    [ testExampleChart getTestDir,
      testSimple getTestDir,
      testSimpleBuildDir getTestDir,
      testFilter getTestDir,
      testFilterPreds getTestDir,
      testFilterDates getTestDir,
      testFilterEmptyError getTestDir,
      testFilterParseExprError getTestDir,
      testDuplicateDateError getTestDir,
      testGarminChart getTestDir,
      testDefaultAndGarminExamples getTestDir,
      testCaseInsensitive getTestDir,
      testGarminChartError getTestDir,
      testBothChartOverlapError getTestDir
    ]

testExampleChart :: IO OsPath -> TestTree
testExampleChart getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs =
            const
              [ "chart",
                "--data",
                dataDir,
                "--runs",
                runsPath,
                "--json"
              ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "Generates example",
          testName = [osp|testExampleChart|]
        }
    dataDir = unsafeDecode [osp|examples|]
    runsPath = unsafeDecode [ospPathSep|examples/runs.toml|]

testSimple :: IO OsPath -> TestTree
testSimple = testChart "Simple example" [osp|testSimple|]

testSimpleBuildDir :: IO OsPath -> TestTree
testSimpleBuildDir getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs =
            const
              [ "chart",
                "--build-dir",
                "another-build",
                "--data",
                dataDir,
                "--json"
              ],
          outFileName = Just [ospPathSep|another-build/charts.json|],
          testDesc = "Simple with build-dir",
          testName = [osp|testSimpleBuildDir|]
        }
    dataDir = unsafeDecode [ospPathSep|test/functional/data/testSimpleBuildDir|]

testFilter :: IO OsPath -> TestTree
testFilter = testChart "Filter example" [osp|testFilter|]

testFilterPreds :: IO OsPath -> TestTree
testFilterPreds = testChart "Filter predicates example" [osp|testFilterPreds|]

testFilterDates :: IO OsPath -> TestTree
testFilterDates = testChart "Filters on dates" [osp|testFilterDates|]

testFilterEmptyError :: IO OsPath -> TestTree
testFilterEmptyError = testChart desc [osp|testFilterEmptyError|]
  where
    desc = "Filter empty error"

testFilterParseExprError :: IO OsPath -> TestTree
testFilterParseExprError = testChartPosix True desc [osp|testFilterParseExprError|]
  where
    desc = "Filter parse expr error"

testDuplicateDateError :: IO OsPath -> TestTree
testDuplicateDateError = testChartPosix True desc [osp|testDuplicateDateError|]
  where
    desc = "Duplicate date error"

testGarminChart :: IO OsPath -> TestTree
testGarminChart = testChart desc [osp|testGarminChart|]
  where
    desc = "Generates garmin example"

testDefaultAndGarminExamples :: IO OsPath -> TestTree
testDefaultAndGarminExamples getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs =
            const
              [ "chart",
                "--json",
                "--data",
                dataDir
              ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "Generates example with default and garmin",
          testName = [osp|testDefaultAndGarminExamples|]
        }
    dataDir = unsafeDecode [osp|examples|]

testCaseInsensitive :: IO OsPath -> TestTree
testCaseInsensitive = testChart desc [osp|testCaseInsensitive|]
  where
    desc = "Case should not affect file discovery"

testGarminChartError :: IO OsPath -> TestTree
testGarminChartError = testChart desc [osp|testGarminChartError|]
  where
    desc = "Garmin fails without chart-requests garmin.settings"

testBothChartOverlapError :: IO OsPath -> TestTree
testBothChartOverlapError = testChart desc [osp|testBothChartOverlapError|]
  where
    desc = "Datetime overlap across multiple run files errors"

pathTests :: IO OsPath -> TestTree
pathTests getTestDir =
  testGroup
    "Paths"
    [ testPathXdg getTestDir,
      testPathChartRequestsOverrideData getTestDir,
      testPathRunsOverrideData getTestDir,
      testPathChartRequestsOverrideXdg getTestDir,
      testPathRunsOverrideXdg getTestDir,
      testPathConfig getTestDir
    ]

testPathXdg :: IO OsPath -> TestTree
testPathXdg getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = const ["chart", "--json"],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "No paths uses XDG config",
          testName = [osp|testPathXdg|]
        }

testPathChartRequestsOverrideData :: IO OsPath -> TestTree
testPathChartRequestsOverrideData getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs =
            const
              [ "chart",
                "--json",
                "--chart-requests",
                chartRequestsPath,
                "--data",
                dataDir,
                "--runs",
                runsPath
              ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "--chart-requests overrides --data",
          testName = [osp|testPathChartRequestsOverrideData|]
        }
    chartRequestsPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/chart-requests.toml|]

    -- TODO:
    --
    -- We shouldn't _have_ to specify the runs file for this test, so why
    -- do we? Due to reusing test files. The situation here is:
    --
    -- 1. We are using the examples data directory.
    -- 2. We are providing a chart-requests path via
    --    testSimple/chart-requests.toml
    --
    -- The intention is that testSimple/chart-requests.toml overrides
    -- examples/chart-requests.toml, so we shouldn't care about runs.toml
    -- at all. The problem is that the examples directory has both runs.toml
    -- _and_ Activities.csv, so both files will be used for runs, hence
    -- the chart-requests file needs to have its garmin-settings set.
    -- But changing testSimple/chart-requests.toml would screw up that test.
    --
    -- The easiest solution for now is to explicitly provide --runs with the
    -- toml file, so that we don't get a garmin error. But really we should
    -- copy some files to a new testPathChartRequestsOverrideData
    -- directory, to eliminate the test interference.
    runsPath = unsafeDecode [ospPathSep|examples/runs.toml|]

    -- Arbitrarily using the examples as the data dir, since we want some
    -- non-XDG directory to be overwritten.
    dataDir = unsafeDecode [osp|examples|]

testPathRunsOverrideData :: IO OsPath -> TestTree
testPathRunsOverrideData getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = const ["chart", "--json", "--runs", runsPath, "--data", dataDir],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "--runs overrides --data",
          testName = [osp|testPathRunsOverrideData|]
        }
    runsPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/runs.toml|]
    dataDir = unsafeDecode [osp|examples|]

testPathChartRequestsOverrideXdg :: IO OsPath -> TestTree
testPathChartRequestsOverrideXdg getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = const ["chart", "--json", "--chart-requests", chartRequestsPath],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "--chart-requests overrides XDG",
          testName = [osp|testPathChartRequestsOverrideXdg|]
        }
    chartRequestsPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/chart-requests.toml|]

testPathRunsOverrideXdg :: IO OsPath -> TestTree
testPathRunsOverrideXdg getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = const ["chart", "--json", "--runs", runsPath],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "--runs overrides XDG",
          testName = [osp|testPathRunsOverrideXdg|]
        }
    runsPath =
      unsafeDecode [ospPathSep|test/functional/data/testSimple/runs.toml|]

testPathConfig :: IO OsPath -> TestTree
testPathConfig getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = const ["chart", "--json", "--config", configPath],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "Uses explicit config",
          testName = [osp|testPathConfig|]
        }

    configPath = unsafeDecode [ospPathSep|examples/config.toml|]

-- TODO: Would be nice to have an e2e test for chart generation i.e. actually
-- invoke node and generate a real html page.
--
-- We do this in CI but it would be nice to have a test here. It would need
-- to be guarded since it requires npm, and we cannot use the existing
-- golden framework since that mocks file writing.
