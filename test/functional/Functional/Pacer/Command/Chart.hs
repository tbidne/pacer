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
      testGarminChartError getTestDir
    ]

testExampleChart :: IO OsPath -> TestTree
testExampleChart getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = const ["chart", "--data", dataDir, "--json"],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "Generates example",
          testName = [osp|testExampleChart|]
        }
    dataDir = unsafeDecode [osp|examples|]

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
testGarminChart getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs =
            const
              [ "chart",
                "--chart-requests",
                chartRequestsPath,
                "--runs",
                runsPath,
                "--json"
              ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "Generates garmin example",
          testName = [osp|testGarminChart|]
        }
    chartRequestsPath = unsafeDecode [ospPathSep|examples/chart-requests-garmin.toml|]
    runsPath = unsafeDecode [ospPathSep|examples/Activities.csv|]

testGarminChartError :: IO OsPath -> TestTree
testGarminChartError getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs =
            const
              [ "chart",
                "--chart-requests",
                chartRequestsPath,
                "--runs",
                runsPath,
                "--json"
              ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "Garmin fails without chart-requests garmin.settings",
          testName = [osp|testGarminChartError|]
        }
    chartRequestsPath = unsafeDecode [ospPathSep|examples/chart-requests.toml|]
    runsPath = unsafeDecode [ospPathSep|examples/Activities.csv|]

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
                dataDir
              ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "--chart-requests overrides --data",
          testName = [osp|testPathChartRequestsOverrideData|]
        }
    chartRequestsPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/chart-requests.toml|]

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
