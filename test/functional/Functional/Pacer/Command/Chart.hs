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
      testBothChartOverlapError getTestDir,
      testUnknownKeyFailure getTestDir,
      testChartSum getTestDir
    ]

testExampleChart :: IO OsPath -> TestTree
testExampleChart getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "chart",
              "--data",
              dataDir,
              "--runs",
              runsPath,
              "--json",
              "--build-dir",
              buildDir p
            ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "Generates example",
          testName = [osp|testExampleChart|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    dataDir = unsafeDecode [osp|examples|]
    runsPath = unsafeDecode [ospPathSep|examples/runs.jsonc|]

testSimple :: IO OsPath -> TestTree
testSimple = testChart "Simple example" [osp|testSimple|]

testSimpleBuildDir :: IO OsPath -> TestTree
testSimpleBuildDir getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "chart",
              "--build-dir",
              buildDir p,
              "--data",
              dataDir,
              "--json"
            ],
          outFileName = Just [ospPathSep|another-build/charts.json|],
          testDesc = "Simple with build-dir",
          testName = [osp|testSimpleBuildDir|]
        }
    buildDir p = unsafeDecode $ p </> [osp|another-build|]
    dataDir =
      unsafeDecode [ospPathSep|test/functional/data/testSimpleBuildDir|]

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
        { mkArgs = \p ->
            [ "chart",
              "--json",
              "--data",
              dataDir,
              "--build-dir",
              buildDir p
            ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "Generates example with default and garmin",
          testName = [osp|testDefaultAndGarminExamples|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
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

testUnknownKeyFailure :: IO OsPath -> TestTree
testUnknownKeyFailure = testChartPosix True desc [osp|testUnknownKeyFailure|]
  where
    desc = "Unknown key causes an error"

testChartSum :: IO OsPath -> TestTree
testChartSum = testChart desc [osp|testChartSum|]
  where
    desc = "Uses chart sum"

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
        { mkArgs = \p ->
            [ "chart",
              "--json",
              "--build-dir",
              buildDir p
            ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "No paths uses XDG config",
          testName = [osp|testPathXdg|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]

testPathChartRequestsOverrideData :: IO OsPath -> TestTree
testPathChartRequestsOverrideData getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "chart",
              "--json",
              "--chart-requests",
              chartRequestsPath,
              "--data",
              dataDir,
              "--runs",
              runsPath,
              "--build-dir",
              buildDir p
            ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "--chart-requests overrides --data",
          testName = [osp|testPathChartRequestsOverrideData|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    chartRequestsPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/chart-requests.json|]

    -- TODO:
    --
    -- We shouldn't _have_ to specify the runs file for this test, so why
    -- do we? Due to reusing test files. The situation here is:
    --
    -- 1. We are using the examples data directory.
    -- 2. We are providing a chart-requests path via
    --    testSimple/chart-requests.json
    --
    -- The intention is that testSimple/chart-requests.json overrides
    -- examples/chart-requests.json, so we shouldn't care about runs.json
    -- at all. The problem is that the examples directory has both runs.json
    -- _and_ Activities.csv, so both files will be used for runs, hence
    -- the chart-requests file needs to have its garmin-settings set.
    -- But changing testSimple/chart-requests.json would screw up that test.
    --
    -- The easiest solution for now is to explicitly provide --runs with the
    -- json file, so that we don't get a garmin error. But really we should
    -- copy some files to a new testPathChartRequestsOverrideData
    -- directory, to eliminate the test interference.
    runsPath = unsafeDecode [ospPathSep|examples/runs.jsonc|]

    -- Arbitrarily using the examples as the data dir, since we want some
    -- non-XDG directory to be overwritten.
    dataDir = unsafeDecode [osp|examples|]

testPathRunsOverrideData :: IO OsPath -> TestTree
testPathRunsOverrideData getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "chart",
              "--json",
              "--runs",
              runsPath,
              "--data",
              dataDir,
              "--build-dir",
              buildDir p
            ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "--runs overrides --data",
          testName = [osp|testPathRunsOverrideData|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    runsPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/runs.json|]
    dataDir = unsafeDecode [osp|examples|]

testPathChartRequestsOverrideXdg :: IO OsPath -> TestTree
testPathChartRequestsOverrideXdg getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "chart",
              "--json",
              "--chart-requests",
              chartRequestsPath,
              "--build-dir",
              buildDir p
            ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "--chart-requests overrides XDG",
          testName = [osp|testPathChartRequestsOverrideXdg|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    chartRequestsPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/chart-requests.json|]

testPathRunsOverrideXdg :: IO OsPath -> TestTree
testPathRunsOverrideXdg getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "chart",
              "--json",
              "--runs",
              runsPath,
              "--build-dir",
              buildDir p
            ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "--runs overrides XDG",
          testName = [osp|testPathRunsOverrideXdg|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    runsPath =
      unsafeDecode [ospPathSep|test/functional/data/testSimple/runs.json|]

testPathConfig :: IO OsPath -> TestTree
testPathConfig getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "chart",
              "--json",
              "--build-dir",
              buildDir p,
              "--config",
              configPath
            ],
          outFileName = Just [ospPathSep|build/charts.json|],
          testDesc = "Uses explicit config",
          testName = [osp|testPathConfig|]
        }

    buildDir p = unsafeDecode $ p </> [osp|build|]
    configPath = unsafeDecode [ospPathSep|examples/config.jsonc|]

-- TODO: Would be nice to have an e2e test for chart generation i.e. actually
-- invoke node and generate a real html page.
--
-- We do this in CI but it would be nice to have a test here. It would need
-- to be guarded since it requires npm, and we cannot use the existing
-- golden framework since that mocks file writing.
