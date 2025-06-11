{-# LANGUAGE QuasiQuotes #-}

module Functional.Pacer.Command.Chart (tests) where

import Data.IORef qualified as Ref
import Data.Text qualified as T
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
      testFilterGlobal getTestDir,
      testFilterGlobalMultiTypes getTestDir,
      testFilterEmptyError getTestDir,
      testFilterParseExprError getTestDir,
      testFilterTypeEarly getTestDir,
      testDuplicateDateError getTestDir,
      testGarminChart getTestDir,
      testDefaultAndGarminExamples getTestDir,
      testDefaultAndGarmin getTestDir,
      testDefaultAndGarminSameCase getTestDir,
      testActivitiesInfix getTestDir,
      testCaseInsensitive getTestDir,
      testGarminChartError getTestDir,
      testBothChartOverlapError getTestDir,
      testUnknownKeyFailure getTestDir,
      testChartSum getTestDir,
      testChartSmooth getTestDir,
      testUnmatchedActivityLabelWarning getTestDir
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
              "--activities",
              activitiesPath,
              "--json",
              "--build-dir",
              buildDir p
            ],
          outFileName = GoldenOutputFile [ospPathSep|build/charts.json|],
          testDesc = "Generates example",
          testName = [osp|testExampleChart|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    dataDir = unsafeDecode [osp|examples|]
    activitiesPath = unsafeDecode [ospPathSep|examples/activities.jsonc|]

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
          outFileName = GoldenOutputFile [ospPathSep|another-build/charts.json|],
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

testFilterGlobal :: IO OsPath -> TestTree
testFilterGlobal = testChart desc [osp|testFilterGlobal|]
  where
    desc = "Uses global filters"

testFilterGlobalMultiTypes :: IO OsPath -> TestTree
testFilterGlobalMultiTypes = testChart desc [osp|testFilterGlobalMultiTypes|]
  where
    desc = "Uses global filters with multiple types"

testFilterEmptyError :: IO OsPath -> TestTree
testFilterEmptyError = testChart desc [osp|testFilterEmptyError|]
  where
    desc = "Filter empty error"

testFilterParseExprError :: IO OsPath -> TestTree
testFilterParseExprError =
  testChartOs True desc [osp|testFilterParseExprError|]
  where
    desc = "Filter parse expr error"

-- Both activities files in testFilterTypeEarly contain errors, so normally
-- this would log two messages (at the error level). However, because they
-- are filtered out, we never see the error because the parse failure never
-- occurs.
--
-- Hence this test verifies that we are correctly applying global type filters
-- before a full parse.
--
-- We set the log level to error because the info level contains
-- non-deterministic filepaths and timestamps.
testFilterTypeEarly :: IO OsPath -> TestTree
testFilterTypeEarly getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "--log-level",
              "error",
              "chart",
              "--json",
              "--data",
              dataDir,
              "--build-dir",
              buildDir p
            ],
          outFileName = GoldenOutputFileLogs [ospPathSep|build/charts.json|],
          testDesc = "Global type filters applied before full parse",
          testName = [osp|testFilterTypeEarly|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    dataDir = unsafeDecode [ospPathSep|test/functional/data/testFilterTypeEarly|]

testDuplicateDateError :: IO OsPath -> TestTree
testDuplicateDateError = testChartOs True desc [osp|testDuplicateDateError|]
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
          outFileName = GoldenOutputFile [ospPathSep|build/charts.json|],
          testDesc = "Generates example with default and garmin",
          testName = [osp|testDefaultAndGarminExamples|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    dataDir = unsafeDecode [osp|examples|]

-- testDefaultAndGarmin and testDefaultAndGarminSameCase exist to test that
-- we find both activitities file, even when they share the same case.
--
-- That is, testDefaultAndGarmin is the relatively standard
--
--   activities.json, Activities.csv
--
-- while testDefaultAndGarminSameCase has
--
--   activities.json, activities.csv
--
-- Both should be the same.

testDefaultAndGarmin :: IO OsPath -> TestTree
testDefaultAndGarmin = testChart desc [osp|testDefaultAndGarmin|]
  where
    desc = "Uses default and garmin activities files"

testDefaultAndGarminSameCase :: IO OsPath -> TestTree
testDefaultAndGarminSameCase = testChart desc [osp|testDefaultAndGarminSameCase|]
  where
    desc = "Uses default and garmin activities files with same case"

testActivitiesInfix :: IO OsPath -> TestTree
testActivitiesInfix = testChart desc [osp|testActivitiesInfix|]
  where
    desc = "Matches multiple activity files via infix"

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
    desc = "Datetime overlap across multiple activity files errors"

-- Previously, any error was an immediate error. To achieve consistency
-- with garmin, individual activities are allowed to fail, and we continue.
-- Hence this test now verifies that the bad activity is excluded, but
-- the rest works.
testUnknownKeyFailure :: IO OsPath -> TestTree
testUnknownKeyFailure = testChart desc [osp|testUnknownKeyFailure|]
  where
    desc = "Skips activity with unknown key"

testChartSum :: IO OsPath -> TestTree
testChartSum = testChart desc [osp|testChartSum|]
  where
    desc = "Uses chart sum"

testChartSmooth :: IO OsPath -> TestTree
testChartSmooth = testChart desc [osp|testChartSmooth|]
  where
    desc = "Uses chart sum with smooth"

testUnmatchedActivityLabelWarning :: IO OsPath -> TestTree
testUnmatchedActivityLabelWarning getTestDir = testCase desc $ do
  testDir <- getTestDir

  let args = mkArgs testDir

  funcEnv <- runAppArgs args

  logs <- Ref.readIORef funcEnv.logsRef

  let mkErr e =
        unpackText
          $ mconcat
            [ "Did not find expected '",
              e,
              "' in logs: ",
              logs
            ]

  assertBool (mkErr expected1) (expected1 `T.isInfixOf` logs)
  assertBool (mkErr expected2) (expected2 `T.isInfixOf` logs)
  where
    desc = "Logs warnings for unused activity-labels"

    expected1 = "The following timestamps with labels from"
    expected2 =
      (T.strip . T.unlines)
        [ "were not found in activities files:",
          "  - 2024-10-15 12:35:20: [unused_1]",
          "  - 2024-10-20: [unused_3, unused_4]",
          "  - 2024-10-20 14:30:00 -0800: [unused_2]"
        ]

    mkArgs p =
      [ "chart",
        "--json",
        "--data",
        dataDir,
        "--build-dir",
        buildDir p
      ]
    buildDir p = unsafeDecode $ p </> [osp|build|]
    dataDir =
      unsafeDecode
        [ospPathSep|test/functional/data/testUnmatchedActivityLabelWarning|]

pathTests :: IO OsPath -> TestTree
pathTests getTestDir =
  testGroup
    "Paths"
    [ testPathXdg getTestDir,
      testPathChartRequestsOverrideData getTestDir,
      testPathActivitiesOverrideData getTestDir,
      testPathChartRequestsOverrideXdg getTestDir,
      testPathActivitiesOverrideXdg getTestDir,
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
          outFileName = GoldenOutputFile [ospPathSep|build/charts.json|],
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
              "--build-dir",
              buildDir p
            ],
          outFileName = GoldenOutputFile [ospPathSep|build/charts.json|],
          testDesc = "--chart-requests overrides --data",
          testName = [osp|testPathChartRequestsOverrideData|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    chartRequestsPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/chart-requests.json|]

    -- Want this dir overridden for chart-requests
    dataDir = unsafeDecode [osp|test/functional/data/testPathChartRequestsOverrideData|]

testPathActivitiesOverrideData :: IO OsPath -> TestTree
testPathActivitiesOverrideData getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "chart",
              "--json",
              "--activities",
              activitiesPath,
              "--data",
              dataDir,
              "--build-dir",
              buildDir p
            ],
          outFileName = GoldenOutputFile [ospPathSep|build/charts.json|],
          testDesc = "--activities overrides --data",
          testName = [osp|testPathActivitiesOverrideData|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    activitiesPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/activities.json|]
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
          outFileName = GoldenOutputFile [ospPathSep|build/charts.json|],
          testDesc = "--chart-requests overrides XDG",
          testName = [osp|testPathChartRequestsOverrideXdg|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    chartRequestsPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/chart-requests.json|]

testPathActivitiesOverrideXdg :: IO OsPath -> TestTree
testPathActivitiesOverrideXdg getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "chart",
              "--json",
              "--activities",
              activitiesPath,
              "--build-dir",
              buildDir p
            ],
          outFileName = GoldenOutputFile [ospPathSep|build/charts.json|],
          testDesc = "--activities overrides XDG",
          testName = [osp|testPathActivitiesOverrideXdg|]
        }
    buildDir p = unsafeDecode $ p </> [osp|build|]
    activitiesPath =
      unsafeDecode
        [ospPathSep|test/functional/data/testSimple/activities.json|]

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
          outFileName = GoldenOutputFile [ospPathSep|build/charts.json|],
          testDesc = "Uses explicit config",
          testName = [osp|testPathConfig|]
        }

    buildDir p = unsafeDecode $ p </> [osp|build|]
    configPath = unsafeDecode [ospPathSep|examples/config.jsonc|]
