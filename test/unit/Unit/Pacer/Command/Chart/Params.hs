{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Unit.Pacer.Command.Chart.Params (tests) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReader
      ( CanonicalizePath,
        DoesDirectoryExist,
        DoesFileExist,
        GetCurrentDirectory,
        GetXdgDirectory,
        ListDirectory
      ),
  )
import Pacer.Command.Chart.Params
  ( ChartParams
      ( MkChartParams,
        activityPaths,
        buildDir,
        chartRequestsPath,
        cleanInstall,
        dataDir,
        json
      ),
    ChartParamsArgs,
  )
import Pacer.Command.Chart.Params qualified as Params
import Pacer.Configuration.Config
  ( ChartConfig,
    Config,
    ConfigWithPath (MkConfigWithPath, config, dirPath),
  )
import Pacer.Configuration.Env.Types (CachedPaths, LogEnv (MkLogEnv), runLoggerMock, runReaderLogEnvMock)
import Pacer.Driver (displayInnerMatchKnown)
import Pacer.Utils (SomeSetter (MkSomeSetter))
import Pacer.Utils qualified as Utils
import System.OsPath qualified as OsP
import Unit.Prelude

data MockEnv = MkMockEnv
  { -- | Directories that should exist. The value is their contents.
    knownDirectories :: Map OsPath (Set OsPath),
    -- | Directories that should not exist. We include this so we can strict
    -- i.e. we should distinguish "known failures" (i.e. the test expects a
    -- path to _not_ exist) vs. "unknown failures" i.e. test test is
    -- called with an expected path, due to a bug. The latter should cause
    -- the test to fail.
    knownDirectoriesFalse :: Set OsPath,
    -- | Files that should exist. We need this because we do not always
    -- test files via the directory contents e.g. if we receive a direct path
    -- --chart-requests /path/to/foo.json, we test foo.json directly.
    knownFiles :: Set OsPath
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''MockEnv

baseMockEnv :: MockEnv
baseMockEnv =
  MkMockEnv
    { knownDirectories = Map.empty,
      knownDirectoriesFalse = Set.empty,
      knownFiles = Set.empty
    }

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Params"
    [ successTests,
      failureTests
    ]

-- NOTE: [Unnecessary test values]
--
-- Some of these tests are "overfitted", in the sense that they have more
-- data set than is needed to pass. For instance, a test may set the
-- config directory, even though the directory ultimately does not get used,
-- because it is "empty".
--
-- This lets us test that the empty directory does not throw an error.
--
-- For another example, a directory may have data (per mocked knownDirectories
-- or knownFiles), but still might not get used because something higher
-- priority was found first. This lets us test the priority.
--
-- While this is useful, it can be somewhat confusing, hence we try to mark
-- such cases where applicable.
--
-- Also note that most tests need to at least set xdg and current working
-- directory, since activity-labels isn't usually tested and it will
-- hence try everything.

successTests :: TestTree
successTests =
  testGroup
    "Success"
    [ buildDirTests,
      testEvolvePhaseCliPaths,
      testEvolvePhaseCliData,
      testEvolvePhaseConfigAbsPaths,
      testEvolvePhaseConfigRelPaths,
      testEvolvePhaseConfigAbsData,
      testEvolvePhaseConfigRelData,
      testEvolvePhaseCwd,
      testEvolvePhaseXdgPaths,
      testEvolvePhaseNoXdgSucceeds,
      testEvolvePhaseGarmin,
      testEvolvePhaseBoth
    ]

buildDirTests :: TestTree
buildDirTests =
  testGroup
    "build-dir"
    [ testEvolvePhaseCliBuildDirAbs,
      testEvolvePhaseCliBuildDirRel,
      testEvolvePhaseConfigBuildDirAbs,
      testEvolvePhaseConfigBuildDirRel
    ]

testEvolvePhaseCliBuildDirAbs :: TestTree
testEvolvePhaseCliBuildDirAbs =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses CLI absolute build-dir",
        testName = [osp|testEvolvePhaseCliBuildDirAbs|],
        runner = goldenRunner mockEnv params Nothing
      }
  where
    params = baseChartParams {buildDir = Just (toOsPath absBuildDir)}

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          goodDir
        ]

    goodDir =
      ( toOsPath xdgConfigPacer,
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|activities.json|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseCliBuildDirRel :: TestTree
testEvolvePhaseCliBuildDirRel =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses CLI relative build-dir",
        testName = [osp|testEvolvePhaseCliBuildDirRel|],
        runner = goldenRunner mockEnv params Nothing
      }
  where
    params = baseChartParams {buildDir = Just [osp|build-dir|]}

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          goodDir
        ]

    goodDir =
      ( toOsPath xdgConfigPacer,
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|activities.json|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseConfigBuildDirAbs :: TestTree
testEvolvePhaseConfigBuildDirAbs =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config absolute build-dir",
        testName = [osp|testEvolvePhaseConfigBuildDirAbs|],
        runner = goldenRunner mockEnv baseChartParams (Just config)
      }
  where
    config =
      set'
        (#config % #chartConfig %? #buildDir)
        (Just (toOsPath absBuildDir))
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          goodDir
        ]

    goodDir =
      ( toOsPath xdgConfigPacer,
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|activities.json|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseConfigBuildDirRel :: TestTree
testEvolvePhaseConfigBuildDirRel =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config relative build-dir",
        testName = [osp|testEvolvePhaseConfigBuildDirRel|],
        runner = goldenRunner mockEnv baseChartParams (Just config)
      }
  where
    config =
      set'
        (#config % #chartConfig %? #buildDir)
        (Just [osp|build-dir|])
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          goodDir
        ]

    goodDir =
      ( toOsPath xdgConfigPacer,
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|activities.json|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseCliPaths :: TestTree
testEvolvePhaseCliPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses CLI paths",
        testName = [osp|testEvolvePhaseCliPaths|],
        runner = goldenRunner mockEnv params (Just config)
      }
  where
    params =
      baseChartParams
        { activityPaths = [[osp|cli-activities.json|]],
          chartRequestsPath = Just [osp|cli-cr.json|],
          dataDir = Just [osp|cli-data|]
        }

    -- Even though config is set, we use the CLI paths, since the latter have
    -- higher priority.
    config =
      over'
        (#config % #chartConfig % _Just)
        ( Utils.setMany' @List @ChartConfig
            [ MkSomeSetter #activityPaths [[osp|config-activities.json|]],
              MkSomeSetter #chartRequestsPath (Just [osp|config-cr.json|]),
              -- NOTE: [Config directory]
              --
              -- Note that this config's dataDir is relative to the config's
              -- path, which is hardcoded to /config-dir.
              -- See NOTE: [Config directory].
              --
              -- Hence if dataDir d is given then the directory
              -- /config-dir/d needs to exist. Since it is merely the current
              -- path here, the directory is /config-data below.
              MkSomeSetter #dataDir (Just [osp|./|])
            ]
        )
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          (toOsPath xdgConfigPacer, Set.empty),
          (toOsPath $ rootPath <</>> [reldirPathSep|config-data|], Set.empty),
          (toOsPath $ rootPath <</>> [reldir|cli-data|], Set.empty)
        ]

    knownFiles =
      Set.fromList
        [ toOsPath $ rootPath <</>> [relfile|cli-cr.json|],
          toOsPath $ rootPath <</>> [relfile|cli-activities.json|]
        ]

    mockEnv =
      baseMockEnv
        { knownDirectories,
          knownFiles
        }

testEvolvePhaseCliData :: TestTree
testEvolvePhaseCliData =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses CLI data",
        testName = [osp|testEvolvePhaseCliData|],
        runner = goldenRunner mockEnv params (Just config)
      }
  where
    params = baseChartParams {dataDir = Just [osp|cli-data|]}

    -- Even though config is set, we use the CLI paths, since the latter have
    -- higher priority.
    config =
      over'
        (#config % #chartConfig % _Just)
        ( Utils.setMany' @List @ChartConfig
            [ MkSomeSetter #activityPaths [[osp|config-activities.json|]],
              MkSomeSetter #chartRequestsPath (Just [osp|config-cr.json|]),
              MkSomeSetter #dataDir (Just [osp|./|])
            ]
        )
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          (toOsPath xdgConfigPacer, Set.empty),
          (toOsPath $ rootPath <</>> [reldirPathSep|config-data|], Set.empty),
          goodDir
        ]

    goodDir =
      ( toOsPath $ rootPath <</>> [reldir|cli-data|],
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|activities.json|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseConfigAbsPaths :: TestTree
testEvolvePhaseConfigAbsPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config absolute paths",
        testName = [osp|testEvolvePhaseConfigAbsPaths|],
        runner = goldenRunner mockEnv params (Just config)
      }
  where
    -- NOTE: [Unused data dir]
    --
    -- Even with --data specified, we will skip to config since the
    -- former contains no paths.
    params = baseChartParams {dataDir = Just [osp|no-data|]}

    config =
      over'
        (#config % #chartConfig % _Just)
        ( Utils.setMany' @List @ChartConfig
            [ MkSomeSetter #activityPaths [rootOsPath </> [osp|config-activities.json|]],
              MkSomeSetter #chartRequestsPath (Just $ rootOsPath </> [osp|config-cr.json|]),
              MkSomeSetter #dataDir (Just [osp|./|])
            ]
        )
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          (toOsPath xdgConfigPacer, Set.empty),
          (toOsPath $ rootPath <</>> [reldirPathSep|config-data|], Set.empty),
          -- see NOTE: [Unnecessary test values]
          (toOsPath $ rootPath <</>> [reldir|no-data|], Set.empty)
        ]

    knownFiles =
      Set.fromList
        [ toOsPath $ rootPath <</>> [relfile|config-cr.json|],
          toOsPath $ rootPath <</>> [relfile|config-activities.json|]
        ]

    mockEnv =
      baseMockEnv
        { knownDirectories,
          knownFiles
        }

testEvolvePhaseConfigRelPaths :: TestTree
testEvolvePhaseConfigRelPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config relative paths",
        testName = [osp|testEvolvePhaseConfigRelPaths|],
        runner = goldenRunner mockEnv params (Just config)
      }
  where
    -- see NOTE: [Unused data dir]
    params = baseChartParams {dataDir = Just [osp|no-data|]}

    config =
      over'
        (#config % #chartConfig % _Just)
        ( Utils.setMany' @List @ChartConfig
            [ MkSomeSetter #activityPaths [[osp|rel-activities.json|]],
              MkSomeSetter #chartRequestsPath (Just [osp|rel-cr.json|]),
              MkSomeSetter #dataDir (Just [osp|./|])
            ]
        )
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          (toOsPath xdgConfigPacer, Set.empty),
          goodDir,
          -- see NOTE: [Unnecessary test values]
          (toOsPath $ rootPath <</>> [reldir|no-data|], Set.empty)
        ]

    -- NOTE: Need to set this since we expect the paths to be relative to the
    -- config's directory, and the DoesFileExist handler uses the directory's
    -- contents when the former is part of knownDirectories. It only defers
    -- to knownFiles if the directory does _not_ exist. Hence, because the
    -- config-data exists per knownDirectories, we need to add the files here.
    --
    -- Alternatively, we could change the handler to check the contents ||
    -- knownFiles, but this is the only affected test at the moment, so we keep
    -- it simpler for now.
    goodDir =
      ( toOsPath $ rootPath <</>> [reldirPathSep|config-data|],
        Set.fromList
          [ [osp|rel-cr.json|],
            [osp|rel-activities.json|]
          ]
      )

    mockEnv =
      baseMockEnv
        { knownDirectories
        }

testEvolvePhaseConfigAbsData :: TestTree
testEvolvePhaseConfigAbsData =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config absolute data",
        testName = [osp|testEvolvePhaseConfigAbsData|],
        runner = goldenRunner mockEnv params (Just config)
      }
  where
    -- see NOTE: [Unused data dir]
    params = baseChartParams {dataDir = Just [osp|no-data|]}

    config =
      set'
        (#config % #chartConfig %? #dataDir)
        (Just $ rootOsPath </> [osp|config-data|])
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          (toOsPath xdgConfigPacer, Set.empty),
          goodDir,
          -- see NOTE: [Unnecessary test values]
          (toOsPath $ rootPath <</>> [reldir|no-data|], Set.empty)
        ]

    goodDir =
      ( toOsPath $ rootPath <</>> [reldir|config-data|],
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|activities.json|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseConfigRelData :: TestTree
testEvolvePhaseConfigRelData =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config relative data",
        testName = [osp|testEvolvePhaseConfigRelData|],
        runner = goldenRunner mockEnv params (Just config)
      }
  where
    -- see NOTE: [Unused data dir]
    params = baseChartParams {dataDir = Just [osp|no-data|]}

    config =
      set'
        (#config % #chartConfig %? #dataDir)
        (Just [osp|./|])
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          (toOsPath xdgConfigPacer, Set.empty),
          goodDir,
          -- see NOTE: [Unnecessary test values]
          (toOsPath $ rootPath <</>> [reldir|no-data|], Set.empty)
        ]

    goodDir =
      ( toOsPath $ rootPath <</>> [reldir|config-data|],
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|activities.json|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseCwd :: TestTree
testEvolvePhaseCwd =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses current directory",
        testName = [osp|testEvolvePhaseCwd|],
        runner = goldenRunner mockEnv baseChartParams (Just config)
      }
  where
    -- Config exists to prove that current directory overrides config. If it
    -- didn't (i.e. findConfigPath was first), we'd receive an error since
    -- the config-data/ dir is not properly mocked (hence does not exist).
    config =
      set'
        (#config % #chartConfig %? #dataDir)
        (Just [osp|./|])
        baseConfigWithPath

    knownDirectories = Map.fromList [goodDir]

    goodDir =
      ( toOsPath cwdPath,
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|Activities.csv|],
            [osp|activities.json|],
            [osp|activity-labels.json|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseXdgPaths :: TestTree
testEvolvePhaseXdgPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses xdg paths",
        testName = [osp|testEvolvePhaseXdgPaths|],
        runner = goldenRunner mockEnv params (Just config)
      }
  where
    -- see NOTE: [Unused data dir]
    params = baseChartParams {dataDir = Just [osp|no-data|]}

    config =
      -- Even with config.data specified, we will skip to config since the
      -- former contains no paths.
      set'
        (#config % #chartConfig %? #dataDir)
        (Just [osp|no-data|])
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          goodDir,
          (toOsPath $ rootPath <</>> [reldir|no-data|], Set.empty),
          -- see NOTE: [Unnecessary test values]
          (toOsPath $ rootPath <</>> [reldirPathSep|config-data/no-data|], Set.empty)
        ]

    goodDir =
      ( toOsPath xdgConfigPacer,
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|activities.json|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseNoXdgSucceeds :: TestTree
testEvolvePhaseNoXdgSucceeds =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Xdg is not required",
        testName = [osp|testEvolvePhaseNoXdgSucceeds|],
        runner = goldenRunner mockEnv params Nothing
      }
  where
    -- Tests that the xdg directory is not required by ensuring that we
    -- use an xdg directory that does not "exist", per XdgMissing.
    --
    -- This hits the check when searching for activity-labels, which are
    -- not required in general, hence it falls through all checks.
    --
    -- We don't do this with chart-requests or activities since those are
    -- (eventually) required, hence will error if nothing is found
    -- (this is separate from a hypothetical xdg error).
    params =
      baseChartParams
        { activityPaths = [[osp|cli-activities.json|]],
          chartRequestsPath = Just [osp|cli-cr.json|]
        }

    knownDirectories = Map.fromList [(toOsPath cwdPath, Set.empty)]

    knownDirectoriesFalse =
      Set.fromList
        [ toOsPath $ rootPath <</>> [reldirPathSep|xdg/config/pacer|]
        ]

    knownFiles =
      Set.fromList
        [ toOsPath $ rootPath <</>> [relfile|cli-cr.json|],
          toOsPath $ rootPath <</>> [relfile|cli-activities.json|]
        ]

    mockEnv =
      baseMockEnv
        { knownDirectories,
          knownDirectoriesFalse,
          knownFiles
        }

testEvolvePhaseGarmin :: TestTree
testEvolvePhaseGarmin =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses garmin path",
        testName = [osp|testEvolvePhaseCliGarmin|],
        runner = goldenRunner mockEnv params Nothing
      }
  where
    params = baseChartParams {dataDir = Just [osp|cli-garmin|]}

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          (toOsPath xdgConfigPacer, Set.empty),
          goodDir
        ]

    goodDir =
      ( toOsPath $ rootPath <</>> [reldir|cli-garmin|],
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|activities.csv|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseBoth :: TestTree
testEvolvePhaseBoth =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses Json and Garmin when both activities types exist",
        testName = [osp|testEvolvePhaseBoth|],
        runner = goldenRunner mockEnv params Nothing
      }
  where
    params = baseChartParams {dataDir = Just [osp|cli-both|]}

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          (toOsPath xdgConfigPacer, Set.empty),
          goodDir
        ]

    goodDir =
      ( toOsPath $ rootPath <</>> [reldir|cli-both|],
        Set.fromList
          [ [osp|chart-requests.json|],
            [osp|activities.csv|],
            [osp|activities.json|]
          ]
      )

    mockEnv = baseMockEnv {knownDirectories}

failureTests :: TestTree
failureTests =
  testGroup
    "Failures"
    [ testEvolvePhaseCliPathsEx,
      testEvolvePhaseCliDataDirEx,
      testEvolvePhaseConfigPathsEx,
      testEvolvePhaseMissingEx
    ]

testEvolvePhaseCliPathsEx :: TestTree
testEvolvePhaseCliPathsEx =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Exception for unknown CLI paths",
        testName = [osp|testEvolvePhaseCliPathsEx|],
        runner = goldenRunner baseMockEnv params (Just config)
      }
  where
    params =
      baseChartParams
        { activityPaths = [[osp|bad_activities.json|]],
          chartRequestsPath = Just [osp|bad_cr.json|],
          dataDir = Just [osp|cli-data|]
        }

    config =
      over'
        (#config % #chartConfig % _Just)
        ( Utils.setMany' @List @ChartConfig
            [ MkSomeSetter #activityPaths [[osp|rel-activities.json|]],
              MkSomeSetter #chartRequestsPath (Just [osp|rel-cr.json|]),
              MkSomeSetter #dataDir (Just [osp|config-data|])
            ]
        )
        baseConfigWithPath

testEvolvePhaseCliDataDirEx :: TestTree
testEvolvePhaseCliDataDirEx =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Exception for unknown CLI data dir",
        testName = [osp|testEvolvePhaseCliDataDirEx|],
        runner = goldenRunner mockEnv params Nothing
      }
  where
    params = baseChartParams {dataDir = Just $ rootOsPath </> [osp|bad_dir|]}

    knownDirectoriesFalse =
      Set.fromList
        [ toOsPath $ rootPath <</>> [reldir|bad_dir|]
        ]

    mockEnv = baseMockEnv {knownDirectoriesFalse}

testEvolvePhaseConfigPathsEx :: TestTree
testEvolvePhaseConfigPathsEx =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Exception for unknown config paths",
        testName = [osp|testEvolvePhaseConfigPathsEx|],
        runner = goldenRunner mockEnv baseChartParams (Just config)
      }
  where
    config =
      over'
        (#config % #chartConfig % _Just)
        ( Utils.setMany' @List @ChartConfig
            [ MkSomeSetter #activityPaths [[osp|bad-activities.json|]],
              MkSomeSetter #chartRequestsPath (Just [osp|bad-cr.json|]),
              MkSomeSetter #dataDir (Just [osp|./|])
            ]
        )
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          (toOsPath xdgConfigPacer, Set.empty),
          (toOsPath $ rootPath <</>> [reldirPathSep|config-data|], Set.empty)
        ]

    mockEnv = baseMockEnv {knownDirectories}

testEvolvePhaseMissingEx :: TestTree
testEvolvePhaseMissingEx =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Exception for missing paths",
        testName = [osp|testEvolvePhaseMissingEx|],
        runner = goldenRunner mockEnv params (Just config)
      }
  where
    params = baseChartParams {dataDir = Just [osp|some-dir|]}

    config =
      set'
        (#config % #chartConfig %? #dataDir)
        (Just [osp|some-config-data|])
        baseConfigWithPath

    knownDirectories =
      Map.fromList
        [ (toOsPath cwdPath, Set.empty),
          (toOsPath $ rootPath <</>> [reldirPathSep|xdg/config/pacer|], Set.empty),
          (toOsPath $ rootPath <</>> [reldir|some-dir|], Set.empty),
          (toOsPath $ rootPath <</>> [reldirPathSep|config-data/some-config-data|], Set.empty)
        ]

    mockEnv = baseMockEnv {knownDirectories}

runPathReaderMock ::
  (Reader MockEnv :> es) =>
  Eff (PathReader : es) a ->
  Eff es a
runPathReaderMock = interpret_ $ \case
  CanonicalizePath p -> pure $ rootOsPath </> p
  DoesDirectoryExist p -> do
    knownDirs <- asks @MockEnv (view #knownDirectories)
    knownDirsFalse <- asks @MockEnv (view #knownDirectoriesFalse)
    if
      | p `Map.member` knownDirs -> pure True
      | p `Set.member` knownDirsFalse -> pure False
      | otherwise -> error $ "doesDirectoryExist: unexpected dir: " ++ show p
  DoesFileExist p -> do
    knownDirs <- asks @MockEnv (view #knownDirectories)
    let (d, fn) = OsP.splitFileName p

    case Map.lookup d knownDirs of
      -- Check to see if this is path is part of a directory known to the
      -- test, in which case existence should be based on the directory's
      -- contents.
      Just contents -> pure $ fn `Set.member` contents
      -- The path does not belong to a known directory i.e. it is was
      -- explicitly given (e.g. --chart-requests). Check if it is a known
      -- explicit path.
      Nothing -> do
        knownFiles <- asks @MockEnv (view #knownFiles)
        pure $ p `Set.member` knownFiles
  GetCurrentDirectory -> pure $ toOsPath cwdPath
  GetXdgDirectory d p ->
    case d of
      XdgConfig -> pure $ toOsPath xdgConfig </> p
      _ -> error $ "runPathReaderMock: unexpected xdg type: " <> show d
  ListDirectory p -> do
    knownDirs <- asks @MockEnv (view #knownDirectories)
    case Map.lookup p knownDirs of
      Nothing -> error $ "ListDirectory: unexpected dir: " ++ show p
      Just contents -> pure $ Set.toList contents
  other -> error $ "runPathReaderMock: unimplemented: " ++ showEffectCons other

goldenRunner ::
  -- | MockEnv, used so that tests can determine which paths should exist.
  MockEnv ->
  -- | Evolve args.
  ChartParamsArgs ->
  -- | Optional config.
  Maybe ConfigWithPath ->
  IO ByteString
goldenRunner mockEnv params mConfigPath = do
  trySync (runner $ Params.evolvePhase @LogEnv params mConfigPath) <&> \case
    Right x -> pShowBS x
    -- displayInner over displayException since we do not want unstable
    -- callstacks in output.
    Left ex -> encodeUtf8 $ packText $ displayInnerMatchKnown ex
  where
    runner =
      runEff
        . evalState @CachedPaths mempty
        . runReader @MockEnv mockEnv
        . runPathReaderMock
        . runLoggerMock
        . runReaderLogEnvMock
        . runReader logEnv

    logEnv = MkLogEnv Nothing mempty mempty

baseChartParams :: ChartParamsArgs
baseChartParams =
  MkChartParams
    { buildDir = Nothing,
      cleanInstall = False,
      chartRequestsPath = Nothing,
      dataDir = Nothing,
      json = False,
      activityLabelsPath = Nothing,
      activityPaths = [],
      port = Nothing
    }

baseConfigWithPath :: ConfigWithPath
baseConfigWithPath =
  MkConfigWithPath
    { -- NOTE: [Config directory]
      dirPath = rootPath <</>> [reldir|config-data|],
      config = baseConfig
    }

rootOsPath :: OsPath
rootOsPath = toOsPath rootPath

baseConfig :: Config
baseConfig = set' #chartConfig (Just mempty) mempty

absBuildDir :: Path Abs Dir
absBuildDir = rootPath <</>> [reldirPathSep|abs/build-dir|]

-- NOTE: xdg and cwd can be defined globally here as there is no reason for
-- a particular test to need a different one. Tests can vary by changing
-- the behavior wrt knownDirectories.

xdgConfigPacer :: Path Abs Dir
xdgConfigPacer = xdgConfig <</>> [reldirPathSep|pacer|]

xdgConfig :: Path Abs Dir
xdgConfig = rootPath <</>> [reldirPathSep|xdg/config|]

cwdPath :: Path Abs Dir
cwdPath = rootPath <</>> [reldir|cwd|]

{- ORMOLU_DISABLE -}

rootPath :: Path Abs Dir
rootPath =
#if POSIX
  [absdir|/|]
#else
  [absdir|C:\|]
#endif

{- ORMOLU_ENABLE -}
