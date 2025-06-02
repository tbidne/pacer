{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Params (tests) where

import Data.List qualified as L
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
import Effectful.Logger.Dynamic (Logger (LoggerLog))
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
    ChartParamsFinal,
  )
import Pacer.Command.Chart.Params qualified as Params
import Pacer.Configuration.Config
  ( Config,
    ConfigWithPath (MkConfigWithPath, config, dirPath),
  )
import Pacer.Configuration.Env.Types (CachedPaths, LogEnv (MkLogEnv))
import Pacer.Driver (displayInnerMatchKnown)
import System.OsPath qualified as OsPath
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Params"
    [ successTests,
      failureTests
    ]

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
        runner = goldenRunner params config
      }
  where
    params =
      set'
        #buildDir
        (Just (toOsPath absBuildDir))
        baseChartParams
    config = baseConfig

testEvolvePhaseCliBuildDirRel :: TestTree
testEvolvePhaseCliBuildDirRel =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses CLI relative build-dir",
        testName = [osp|testEvolvePhaseCliBuildDirRel|],
        runner = goldenRunner params config
      }
  where
    params =
      set'
        #buildDir
        (Just [osp|build-dir|])
        baseChartParams
    config = baseConfig

testEvolvePhaseConfigBuildDirAbs :: TestTree
testEvolvePhaseConfigBuildDirAbs =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config absolute build-dir",
        testName = [osp|testEvolvePhaseConfigBuildDirAbs|],
        runner = goldenRunner params config
      }
  where
    params = baseChartParams
    config =
      set'
        (#chartConfig %? #buildDir)
        (Just (toOsPath absBuildDir))
        baseConfig

testEvolvePhaseConfigBuildDirRel :: TestTree
testEvolvePhaseConfigBuildDirRel =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config relative build-dir",
        testName = [osp|testEvolvePhaseConfigBuildDirRel|],
        runner = goldenRunner params config
      }
  where
    params = baseChartParams
    config =
      set'
        (#chartConfig %? #buildDir)
        (Just [osp|build-dir|])
        baseConfig

testEvolvePhaseCliPaths :: TestTree
testEvolvePhaseCliPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses CLI paths",
        testName = [osp|testEvolvePhaseCliPaths|],
        runner = goldenRunner params config
      }
  where
    params =
      set' #chartRequestsPath (Just [osp|cli-cr.json|])
        $ set'
          #dataDir
          (Just [osp|cli-data|])
        $ set'
          #activityPaths
          [[osp|cli-activities.json|]]
          baseChartParams

    config =
      set'
        (#chartConfig %? #activityPaths)
        [[osp|config-activities.json|]]
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|config-cr.json|])
          baseConfig

testEvolvePhaseCliData :: TestTree
testEvolvePhaseCliData =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses CLI data",
        testName = [osp|testEvolvePhaseCliData|],
        runner = goldenRunner params config
      }
  where
    params =
      set'
        #dataDir
        (Just [osp|cli-data|])
        baseChartParams

    config =
      set'
        (#chartConfig %? #activityPaths)
        [[osp|config-activities.json|]]
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|config-cr.json|])
          baseConfig

testEvolvePhaseConfigAbsPaths :: TestTree
testEvolvePhaseConfigAbsPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config absolute paths",
        testName = [osp|testEvolvePhaseConfigAbsPaths|],
        runner = goldenRunner params config
      }
  where
    params =
      -- Even with --data specified, we will skip to config since the
      -- former contains no paths.
      set'
        #dataDir
        (Just [osp|no-data|])
        baseChartParams

    config =
      set'
        (#chartConfig %? #activityPaths)
        [rootOsPath </> [osp|config-activities.json|]]
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just $ rootOsPath </> [osp|config-cr.json|])
          baseConfig

testEvolvePhaseConfigRelPaths :: TestTree
testEvolvePhaseConfigRelPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config relative paths",
        testName = [osp|testEvolvePhaseConfigRelPaths|],
        runner = goldenRunner params config
      }
  where
    params =
      -- Even with --data specified, we will skip to config since the
      -- former contains no paths.
      set'
        #dataDir
        (Just [osp|no-data|])
        baseChartParams

    config =
      set'
        (#chartConfig %? #activityPaths)
        [[osp|rel-activities.json|]]
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|rel-cr.json|])
          baseConfig

testEvolvePhaseConfigAbsData :: TestTree
testEvolvePhaseConfigAbsData =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config absolute data",
        testName = [osp|testEvolvePhaseConfigAbsData|],
        runner = goldenRunner params config
      }
  where
    params =
      -- Even with --data specified, we will skip to config since the
      -- former contains no paths.
      set'
        #dataDir
        (Just [osp|no-data|])
        baseChartParams

    config =
      set'
        (#chartConfig %? #dataDir)
        (Just $ rootOsPath </> [osp|config-data|])
        baseConfig

testEvolvePhaseConfigRelData :: TestTree
testEvolvePhaseConfigRelData =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config relative data",
        testName = [osp|testEvolvePhaseConfigRelData|],
        runner = goldenRunner params config
      }
  where
    params =
      -- Even with --data specified, we will skip to config since the
      -- former contains no paths.
      set'
        #dataDir
        (Just [osp|no-data|])
        baseChartParams

    config =
      set'
        (#chartConfig %? #dataDir)
        (Just [osp|./|])
        baseConfig

testEvolvePhaseCwd :: TestTree
testEvolvePhaseCwd =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses current directory",
        testName = [osp|testEvolvePhaseCwd|],
        runner = goldenRunnerCwd (toOsPath cwd) params config
      }
  where
    params = baseChartParams
    config = baseConfig

    cwd = baseRoot <</>> [reldir|cwd_files|]

testEvolvePhaseXdgPaths :: TestTree
testEvolvePhaseXdgPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses xdg paths",
        testName = [osp|testEvolvePhaseXdgPaths|],
        runner = goldenRunner params config
      }
  where
    params =
      -- Even with --data specified, we will skip to config since the
      -- former contains no paths.
      set'
        #dataDir
        (Just [osp|no-data|])
        baseChartParams

    config =
      -- Even with config.data specified, we will skip to config since the
      -- former contains no paths.
      set'
        (#chartConfig %? #dataDir)
        (Just [osp|no-data|])
        baseConfig

testEvolvePhaseNoXdgSucceeds :: TestTree
testEvolvePhaseNoXdgSucceeds =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Xdg is not required",
        testName = [osp|testEvolvePhaseNoXdgSucceeds|],
        runner = goldenRunnerXdg XdgMissing params config
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
      set' #chartRequestsPath (Just [osp|cli-cr.json|])
        $ set'
          #activityPaths
          [[osp|cli-activities.json|]]
          baseChartParams

    config = baseConfig

testEvolvePhaseGarmin :: TestTree
testEvolvePhaseGarmin =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses garmin path",
        testName = [osp|testEvolvePhaseCliGarmin|],
        runner = goldenRunner params config
      }
  where
    params =
      set'
        #dataDir
        (Just [osp|cli-garmin|])
        baseChartParams
    config = baseConfig

testEvolvePhaseBoth :: TestTree
testEvolvePhaseBoth =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses Json and Garmin when both activities types exist",
        testName = [osp|testEvolvePhaseBoth|],
        runner = goldenRunner params config
      }
  where
    params =
      set'
        #dataDir
        (Just [osp|cli-both|])
        baseChartParams
    config = baseConfig

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
        runner = goldenRunner params config
      }
  where
    params =
      set' #chartRequestsPath (Just [osp|bad_cr.json|])
        $ set'
          #dataDir
          (Just [osp|cli-data|])
        $ set'
          #activityPaths
          [[osp|bad_activities.json|]]
          baseChartParams
    config =
      set'
        (#chartConfig %? #activityPaths)
        [[osp|rel-activities.json|]]
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|rel-cr.json|])
          baseConfig

testEvolvePhaseCliDataDirEx :: TestTree
testEvolvePhaseCliDataDirEx =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Exception for unknown CLI data dir",
        testName = [osp|testEvolvePhaseCliDataDirEx|],
        runner = goldenRunner params config
      }
  where
    params =
      set'
        #dataDir
        (Just $ rootOsPath </> [osp|bad_dir|])
        baseChartParams
    config = baseConfig

testEvolvePhaseConfigPathsEx :: TestTree
testEvolvePhaseConfigPathsEx =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Exception for unknown config paths",
        testName = [osp|testEvolvePhaseConfigPathsEx|],
        runner = goldenRunner params config
      }
  where
    params = baseChartParams
    config =
      set'
        (#chartConfig %? #activityPaths)
        [[osp|bad-activities.json|]]
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|bad-cr.json|])
          baseConfig

testEvolvePhaseMissingEx :: TestTree
testEvolvePhaseMissingEx =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Exception for missing paths",
        testName = [osp|testEvolvePhaseMissingEx|],
        runner = goldenRunnerXdg XdgEmpty params config
      }
  where
    params =
      set'
        #dataDir
        (Just [osp|some-dir|])
        baseChartParams
    config =
      set'
        (#chartConfig %? #dataDir)
        (Just [osp|some-config-data|])
        baseConfig

-- | How to handle xdg directory.
data XdgHandler
  = -- | Xdg directory has pacer files.
    XdgGood
  | -- | Xdg directory does not have pacer files.
    XdgEmpty
  | -- | Xdg directory does not exist.
    XdgMissing
  deriving stock (Eq, Show)

data MockEnv = MkMockEnv
  { cachedPaths :: CachedPaths,
    cwd :: Maybe OsPath,
    knownDirectories :: Set OsPath,
    knownDirectoriesFalse :: Set OsPath,
    knownFiles :: Set OsPath,
    -- Returns xdg dir w/ expected files iff xdg is true
    xdg :: XdgHandler
  }
  deriving stock (Eq, Show)

runEvolvePhase ::
  -- | Current directory override.
  Maybe OsPath ->
  -- | Xdg override.
  XdgHandler ->
  ChartParamsArgs ->
  Maybe ConfigWithPath ->
  IO ChartParamsFinal
runEvolvePhase mCwd xdg params mConfig = do
  runner $ Params.evolvePhase params mConfig
  where
    env =
      MkMockEnv
        { cachedPaths = mempty,
          cwd = mCwd,
          knownDirectories =
            Set.fromList
              $ (rootOsPath </>)
              <$> [ [ospPathSep|bad_xdg/config/pacer/|],
                    [ospPathSep|cli-both/|],
                    [ospPathSep|cli-data/|],
                    [ospPathSep|cli-garmin/|],
                    [ospPathSep|config-data/|],
                    [ospPathSep|config-data/config-data/|],
                    [ospPathSep|config-data/no-data/|],
                    [ospPathSep|config-data/some-config-data/|],
                    (toOsPath cwdPath),
                    (toOsPath $ baseRoot <</>> [reldir|cwd_files|]),
                    [ospPathSep|no-data/|],
                    [ospPathSep|some-dir/|],
                    [ospPathSep|xdg/config/pacer/|]
                  ],
          knownDirectoriesFalse =
            Set.fromList
              $ (rootOsPath </>)
              <$> [ [ospPathSep|missing_xdg/config/pacer/|],
                    [ospPathSep|bad_dir/|]
                  ],
          knownFiles =
            Set.fromList
              $ (rootOsPath </>)
              <$> [ [ospPathSep|cli-cr.json|],
                    [ospPathSep|cli-activities.json|],
                    [ospPathSep|cli-data/chart-requests.json|],
                    [ospPathSep|cli-data/activities.json|],
                    [ospPathSep|cli-garmin/activities.csv|],
                    [ospPathSep|cli-garmin/chart-requests.json|],
                    [ospPathSep|cli-both/activities.json|],
                    [ospPathSep|cli-both/activities.csv|],
                    [ospPathSep|cli-both/chart-requests.json|],
                    [ospPathSep|config-cr.json|],
                    [ospPathSep|config-activities.json|],
                    [ospPathSep|config-data/chart-requests.json|],
                    [ospPathSep|config-data/activities.json|],
                    [ospPathSep|config-data/rel-cr.json|],
                    [ospPathSep|config-data/rel-activities.json|],
                    (toOsPath $ baseRoot <</>> [relfilePathSep|cwd_files/activities.json|]),
                    (toOsPath $ baseRoot <</>> [relfilePathSep|cwd_files/Activities.csv|]),
                    (toOsPath $ baseRoot <</>> [relfilePathSep|cwd_files/chart-requests.json|]),
                    (toOsPath $ baseRoot <</>> [relfilePathSep|cwd_files/activity-labels.json|]),
                    [ospPathSep|xdg/config/pacer/chart-requests.json|],
                    [ospPathSep|xdg/config/pacer/activities.json|]
                  ],
          xdg
        }

    runner =
      runEff
        . evalState env.cachedPaths
        . runReader @MockEnv env
        . runPathReaderMock
        . runLoggerMock
        . runLoggerNS ""
        . runReader logEnv

    logEnv = MkLogEnv Nothing mempty mempty

runLoggerMock :: Eff (Logger : es) a -> Eff es a
runLoggerMock = interpret_ $ \case
  LoggerLog {} -> pure ()

runPathReaderMock ::
  (Reader MockEnv :> es) =>
  Eff (PathReader : es) a ->
  Eff es a
runPathReaderMock = interpret_ $ \case
  CanonicalizePath p -> pure $ rootOsPath </> p
  DoesDirectoryExist p -> do
    knownDirs <- asks @MockEnv (.knownDirectories)
    knownDirsFalse <- asks @MockEnv (.knownDirectoriesFalse)
    if
      | p `Set.member` knownDirs -> pure True
      | p `Set.member` knownDirsFalse -> pure False
      | otherwise -> error $ "doesDirectoryExist: unexpected dir: " ++ show p
  DoesFileExist p -> do
    knownFiles <- asks @MockEnv (.knownFiles)
    pure $ p `Set.member` knownFiles
  GetCurrentDirectory ->
    asks @MockEnv (.cwd) <&> \case
      Nothing -> (toOsPath cwdPath)
      Just cwd -> cwd
  GetXdgDirectory d p ->
    case d of
      XdgConfig -> do
        xdg <- asks @MockEnv (.xdg)
        pure $ case xdg of
          XdgGood -> rootOsPath </> [ospPathSep|xdg/config|] </> p
          XdgEmpty -> rootOsPath </> [ospPathSep|bad_xdg/config|] </> p
          XdgMissing -> rootOsPath </> [ospPathSep|missing_xdg/config|] </> p
      _ -> error $ "runPathReaderMock: unexpected xdg type: " <> show d
  ListDirectory p
    | dirName == [osp|cli-garmin|] ->
        pure [[osp|activities.csv|], [osp|chart-requests.json|]]
    | dirName == [osp|cli-both|] ->
        pure
          [ [osp|activities.csv|],
            [osp|chart-requests.json|],
            [osp|activities.json|]
          ]
    -- xdg
    | xdgNormal ->
        pure
          [ [osp|activities.json|],
            [osp|chart-requests.json|]
          ]
    | dirName == [osp|cli-data|] ->
        pure
          [ [osp|activities.json|],
            [osp|chart-requests.json|]
          ]
    | dirName == [osp|config-data|] ->
        pure
          [ [osp|activities.json|],
            [osp|chart-requests.json|]
          ]
    | dirName == [osp|cwd_files|] ->
        pure
          [ [osp|Activities.csv|],
            [osp|activities.json|],
            [osp|activity-labels.json|],
            [osp|chart-requests.json|]
          ]
    | otherwise -> pure []
    where
      dirsSplit = OsPath.splitDirectories p
      dirName = L.last dirsSplit

      xdgNormal =
        [osp|xdg|]
          `elem` dirsSplit
          && [osp|bad_xdg|]
          `nelem` dirsSplit
          && [osp|missing_xdg|]
          `nelem` dirsSplit

      nelem x = not . elem x
  other -> error $ "runPathReaderMock: unimplemented: " ++ showEffectCons other

goldenRunner :: ChartParamsArgs -> Config -> IO ByteString
goldenRunner = goldenRunnerXdg XdgGood

goldenRunnerCwd :: OsPath -> ChartParamsArgs -> Config -> IO ByteString
goldenRunnerCwd cwd = goldenRunnerGeneral (Just cwd) XdgGood

goldenRunnerXdg :: XdgHandler -> ChartParamsArgs -> Config -> IO ByteString
goldenRunnerXdg = goldenRunnerGeneral Nothing

goldenRunnerGeneral ::
  Maybe OsPath ->
  XdgHandler ->
  ChartParamsArgs ->
  Config ->
  IO ByteString
goldenRunnerGeneral mCwd xdg params config = do
  let configPath =
        MkConfigWithPath
          { dirPath = rootPath <</>> [reldir|config-data|],
            config
          }
  trySync (runEvolvePhase mCwd xdg params (Just configPath)) <&> \case
    Right x -> pShowBS x
    -- displayInner over displayException since we do not want unstable
    -- callstacks in output.
    Left ex -> encodeUtf8 $ packText $ displayInnerMatchKnown ex

rootOsPath :: OsPath
rootOsPath = toOsPath rootPath

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

baseConfig :: Config
baseConfig = set' #chartConfig (Just mempty) mempty

{- ORMOLU_DISABLE -}

absBuildDir :: Path Abs Dir
absBuildDir = baseRoot <</>> [reldirPathSep|abs/build-dir|]

cwdPath :: Path Abs Dir
cwdPath = baseRoot <</>> [reldir|cwd|]

rootPath :: Path Abs Dir
rootPath = baseRoot <</>> [reldir|root|]

baseRoot :: Path Abs Dir
baseRoot =
#if POSIX
  [absdir|/|]
#else
  [absdir|C:\|]
#endif

{- ORMOLU_ENABLE -}
