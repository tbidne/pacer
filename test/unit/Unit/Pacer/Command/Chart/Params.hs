{-# LANGUAGE CPP #-}
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
        buildDir,
        chartRequestsPath,
        cleanInstall,
        dataDir,
        json,
        runPaths
      ),
    ChartParamsArgs,
    ChartParamsFinal,
  )
import Pacer.Command.Chart.Params qualified as Params
import Pacer.Configuration.Config
  ( Config,
    ConfigWithPath (MkConfigWithPath, config, dirPath),
  )
import Pacer.Configuration.Env.Types (CachedPaths)
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
      testEvolvePhaseXdgPaths,
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
          #runPaths
          [[osp|cli-runs.json|]]
          baseChartParams

    config =
      set'
        (#chartConfig %? #runPaths)
        [[osp|config-runs.json|]]
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
        (#chartConfig %? #runPaths)
        [[osp|config-runs.json|]]
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
        (#chartConfig %? #runPaths)
        [rootOsPath </> [osp|config-runs.json|]]
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
        (#chartConfig %? #runPaths)
        [[osp|rel-runs.json|]]
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
      { testDesc = "Uses Json and Garmin when both runs types exist",
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
          #runPaths
          [[osp|bad_runs.json|]]
          baseChartParams
    config =
      set'
        (#chartConfig %? #runPaths)
        [[osp|rel-runs.json|]]
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|rel-cr.json|])
          baseConfig

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
        (#chartConfig %? #runPaths)
        [[osp|bad-runs.json|]]
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
        runner = goldenRunnerXdg False params config
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

data MockEnv = MkMockEnv
  { cachedPaths :: CachedPaths,
    knownDirectories :: Set OsPath,
    knownFiles :: Set OsPath,
    -- Returns xdg dir w/ expected files iff xdg is true
    xdg :: Bool
  }
  deriving stock (Eq, Show)

runEvolvePhase ::
  Bool ->
  ChartParamsArgs ->
  Maybe ConfigWithPath ->
  IO ChartParamsFinal
runEvolvePhase xdg params mConfig = do
  runner $ Params.evolvePhase params mConfig
  where
    env =
      MkMockEnv
        { cachedPaths = mempty,
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
                    [ospPathSep|no-data/|],
                    [ospPathSep|some-dir/|],
                    [ospPathSep|xdg/config/pacer/|]
                  ],
          knownFiles =
            Set.fromList
              $ (rootOsPath </>)
              <$> [ [ospPathSep|cli-cr.json|],
                    [ospPathSep|cli-runs.json|],
                    [ospPathSep|cli-data/chart-requests.json|],
                    [ospPathSep|cli-data/runs.json|],
                    [ospPathSep|cli-garmin/activities.csv|],
                    [ospPathSep|cli-garmin/chart-requests.json|],
                    [ospPathSep|cli-both/runs.json|],
                    [ospPathSep|cli-both/activities.csv|],
                    [ospPathSep|cli-both/chart-requests.json|],
                    [ospPathSep|config-cr.json|],
                    [ospPathSep|config-runs.json|],
                    [ospPathSep|config-data/chart-requests.json|],
                    [ospPathSep|config-data/runs.json|],
                    [ospPathSep|config-data/rel-cr.json|],
                    [ospPathSep|config-data/rel-runs.json|],
                    [ospPathSep|xdg/config/pacer/chart-requests.json|],
                    [ospPathSep|xdg/config/pacer/runs.json|]
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
    if p `Set.member` knownDirs
      then pure True
      else error $ "doesDirectoryExist: unknown dir: " ++ show p
  DoesFileExist p -> do
    knownFiles <- asks @MockEnv (.knownFiles)
    pure $ p `Set.member` knownFiles
  GetCurrentDirectory -> pure (toOsPath cwdPath)
  GetXdgDirectory d p ->
    case d of
      XdgConfig -> do
        xdg <- asks @MockEnv (.xdg)
        pure
          $ if xdg
            then rootOsPath </> [ospPathSep|xdg/config|] </> p
            else rootOsPath </> [ospPathSep|bad_xdg/config|] </> p
      _ -> error $ "runPathReaderMock: unexpected xdg type: " <> show d
  ListDirectory p
    | dirName == [osp|cli-garmin|] ->
        pure [[osp|activities.csv|], [osp|chart-requests.json|]]
    | dirName == [osp|cli-both|] ->
        pure
          [ [osp|activities.csv|],
            [osp|chart-requests.json|],
            [osp|runs.json|]
          ]
    | otherwise -> pure []
    where
      dirName = L.last $ OsPath.splitDirectories p
  other -> error $ "runPathReaderMock: unimplemented: " ++ (showEffectCons other)

goldenRunner :: ChartParamsArgs -> Config -> IO ByteString
goldenRunner = goldenRunnerXdg True

goldenRunnerXdg :: Bool -> ChartParamsArgs -> Config -> IO ByteString
goldenRunnerXdg xdg params config = do
  let configPath =
        MkConfigWithPath
          { dirPath = rootPath <</>> [reldir|config-data|],
            config
          }
  trySync (runEvolvePhase xdg params (Just configPath)) <&> \case
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
      runLabelsPath = Nothing,
      runPaths = []
    }

baseConfig :: Config
baseConfig = set' #chartConfig (Just mempty) mempty

{- ORMOLU_DISABLE -}

absBuildDir :: Path Abs Dir
absBuildDir =
#if POSIX
  [absdir|/abs/build-dir|]
#else
  [absdir|C:\abs\build-dir|]
#endif

cwdPath :: Path Abs Dir
cwdPath =
#if POSIX
  [absdir|/cwd|]
#else
  [absdir|C:\cwd|]
#endif

rootPath :: Path Abs Dir
rootPath =
#if POSIX
  [absdir|/root|]
#else
  [absdir|C:\root|]
#endif

{- ORMOLU_ENABLE -}
