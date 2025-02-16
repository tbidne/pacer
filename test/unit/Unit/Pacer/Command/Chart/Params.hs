{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Params (tests) where

import Data.Set (Set)
import Data.Set qualified as Set
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReader
      ( CanonicalizePath,
        DoesFileExist,
        GetCurrentDirectory,
        GetXdgDirectory
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
        runsPath,
        runsType
      ),
    ChartParamsArgs,
    ChartParamsFinal,
    RunsType (RunsDefault, RunsGarmin),
  )
import Pacer.Command.Chart.Params qualified as Params
import Pacer.Config.Env.Types (CachedPaths)
import Pacer.Config.Toml
  ( Toml,
    TomlWithPath (MkTomlWithPath, dirPath, toml),
  )
import Pacer.Exception qualified as Ex
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
      testEvolvePhaseBothToml,
      testEvolvePhaseBothTomlRunsType,
      testEvolvePhaseBothGarmin
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
        runner = goldenRunner params toml
      }
  where
    params =
      set'
        #buildDir
        (Just (pathToOsPath absBuildDir))
        baseChartParams
    toml = baseToml

testEvolvePhaseCliBuildDirRel :: TestTree
testEvolvePhaseCliBuildDirRel =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses CLI relative build-dir",
        testName = [osp|testEvolvePhaseCliBuildDirRel|],
        runner = goldenRunner params toml
      }
  where
    params =
      set'
        #buildDir
        (Just [osp|build-dir|])
        baseChartParams
    toml = baseToml

testEvolvePhaseConfigBuildDirAbs :: TestTree
testEvolvePhaseConfigBuildDirAbs =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config absolute build-dir",
        testName = [osp|testEvolvePhaseConfigBuildDirAbs|],
        runner = goldenRunner params toml
      }
  where
    params = baseChartParams
    toml =
      set'
        (#chartConfig %? #buildDir)
        (Just (pathToOsPath absBuildDir))
        baseToml

testEvolvePhaseConfigBuildDirRel :: TestTree
testEvolvePhaseConfigBuildDirRel =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config relative build-dir",
        testName = [osp|testEvolvePhaseConfigBuildDirRel|],
        runner = goldenRunner params toml
      }
  where
    params = baseChartParams
    toml =
      set'
        (#chartConfig %? #buildDir)
        (Just [osp|build-dir|])
        baseToml

testEvolvePhaseCliPaths :: TestTree
testEvolvePhaseCliPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses CLI paths",
        testName = [osp|testEvolvePhaseCliPaths|],
        runner = goldenRunner params toml
      }
  where
    params =
      set' #chartRequestsPath (Just [osp|cli-cr.toml|])
        $ set'
          #dataDir
          (Just [osp|cli-data|])
        $ set'
          #runsPath
          (Just [osp|cli-runs.toml|])
          baseChartParams

    toml =
      set'
        (#chartConfig %? #runsPath)
        (Just [osp|config-runs.toml|])
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|config-cr.toml|])
          baseToml

testEvolvePhaseCliData :: TestTree
testEvolvePhaseCliData =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses CLI data",
        testName = [osp|testEvolvePhaseCliData|],
        runner = goldenRunner params toml
      }
  where
    params =
      set'
        #dataDir
        (Just [osp|cli-data|])
        baseChartParams

    toml =
      set'
        (#chartConfig %? #runsPath)
        (Just [osp|config-runs.toml|])
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|config-cr.toml|])
          baseToml

testEvolvePhaseConfigAbsPaths :: TestTree
testEvolvePhaseConfigAbsPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config absolute paths",
        testName = [osp|testEvolvePhaseConfigAbsPaths|],
        runner = goldenRunner params toml
      }
  where
    params =
      -- Even with --data specified, we will skip to toml since the
      -- former contains no paths.
      set'
        #dataDir
        (Just [osp|no-data|])
        baseChartParams

    toml =
      set'
        (#chartConfig %? #runsPath)
        (Just $ rootOsPath </> [osp|config-runs.toml|])
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just $ rootOsPath </> [osp|config-cr.toml|])
          baseToml

testEvolvePhaseConfigRelPaths :: TestTree
testEvolvePhaseConfigRelPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config relative paths",
        testName = [osp|testEvolvePhaseConfigRelPaths|],
        runner = goldenRunner params toml
      }
  where
    params =
      -- Even with --data specified, we will skip to toml since the
      -- former contains no paths.
      set'
        #dataDir
        (Just [osp|no-data|])
        baseChartParams

    toml =
      set'
        (#chartConfig %? #runsPath)
        (Just [osp|rel-runs.toml|])
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|rel-cr.toml|])
          baseToml

testEvolvePhaseConfigAbsData :: TestTree
testEvolvePhaseConfigAbsData =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config absolute data",
        testName = [osp|testEvolvePhaseConfigAbsData|],
        runner = goldenRunner params toml
      }
  where
    params =
      -- Even with --data specified, we will skip to toml since the
      -- former contains no paths.
      set'
        #dataDir
        (Just [osp|no-data|])
        baseChartParams

    toml =
      set'
        (#chartConfig %? #dataDir)
        (Just $ rootOsPath </> [osp|config-data|])
        baseToml

testEvolvePhaseConfigRelData :: TestTree
testEvolvePhaseConfigRelData =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses config relative data",
        testName = [osp|testEvolvePhaseConfigRelData|],
        runner = goldenRunner params toml
      }
  where
    params =
      -- Even with --data specified, we will skip to toml since the
      -- former contains no paths.
      set'
        #dataDir
        (Just [osp|no-data|])
        baseChartParams

    toml =
      set'
        (#chartConfig %? #dataDir)
        (Just [osp|./|])
        baseToml

testEvolvePhaseXdgPaths :: TestTree
testEvolvePhaseXdgPaths =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses xdg paths",
        testName = [osp|testEvolvePhaseXdgPaths|],
        runner = goldenRunner params toml
      }
  where
    params =
      -- Even with --data specified, we will skip to toml since the
      -- former contains no paths.
      set'
        #dataDir
        (Just [osp|no-data|])
        baseChartParams

    toml =
      -- Even with config.data specified, we will skip to toml since the
      -- former contains no paths.
      set'
        (#chartConfig %? #dataDir)
        (Just [osp|no-data|])
        baseToml

testEvolvePhaseGarmin :: TestTree
testEvolvePhaseGarmin =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses garmin path",
        testName = [osp|testEvolvePhaseCliGarmin|],
        runner = goldenRunner params toml
      }
  where
    params =
      set'
        #dataDir
        (Just [osp|cli-garmin|])
        baseChartParams
    toml = baseToml

testEvolvePhaseBothToml :: TestTree
testEvolvePhaseBothToml =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses TOML when both runs types exist",
        testName = [osp|testEvolvePhaseBothToml|],
        runner = goldenRunner params toml
      }
  where
    params =
      set'
        #dataDir
        (Just [osp|cli-both|])
        baseChartParams
    toml = baseToml

testEvolvePhaseBothTomlRunsType :: TestTree
testEvolvePhaseBothTomlRunsType =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses TOML when both runs types exist and runs-type default",
        testName = [osp|testEvolvePhaseBothTomlRunsType|],
        runner = goldenRunner params toml
      }
  where
    params =
      set' #runsType (Just RunsDefault)
        $ set'
          #dataDir
          (Just [osp|cli-both|])
          baseChartParams
    toml = baseToml

testEvolvePhaseBothGarmin :: TestTree
testEvolvePhaseBothGarmin =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Uses garmin when both runs types exist and runs-type garmin",
        testName = [osp|testEvolvePhaseBothGarmin|],
        runner = goldenRunner params toml
      }
  where
    params =
      set' #runsType (Just RunsGarmin)
        $ set'
          #dataDir
          (Just [osp|cli-both|])
          baseChartParams
    toml = baseToml

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
        runner = goldenRunner params toml
      }
  where
    params =
      set' #chartRequestsPath (Just [osp|bad_cr.toml|])
        $ set'
          #dataDir
          (Just [osp|cli-data|])
        $ set'
          #runsPath
          (Just [osp|bad_runs.toml|])
          baseChartParams
    toml =
      set'
        (#chartConfig %? #runsPath)
        (Just [osp|rel-runs.toml|])
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|rel-cr.toml|])
          baseToml

testEvolvePhaseConfigPathsEx :: TestTree
testEvolvePhaseConfigPathsEx =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Exception for unknown config paths",
        testName = [osp|testEvolvePhaseConfigPathsEx|],
        runner = goldenRunner params toml
      }
  where
    params = baseChartParams
    toml =
      set'
        (#chartConfig %? #runsPath)
        (Just [osp|bad-runs.toml|])
        $ set'
          (#chartConfig %? #dataDir)
          (Just [osp|config-data|])
        $ set'
          (#chartConfig %? #chartRequestsPath)
          (Just [osp|bad-cr.toml|])
          baseToml

testEvolvePhaseMissingEx :: TestTree
testEvolvePhaseMissingEx =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Exception for missing paths",
        testName = [osp|testEvolvePhaseMissingEx|],
        runner = goldenRunnerXdg False params toml
      }
  where
    params =
      set'
        #dataDir
        (Just [osp|some-dir|])
        baseChartParams
    toml =
      set'
        (#chartConfig %? #dataDir)
        (Just [osp|some-config-data|])
        baseToml

data MockEnv = MkMockEnv
  { cachedPaths :: CachedPaths,
    knownFiles :: Set OsPath,
    -- Returns xdg dir w/ expected files iff xdg is true
    xdg :: Bool
  }
  deriving stock (Eq, Show)

runEvolvePhase ::
  Bool ->
  ChartParamsArgs ->
  Maybe TomlWithPath ->
  IO ChartParamsFinal
runEvolvePhase xdg params mToml = do
  runner $ Params.evolvePhase params mToml
  where
    env =
      MkMockEnv
        { cachedPaths = mempty,
          knownFiles =
            Set.fromList
              $ (rootOsPath </>)
              <$> [ [ospPathSep|cli-cr.toml|],
                    [ospPathSep|cli-runs.toml|],
                    [ospPathSep|cli-data/chart-requests.toml|],
                    [ospPathSep|cli-data/runs.toml|],
                    [ospPathSep|cli-garmin/activities.csv|],
                    [ospPathSep|cli-garmin/chart-requests.toml|],
                    [ospPathSep|cli-both/runs.toml|],
                    [ospPathSep|cli-both/activities.csv|],
                    [ospPathSep|cli-both/chart-requests.toml|],
                    [ospPathSep|config-cr.toml|],
                    [ospPathSep|config-runs.toml|],
                    [ospPathSep|config-data/chart-requests.toml|],
                    [ospPathSep|config-data/runs.toml|],
                    [ospPathSep|config-data/rel-cr.toml|],
                    [ospPathSep|config-data/rel-runs.toml|],
                    [ospPathSep|xdg/config/pacer/chart-requests.toml|],
                    [ospPathSep|xdg/config/pacer/runs.toml|]
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
  DoesFileExist p -> do
    knownFiles <- asks @MockEnv (.knownFiles)
    pure $ p `Set.member` knownFiles
  GetCurrentDirectory -> pure (pathToOsPath cwdPath)
  GetXdgDirectory d p ->
    case d of
      XdgConfig -> do
        xdg <- asks @MockEnv (.xdg)
        pure
          $ if xdg
            then rootOsPath </> [ospPathSep|xdg/config|] </> p
            else rootOsPath </> [ospPathSep|bad_xdg/config|] </> p
      _ -> error $ "runPathReaderMock: unexpected xdg type: " <> show d
  _ -> error "runPathReaderMock: unimplemented"

goldenRunner :: ChartParamsArgs -> Toml -> IO ByteString
goldenRunner = goldenRunnerXdg True

goldenRunnerXdg :: Bool -> ChartParamsArgs -> Toml -> IO ByteString
goldenRunnerXdg xdg params toml = do
  let tomlPath =
        MkTomlWithPath
          { dirPath = rootPath <</>> [reldir|config-data|],
            toml
          }
  trySync (runEvolvePhase xdg params (Just tomlPath)) <&> \case
    Right x -> pShowBS x
    -- displayInner over displayException since we do not want unstable
    -- callstacks in output.
    Left ex -> encodeUtf8 $ packText $ Ex.displayInnerMatchKnown ex

rootOsPath :: OsPath
rootOsPath = pathToOsPath rootPath

baseChartParams :: ChartParamsArgs
baseChartParams =
  MkChartParams
    { buildDir = Nothing,
      cleanInstall = False,
      chartRequestsPath = Nothing,
      dataDir = Nothing,
      json = False,
      runLabelsPath = Nothing,
      runsPath = Nothing,
      runsType = Nothing
    }

baseToml :: Toml
baseToml = set' #chartConfig (Just mempty) mempty

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
