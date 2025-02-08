{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Params (tests) where

import Data.Set (Set)
import Data.Set qualified as Set
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReader (CanonicalizePath, DoesFileExist, GetXdgDirectory),
  )
import Effectful.Logger.Dynamic (Logger (LoggerLog))
import Pacer.Command.Chart.Params
  ( ChartParams
      ( MkChartParams,
        chartRequestsPath,
        cleanInstall,
        dataDir,
        json,
        runsPath,
        runsType
      ),
    ChartParamsArgs,
    ChartParamsFinal,
  )
import Pacer.Command.Chart.Params qualified as Params
import Pacer.Config.Env.Types (CachedPaths)
import Pacer.Config.Toml
  ( Toml
      ( MkToml,
        chartRequestsPath,
        dataDir,
        logLevel,
        runsPath
      ),
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
    [ testEvolvePhaseCliPaths,
      testEvolvePhaseCliData,
      testEvolvePhaseConfigAbsPaths,
      testEvolvePhaseConfigRelPaths,
      testEvolvePhaseConfigAbsData,
      testEvolvePhaseConfigRelData,
      testEvolvePhaseXdgPaths
    ]

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
      MkChartParams
        { cleanInstall = False,
          chartRequestsPath = Just [osp|cli-cr.toml|],
          dataDir = Just [osp|cli-data|],
          json = False,
          runsPath = Just [osp|cli-runs.toml|],
          runsType = Nothing
        }
    toml =
      MkToml
        { chartRequestsPath = Just [osp|config-cr.toml|],
          dataDir = Just [osp|config-data|],
          logLevel = Nothing,
          runsPath = Just [osp|config-runs.toml|]
        }

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
      MkChartParams
        { cleanInstall = False,
          chartRequestsPath = Nothing,
          dataDir = Just [osp|cli-data|],
          json = False,
          runsPath = Nothing,
          runsType = Nothing
        }

    toml =
      MkToml
        { chartRequestsPath = Just [osp|config-cr.toml|],
          dataDir = Just [osp|config-data|],
          logLevel = Nothing,
          runsPath = Just [osp|config-runs.toml|]
        }

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
      MkChartParams
        { cleanInstall = False,
          chartRequestsPath = Nothing,
          -- Even with --data specified, we will skip to toml since the
          -- former contains no paths.
          dataDir = Just [osp|no-data|],
          json = False,
          runsPath = Nothing,
          runsType = Nothing
        }

    toml =
      MkToml
        { chartRequestsPath = Just $ rootOsPath </> [osp|config-cr.toml|],
          dataDir = Just [osp|config-data|],
          logLevel = Nothing,
          runsPath = Just $ rootOsPath </> [osp|config-runs.toml|]
        }

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
      MkChartParams
        { cleanInstall = False,
          chartRequestsPath = Nothing,
          -- Even with --data specified, we will skip to toml since the
          -- former contains no paths.
          dataDir = Just [osp|no-data|],
          json = False,
          runsPath = Nothing,
          runsType = Nothing
        }

    toml =
      MkToml
        { chartRequestsPath = Just [osp|rel-cr.toml|],
          dataDir = Just [osp|config-data|],
          logLevel = Nothing,
          runsPath = Just [osp|rel-runs.toml|]
        }

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
      MkChartParams
        { cleanInstall = False,
          chartRequestsPath = Nothing,
          -- Even with --data specified, we will skip to toml since the
          -- former contains no paths.
          dataDir = Just [osp|no-data|],
          json = False,
          runsPath = Nothing,
          runsType = Nothing
        }

    toml =
      MkToml
        { chartRequestsPath = Nothing,
          dataDir = Just $ rootOsPath </> [osp|config-data|],
          logLevel = Nothing,
          runsPath = Nothing
        }

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
      MkChartParams
        { cleanInstall = False,
          chartRequestsPath = Nothing,
          -- Even with --data specified, we will skip to toml since the
          -- former contains no paths.
          dataDir = Just [osp|no-data|],
          json = False,
          runsPath = Nothing,
          runsType = Nothing
        }

    toml =
      MkToml
        { chartRequestsPath = Nothing,
          dataDir = Just [osp|./|],
          logLevel = Nothing,
          runsPath = Nothing
        }

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
      MkChartParams
        { cleanInstall = False,
          chartRequestsPath = Nothing,
          -- Even with --data specified, we will skip to toml since the
          -- former contains no paths.
          dataDir = Just [osp|no-data|],
          json = False,
          runsPath = Nothing,
          runsType = Nothing
        }

    toml =
      MkToml
        { chartRequestsPath = Nothing,
          -- Even with config.data specified, we will skip to toml since the
          -- former contains no paths.
          dataDir = Just [osp|no-data|],
          logLevel = Nothing,
          runsPath = Nothing
        }

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
      MkChartParams
        { cleanInstall = False,
          chartRequestsPath = Just [osp|bad_cr.toml|],
          dataDir = Just [osp|cli-data|],
          json = False,
          runsPath = Just [osp|bad_runs.toml|],
          runsType = Nothing
        }
    toml =
      MkToml
        { chartRequestsPath = Just [osp|config-cr.toml|],
          dataDir = Just [osp|config-data|],
          logLevel = Nothing,
          runsPath = Just [osp|config-runs.toml|]
        }

testEvolvePhaseConfigPathsEx :: TestTree
testEvolvePhaseConfigPathsEx =
  testGoldenParamsOs
    $ MkGoldenParams
      { testDesc = "Exception for unknown config paths",
        testName = [osp|testEvolvePhaseConfigPathsEx|],
        runner = goldenRunner params toml
      }
  where
    params =
      MkChartParams
        { cleanInstall = False,
          chartRequestsPath = Nothing,
          dataDir = Nothing,
          json = False,
          runsPath = Nothing,
          runsType = Nothing
        }
    toml =
      MkToml
        { chartRequestsPath = Just [osp|bad-cr.toml|],
          dataDir = Just [osp|config-data|],
          logLevel = Nothing,
          runsPath = Just [osp|bad-runs.toml|]
        }

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
      MkChartParams
        { cleanInstall = False,
          chartRequestsPath = Nothing,
          dataDir = Just [osp|some-dir|],
          json = False,
          runsPath = Nothing,
          runsType = Nothing
        }
    toml =
      MkToml
        { chartRequestsPath = Nothing,
          dataDir = Just [osp|some-config-data|],
          logLevel = Nothing,
          runsPath = Nothing
        }

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

{- ORMOLU_DISABLE -}

rootPath :: Path Abs Dir
rootPath =
#if POSIX
  [absdir|/root|]
#else
  [absdir|C:\root|]
#endif

{- ORMOLU_ENABLE -}
