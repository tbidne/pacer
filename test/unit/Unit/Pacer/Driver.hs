{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Driver (tests) where

import Effectful.Environment qualified as Env
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReader
      ( CanonicalizePath,
        DoesDirectoryExist,
        DoesFileExist,
        GetXdgDirectory,
        ListDirectory
      ),
  )
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic (LogLevel (LevelInfo))
import Pacer.Command (Command (Chart))
import Pacer.Command.Chart.Params
  ( ChartParams
      ( MkChartParams,
        activityLabelsPath,
        activityPaths,
        buildDir,
        chartRequestsPath,
        cleanInstall,
        dataDir,
        json,
        port
      ),
  )
import Pacer.Configuration.Env.Types
  ( CachedPaths (MkCachedPaths, currentDirectory, xdgConfigPath),
    LogEnv (MkLogEnv, logLevel, logNamespace, logVerbosity),
  )
import Pacer.Driver (Env)
import Pacer.Driver qualified as Driver
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Unit.Pacer.Driver"
    [ testNoXdgIsFine
    ]

testNoXdgIsFine :: TestTree
testNoXdgIsFine = testCase desc $ do
  -- The primary intention of this test is to ensure runGetEnv does not
  -- explode when the xdg directory does not exist, which occurred due to
  -- a bug. That said, we might as well assert the result.
  (cmd, mCfg, cachedPaths, logEnv) <- runGetEnv args
  expectedCmd @=? cmd
  Nothing @=? mCfg
  expectedCachedPaths @=? cachedPaths
  expectedLogEnv @=? logEnv
  where
    desc = "Chart command without XDG directory succeeds"
    args = ["chart"]

    expectedCmd =
      Chart
        ( MkChartParams
            { activityLabelsPath = Nothing,
              activityPaths = [],
              buildDir = Nothing,
              cleanInstall = False,
              chartRequestsPath = Nothing,
              dataDir = Nothing,
              json = False,
              port = Nothing
            }
        )

    expectedCachedPaths =
      MkCachedPaths
        { currentDirectory = Nothing,
          xdgConfigPath = Just (root <</>> xdgConfig <</>> [reldir|pacer|])
        }

    expectedLogEnv =
      MkLogEnv
        { logLevel = Just LevelInfo,
          logNamespace = mempty,
          logVerbosity = mempty
        }

runGetEnv :: List String -> IO Env
runGetEnv args = run $ Env.withArgs args $ Driver.getEnv
  where
    run =
      runEff
        . runConcurrent
        . Env.runEnvironment
        . runFileReader
        . runPathReaderMock
        . runOptparse
        . runTerminalMock
        . runTime

runPathReaderMock ::
  (IOE :> es) =>
  Eff (PathReader : es) a ->
  Eff es a
runPathReaderMock = reinterpret_ runPathReader $ \case
  CanonicalizePath p -> pure p
  DoesDirectoryExist p
    | p == rootOsPath </> xdgConfigOsPath </> p -> pure False
    | otherwise -> error $ "Unexpected dir: " ++ show p
  DoesFileExist p -> PR.doesFileExist p
  GetXdgDirectory XdgConfig p -> pure $ rootOsPath </> xdgConfigOsPath </> p
  ListDirectory p -> PR.listDirectory p
  other -> error $ "runPathReaderMock: unimplemented: " ++ (showEffectCons other)

runTerminalMock :: Eff (Terminal : es) a -> Eff es a
runTerminalMock = interpret_ $ \case
  other -> error $ "runTerminalMock: unimplemented: " ++ (showEffectCons other)

xdgConfigOsPath :: OsPath
xdgConfigOsPath = toOsPath xdgConfig

xdgConfig :: Path Rel Dir
xdgConfig = [reldirPathSep|xdg/config|]

rootOsPath :: OsPath
rootOsPath = toOsPath root

{- ORMOLU_DISABLE -}

root :: Path Abs Dir
root =
#if POSIX
  [absdir|/some_root|]
#else
  [absdir|C:\some_root|]
#endif

{- ORMOLU_ENABLE -}
