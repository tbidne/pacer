{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Unit.Pacer.Driver (tests) where

import Effectful.Environment qualified as Env
import Effectful.FileSystem.FileReader.Dynamic (FileReader (ReadBinaryFile))
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
import Pacer.Configuration.Config
import Pacer.Configuration.Env.Types
  ( CachedPaths (MkCachedPaths, currentDirectory, xdgConfigPath),
    LogEnv (MkLogEnv, logLevel, logNamespace, logVerbosity),
  )
import Pacer.Configuration.Phase
import Pacer.Driver (Env)
import Pacer.Driver qualified as Driver
import Unit.Prelude

newtype TestEnv = MkTestEnv {cwd :: Maybe OsPath}

makeFieldLabelsNoPrefix ''TestEnv

tests :: TestTree
tests =
  testGroup
    "Unit.Pacer.Driver"
    [ testNoXdgIsFine,
      testFindsConfigCurrDir
    ]

testNoXdgIsFine :: TestTree
testNoXdgIsFine = testCase desc $ do
  -- The primary intention of this test is to ensure runGetEnv does not
  -- explode when the xdg directory does not exist, which occurred due to
  -- a bug. That said, we might as well assert the result.
  (cmd, mCfg, cachedPaths, logEnv) <- runGetEnv mempty args
  baseChart @=? cmd
  Nothing @=? mCfg
  expectedCachedPaths @=? cachedPaths
  baseLogEnv @=? logEnv
  where
    desc = "Chart command without XDG directory succeeds"
    args = ["chart"]

    expectedCachedPaths =
      MkCachedPaths
        { currentDirectory = Just (root <</>> [reldir|cwd|]),
          xdgConfigPath = Just (root <</>> xdgConfig <</>> [reldir|pacer|])
        }

testFindsConfigCurrDir :: TestTree
testFindsConfigCurrDir = testCase desc $ do
  (cmd, mCfg, cachedPaths, logEnv) <- runGetEnv testEnv args
  baseChart @=? cmd
  Just expectedConfig @=? mCfg
  expectedCachedPaths @=? cachedPaths
  baseLogEnv @=? logEnv
  where
    desc = "Finds config in current directory"
    args = ["chart"]

    testEnv =
      mempty {cwd = Just $ rootOsPath </> [osp|cwd_config|]}

    expectedConfig =
      MkConfigWithPath
        { dirPath = root <</>> [reldir|cwd_config/|],
          config =
            MkConfig
              { chartConfig = Nothing,
                logConfig = Nothing
              }
        }

    expectedCachedPaths =
      MkCachedPaths
        { currentDirectory = Just (root <</>> [reldir|cwd_config|]),
          xdgConfigPath = Nothing
        }

instance Semigroup TestEnv where
  l <> r = MkTestEnv (l ^. #cwd <|> r ^. #cwd)

instance Monoid TestEnv where
  mempty = MkTestEnv mempty

runGetEnv :: TestEnv -> List String -> IO Env
runGetEnv testEnv args = run $ Env.withArgs args Driver.getEnv
  where
    run =
      runEff
        . runConcurrent
        . Env.runEnvironment
        . runReader testEnv
        . runFileReaderMock
        . runPathReaderMock
        . runOptparse
        . runTerminalMock
        . runTime

runFileReaderMock ::
  Eff (FileReader : es) a ->
  Eff es a
runFileReaderMock = interpret_ $ \case
  ReadBinaryFile p ->
    if p == rootOsPath </> [ospPathSep|cwd_config/config.json|]
      then pure "{}"
      else error $ "Unexpected read file: " ++ show p

runPathReaderMock ::
  (IOE :> es, Reader TestEnv :> es) =>
  Eff (PathReader : es) a ->
  Eff es a
runPathReaderMock = reinterpret_ runPathReader $ \case
  CanonicalizePath p -> pure p
  DoesDirectoryExist p ->
    -- NOTE: Probably only want literals in here, because otherwise we can
    -- easily end up with something like @/root </> /a/path@ which returns
    -- the 2nd i.e. almost certainly an error!
    if
      | p == rootOsPath </> [ospPathSep|xdg/config/pacer/|] -> pure False
      | p == rootOsPath </> [ospPathSep|cwd/|] -> pure True
      | p == rootOsPath </> [ospPathSep|cwd_config/|] -> pure True
      | otherwise -> error $ "Unexpected dir: " ++ show p
  DoesFileExist p ->
    if
      | p == rootOsPath </> [ospPathSep|cwd/config.json|] -> pure False
      | p == rootOsPath </> [ospPathSep|cwd/config.jsonc|] -> pure False
      | p == rootOsPath </> [ospPathSep|cwd_config/config.json|] -> pure True
      -- TODO: Investigate. The fact that this jsonc line is needed for the
      -- tests to pass likely indicates a (minor) bug...if we find the json first, we
      -- shouldn't even check jsonc, since we're using Maybe, right?
      --
      -- See if this is still needed after we refactor the alias search.
      | p == rootOsPath </> [ospPathSep|cwd_config/config.jsonc|] -> pure False
      | otherwise -> error $ "Unexpected file: " ++ show p
  GetCurrentDirectory ->
    asks @TestEnv (view #cwd) <&> \case
      Nothing -> rootOsPath </> [osp|cwd|]
      Just p -> p
  GetXdgDirectory XdgConfig p -> pure $ rootOsPath </> xdgConfigOsPath </> p
  ListDirectory p
    | p == rootOsPath </> [ospPathSep|cwd/|] -> pure []
    | p == rootOsPath </> [ospPathSep|cwd_config/|] ->
        pure [[ospPathSep|config.json|]]
    | otherwise -> PR.listDirectory p
  other -> error $ "runPathReaderMock: unimplemented: " ++ showEffectCons other

runTerminalMock :: Eff (Terminal : es) a -> Eff es a
runTerminalMock = interpret_ $ \case
  other -> error $ "runTerminalMock: unimplemented: " ++ showEffectCons other

baseChart :: Command ConfigPhaseArgs Double
baseChart = Chart baseChartParams

baseChartParams :: ChartParams ConfigPhaseArgs
baseChartParams =
  MkChartParams
    { activityLabelsPath = Nothing,
      activityPaths = [],
      buildDir = Nothing,
      cleanInstall = False,
      chartRequestsPath = Nothing,
      dataDir = Nothing,
      json = False,
      port = Nothing
    }

baseLogEnv :: LogEnv
baseLogEnv =
  MkLogEnv
    { logLevel = Just LevelInfo,
      logNamespace = mempty,
      logVerbosity = mempty
    }

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
