{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Unit.Pacer.Command.Chart (tests) where

import Control.Exception (IOException)
import Data.IORef qualified as Ref
import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful.FileSystem.FileReader.Dynamic (FileReader (ReadBinaryFile))
import Effectful.FileSystem.FileWriter.Dynamic (FileWriter (WriteBinaryFile))
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReader
      ( CanonicalizePath,
        DoesDirectoryExist,
        DoesFileExist,
        FindExecutable,
        GetCurrentDirectory,
        GetXdgDirectory,
        ListDirectory,
        PathIsSymbolicLink
      ),
  )
import Effectful.FileSystem.PathReader.Static qualified as PRS
import Effectful.FileSystem.PathWriter.Dynamic
  ( PathWriter
      ( CreateDirectory,
        CreateDirectoryIfMissing,
        RemovePathForcibly,
        SetCurrentDirectory
      ),
  )
import Effectful.Logger.Dynamic (Logger (LoggerLog))
import Effectful.Process.Typed.Dynamic
  ( ProcessConfig,
    TypedProcess (ReadProcess),
  )
import Effectful.Terminal.Dynamic (Terminal (PutStr, PutStrLn))
import FileSystem.IO (readBinaryFileIO)
import FileSystem.OsPath (decodeLenient)
import Pacer.Command.Chart qualified as Chart
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
  )
import Pacer.Command.Chart.Params qualified as ChartParams
import Pacer.Config.Env.Types (CachedPaths)
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath qualified as OsPath
import Test.Tasty.HUnit (assertEqual)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart"
    [ createChartTests
    ]

createChartTests :: TestTree
createChartTests =
  testGroup
    "createCharts"
    [ createChartSuccessTests,
      createChartFailureTests
    ]

createChartSuccessTests :: TestTree
createChartSuccessTests =
  testGroup
    "Happy paths"
    [ testDefault,
      testJson,
      testPathNodeModExists,
      testPathNodeModExistsClean
    ]

testDefault :: TestTree
testDefault = testCase "Default" $ do
  refsEnv <- runCreateCharts coreEnv params
  assertEnvRefs refsEnv [1, 1, 1, 0, 1, 1]
  where
    params =
      MkChartParams
        { buildDir = Nothing,
          cleanInstall = False,
          dataDir = Nothing,
          json = False,
          chartRequestsPath = Nothing,
          runsPath = Nothing,
          runsType = Nothing
        }
    coreEnv =
      MkCoreEnv
        { cachedPaths = mempty,
          nodeModulesExists = False,
          npmExists = True
        }

testJson :: TestTree
testJson = testCase "With --json" $ do
  refsEnv <- runCreateCharts coreEnv params
  assertEnvRefs refsEnv [1, 0, 0, 0, 0, 1]
  where
    params =
      MkChartParams
        { buildDir = Nothing,
          cleanInstall = False,
          dataDir = Nothing,
          json = True,
          chartRequestsPath = Nothing,
          runsPath = Nothing,
          runsType = Nothing
        }
    coreEnv =
      MkCoreEnv
        { cachedPaths = mempty,
          nodeModulesExists = False,
          -- npm is False since it doesn't need to exist w/ --json
          npmExists = False
        }

testPathNodeModExists :: TestTree
testPathNodeModExists = testCase "node_modules exists" $ do
  refsEnv <- runCreateCharts coreEnv params
  assertEnvRefs refsEnv [1, 1, 0, 0, 1, 1]
  where
    params =
      MkChartParams
        { buildDir = Nothing,
          cleanInstall = False,
          dataDir = Nothing,
          json = False,
          chartRequestsPath = Nothing,
          runsPath = Nothing,
          runsType = Nothing
        }
    coreEnv =
      MkCoreEnv
        { cachedPaths = mempty,
          nodeModulesExists = True,
          npmExists = True
        }

testPathNodeModExistsClean :: TestTree
testPathNodeModExistsClean = testCase "With --clean" $ do
  refsEnv <- runCreateCharts coreEnv params
  assertEnvRefs refsEnv [11, 1, 1, 2, 1, 1]
  where
    params =
      MkChartParams
        { buildDir = Nothing,
          cleanInstall = True,
          dataDir = Nothing,
          json = False,
          chartRequestsPath = Nothing,
          runsPath = Nothing,
          runsType = Nothing
        }
    coreEnv =
      MkCoreEnv
        { cachedPaths = mempty,
          nodeModulesExists = True,
          npmExists = True
        }

createChartFailureTests :: TestTree
createChartFailureTests =
  testGroup
    "Failures"
    [ testNoNpmFailure
    ]

testNoNpmFailure :: TestTree
testNoNpmFailure = testCase "No npm failure" $ do
  (refsEnv, ex) <- runCreateChartsEx @IOException coreEnv params

  assertEnvRefs refsEnv [0, 0, 0, 0, 0, 1]
  expectedErr @=? displayException ex
  where
    expectedErr =
      mconcat
        [ npmStr,
          ": createCharts: does not exist (Required npm executable not ",
          "found. Please add it to the PATH.)"
        ]
    params =
      MkChartParams
        { buildDir = Nothing,
          cleanInstall = False,
          dataDir = Nothing,
          json = False,
          chartRequestsPath = Nothing,
          runsPath = Nothing,
          runsType = Nothing
        }
    coreEnv =
      MkCoreEnv
        { cachedPaths = mempty,
          nodeModulesExists = False,
          npmExists = False
        }

assertEnvRefs :: RefsEnv -> List Word8 -> IO ()
assertEnvRefs refsEnv expecteds = do
  length expecteds @=? length refs
  assertRefs vals
  where
    vals = zipWith (\(d, r) e -> (d, r, e)) refs expecteds

    refs =
      [ ("numFileWriterCalls", refsEnv.numFileWriterCalls),
        ("numNpmBuildCalls", refsEnv.numNpmBuildCalls),
        ("numNpmInstallCalls", refsEnv.numNpmInstallCalls),
        ("numRemoveDirectoryCalls", refsEnv.numRemoveDirectoryCalls),
        ("numXdgCacheCalls", refsEnv.numXdgCacheCalls),
        ("numXdgConfigCalls", refsEnv.numXdgConfigCalls)
      ]

assertRefs :: (Eq a, Show a) => List (Tuple3 String (IORef a) a) -> IO ()
assertRefs xs = for_ xs $ \(desc, ref, expected) -> do
  result <- Ref.readIORef ref
  let msg =
        mconcat
          [ desc,
            ": ",
            show expected,
            " /= ",
            show result
          ]
  assertEqual msg expected result

data CoreEnv = MkCoreEnv
  { cachedPaths :: CachedPaths,
    nodeModulesExists :: Bool,
    npmExists :: Bool
  }

data RefsEnv = MkRefsEnv
  { numFileWriterCalls :: IORef Word8,
    numNpmBuildCalls :: IORef Word8,
    numNpmInstallCalls :: IORef Word8,
    numRemoveDirectoryCalls :: IORef Word8,
    numXdgCacheCalls :: IORef Word8,
    numXdgConfigCalls :: IORef Word8
  }

data ChartEnv = MkChartEnv
  { coreEnv :: CoreEnv,
    refsEnv :: RefsEnv
  }

runFileReaderMock :: (IOE :> es) => Eff (FileReader : es) a -> Eff es a
runFileReaderMock = interpret_ $ \case
  ReadBinaryFile p ->
    if fileName `Set.member` knownFiles
      then liftIO $ readBinaryFileIO p
      else error $ "readBinaryFile: unexpected: " ++ show fileName
    where
      (_dir, fileName) = OsPath.splitFileName p
      knownFiles =
        Set.fromList
          [ [osp|chart-requests.toml|],
            [osp|runs.toml|]
          ]

runFileWriterMock ::
  ( IORefE :> es,
    Reader ChartEnv :> es
  ) =>
  Eff (FileWriter : es) a ->
  Eff es a
runFileWriterMock = interpret_ $ \case
  WriteBinaryFile _ _ -> incIORef (.refsEnv.numFileWriterCalls)
  _ -> error "runFileWriterMock: unimplemented"

runPathReaderMock ::
  (IOE :> es, IORefE :> es, Reader ChartEnv :> es) =>
  Eff (PathReader : es) a ->
  Eff es a
runPathReaderMock = reinterpret_ PRS.runPathReader $ \case
  -- Real IO since otherwise parsing the absolute path will fail.
  -- Alternatively, we could override MonadThrow to catch the parse
  -- exception, like in the functional tests.
  CanonicalizePath p -> PRS.canonicalizePath p
  DoesDirectoryExist p -> case dirName of
    [osp|build|] -> pure False
    [osp|dist|] -> pure True
    [osp|pacer|] -> pure True
    [osp|node_modules|] -> asks @ChartEnv (.coreEnv.nodeModulesExists)
    [osp|web|] -> pure True
    other -> do
      -- we need the current directory to return true, but it can be
      -- non-deterministic e.g. for out-of-tree builds, hence this check.
      currDir <- PRS.getCurrentDirectory
      -- The LHS might (always?) have a trailing separator since it's no
      -- longer a hardcoded 'build' but possibly the result of e.g.
      -- @cwdPath <</>> [reldir|build|]@. Presumably some part of this adds
      -- the slash.
      if OsPath.dropTrailingPathSeparator p == currDir
        then pure True
        else
          error
            $ mconcat
              [ "doesDirectoryExist: unexpected dir name: '",
                decodeLenient other,
                "'\nfull path: '",
                decodeLenient p,
                "'\ncurrent path: '",
                decodeLenient currDir,
                "'\n"
              ]
    where
      dirName = L.last $ OsPath.splitDirectories p
  DoesFileExist p ->
    if fileName `Set.member` knownFiles
      then pure True
      else error $ "doesFileExist: unexpected: '" ++ show fileName ++ "'"
    where
      (_dir, fileName) = OsPath.splitFileName p

      knownFiles =
        Set.fromList
          [ [osp|build_charts.ts|],
            [osp|chart-requests.toml|],
            [osp|index.html|],
            [osp|index.ts|],
            [osp|package.json|],
            [osp|package-lock.json|],
            [osp|runs.toml|],
            [osp|style.css|],
            [osp|tsconfig.json|],
            [osp|types.ts|],
            [osp|utils.ts|],
            [osp|webpack.config.js|]
          ]
  FindExecutable p -> case p of
    [osp|npm|] ->
      asks @ChartEnv (.coreEnv.npmExists) <&> \case
        True -> Just [osp|npm_exe|]
        False -> Nothing
    [osp|npm.cmd|] ->
      asks @ChartEnv (.coreEnv.npmExists) <&> \case
        True -> Just [osp|npm_exe|]
        False -> Nothing
    _ -> error $ "findExecutable: unexpected: " ++ show p
  GetCurrentDirectory -> PRS.getCurrentDirectory
  GetXdgDirectory xdg p -> case xdg of
    XdgCache ->
      incIORef (.refsEnv.numXdgCacheCalls)
        $> ([ospPathSep|test/unit/data/xdg/cache|] </> p)
    XdgConfig ->
      incIORef (.refsEnv.numXdgConfigCalls)
        $> ([ospPathSep|test/unit/data/xdg/config|] </> p)
    other ->
      error $ "getXdgDirectory: unexpected: " ++ show other
  ListDirectory p -> case dirName of
    [osp|dist|] -> pure []
    other -> error $ "listDirectory: unexpected: " ++ show other
    where
      dirName = L.last $ OsPath.splitDirectories p
  PathIsSymbolicLink _ -> pure False
  _ -> error "runPathReaderMock: unimplemented"

runPathWriterMock ::
  ( IORefE :> es,
    Reader ChartEnv :> es
  ) =>
  Eff (PathWriter : es) a ->
  Eff es a
runPathWriterMock = interpret_ $ \case
  CreateDirectory _ -> pure ()
  CreateDirectoryIfMissing _ _ -> pure ()
  -- Needed when node_modules exists
  RemovePathForcibly _ -> incIORef (.refsEnv.numRemoveDirectoryCalls)
  SetCurrentDirectory _ -> pure ()
  _ -> error "runPathWriterMock: unimplemented"

runLoggerMock :: Eff (Logger : es) a -> Eff es a
runLoggerMock = interpret_ $ \case
  LoggerLog _ _ _ _ -> pure ()

runTerminalMock :: Eff (Terminal : es) a -> Eff es a
runTerminalMock = interpret_ $ \case
  PutStr _ -> pure ()
  PutStrLn _ -> pure ()
  _ -> error "runTerminalMock: unimplemented"

runTypedProcessMock ::
  ( IORefE :> es,
    Reader ChartEnv :> es
  ) =>
  Eff (TypedProcess : es) a ->
  Eff es a
runTypedProcessMock = interpret_ $ \case
  ReadProcess pc -> case processConfigToCmd pc of
    "Raw command: npm_exe install --save" ->
      incIORef (.refsEnv.numNpmInstallCalls) $> mockResult
    "Raw command: npm_exe run start" ->
      incIORef (.refsEnv.numNpmBuildCalls) $> mockResult
    other -> error $ "readProcess: unexpected: " ++ other
  _ -> error "runTypedProcessMock: unimplemented"
  where
    processConfigToCmd :: ProcessConfig i o e -> String
    processConfigToCmd = unpackText . T.strip . packText . show

    mockResult :: (ExitCode, LazyByteString, LazyByteString)
    mockResult = (ExitSuccess, "stdout", "stderr")

incIORef ::
  ( IORefE :> es,
    Reader ChartEnv :> es
  ) =>
  (ChartEnv -> IORef Word8) ->
  Eff es ()
incIORef toRef = asks toRef >>= \r -> modifyIORef' r (+ 1)

type TestEffects =
  [ FileReader,
    FileWriter,
    PathReader,
    PathWriter,
    Terminal,
    TypedProcess,
    Logger,
    LoggerNS,
    IORefE,
    Reader ChartEnv,
    State CachedPaths,
    IOE
  ]

runTestEff :: ChartEnv -> Eff TestEffects a -> IO a
runTestEff env m =
  runEff
    . evalState env.coreEnv.cachedPaths
    . runReader env
    . runIORef
    . runLoggerNS mempty
    . runLoggerMock
    . runTypedProcessMock
    . runTerminalMock
    . runPathWriterMock
    . runPathReaderMock
    . runFileWriterMock
    . runFileReaderMock
    $ m

runMockChartIO ::
  CoreEnv ->
  Eff TestEffects a ->
  IO RefsEnv
runMockChartIO coreEnv m = do
  numFileWriterCalls <- Ref.newIORef 0
  numNpmBuildCalls <- Ref.newIORef 0
  numNpmInstallCalls <- Ref.newIORef 0
  numRemoveDirectoryCalls <- Ref.newIORef 0
  numXdgCacheCalls <- Ref.newIORef 0
  numXdgConfigCalls <- Ref.newIORef 0

  let refsEnv =
        MkRefsEnv
          { numFileWriterCalls,
            numNpmBuildCalls,
            numNpmInstallCalls,
            numRemoveDirectoryCalls,
            numXdgCacheCalls,
            numXdgConfigCalls
          }
      env =
        MkChartEnv
          { coreEnv,
            refsEnv
          }

  _ <- runTestEff env m
  pure refsEnv

runMockChartIOEx ::
  (Exception e) =>
  CoreEnv ->
  Eff TestEffects a ->
  IO (Tuple2 RefsEnv e)
runMockChartIOEx @e coreEnv m = do
  numFileWriterCalls <- Ref.newIORef 0
  numNpmBuildCalls <- Ref.newIORef 0
  numNpmInstallCalls <- Ref.newIORef 0
  numRemoveDirectoryCalls <- Ref.newIORef 0
  numXdgCacheCalls <- Ref.newIORef 0
  numXdgConfigCalls <- Ref.newIORef 0

  let refsEnv =
        MkRefsEnv
          { numFileWriterCalls,
            numNpmBuildCalls,
            numNpmInstallCalls,
            numRemoveDirectoryCalls,
            numXdgCacheCalls,
            numXdgConfigCalls
          }
      env =
        MkChartEnv
          { coreEnv,
            refsEnv
          }

  eResult <- try @_ @e $ runTestEff env m
  case eResult of
    Left ex -> pure (refsEnv, ex)
    Right _ -> assertFailure $ "Expected exception, received none"

runCreateCharts :: CoreEnv -> ChartParamsArgs -> IO RefsEnv
runCreateCharts coreEnv params = runMockChartIO coreEnv $ do
  params' <- ChartParams.evolvePhase params Nothing
  Chart.createCharts $ params'

runCreateChartsEx :: (Exception e) => CoreEnv -> ChartParamsArgs -> IO (RefsEnv, e)
runCreateChartsEx coreEnv params = runMockChartIOEx coreEnv $ do
  params' <- ChartParams.evolvePhase params Nothing
  Chart.createCharts $ params'

npmStr :: String
npmStr = case currentOs of
  Windows -> "npm.cmd"
  _ -> "npm"
