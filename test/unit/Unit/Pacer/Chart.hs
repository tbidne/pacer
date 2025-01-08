{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Unit.Pacer.Chart (tests) where

import Control.Exception (IOException)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word8)
import Effects.FileSystem.PathReader (MonadPathReader (canonicalizePath))
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter
  ( MonadPathWriter
      ( createDirectory,
        removePathForcibly,
        setCurrentDirectory
      ),
  )
import Effects.Process.Typed (MonadTypedProcess (readProcess), ProcessConfig)
import Effects.System.Terminal (MonadTerminal (putStr))
import Pacer.Chart
  ( ChartParams
      ( MkChartParams,
        cleanInstall,
        dataDir,
        json,
        mChartRequestsPath,
        mRunsPath
      ),
  )
import Pacer.Chart qualified as Chart
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath qualified as OsPath
import Test.Tasty.HUnit (assertEqual)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Chart"
    [ successTests,
      failureTests
    ]

successTests :: TestTree
successTests =
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
        { cleanInstall = False,
          dataDir = Nothing,
          json = False,
          mChartRequestsPath = Nothing,
          mRunsPath = Nothing
        }
    coreEnv =
      MkCoreEnv
        { nodeModulesExists = False,
          npmExists = True
        }

testJson :: TestTree
testJson = testCase "With --json" $ do
  refsEnv <- runCreateCharts coreEnv params
  assertEnvRefs refsEnv [1, 0, 0, 0, 0, 1]
  where
    params =
      MkChartParams
        { cleanInstall = False,
          dataDir = Nothing,
          json = True,
          mChartRequestsPath = Nothing,
          mRunsPath = Nothing
        }
    coreEnv =
      MkCoreEnv
        { nodeModulesExists = False,
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
        { cleanInstall = False,
          dataDir = Nothing,
          json = False,
          mChartRequestsPath = Nothing,
          mRunsPath = Nothing
        }
    coreEnv =
      MkCoreEnv
        { nodeModulesExists = True,
          npmExists = True
        }

testPathNodeModExistsClean :: TestTree
testPathNodeModExistsClean = testCase "With --clean" $ do
  refsEnv <- runCreateCharts coreEnv params
  assertEnvRefs refsEnv [11, 1, 1, 2, 1, 1]
  where
    params =
      MkChartParams
        { cleanInstall = True,
          dataDir = Nothing,
          json = False,
          mChartRequestsPath = Nothing,
          mRunsPath = Nothing
        }
    coreEnv =
      MkCoreEnv
        { nodeModulesExists = True,
          npmExists = True
        }

failureTests :: TestTree
failureTests =
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
        { cleanInstall = False,
          dataDir = Nothing,
          json = False,
          mChartRequestsPath = Nothing,
          mRunsPath = Nothing
        }
    coreEnv =
      MkCoreEnv
        { nodeModulesExists = False,
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
  result <- readIORef ref
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
  { nodeModulesExists :: Bool,
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

newtype MockChartIO a = MkMockChartIO {unMockChartIO :: ReaderT ChartEnv IO a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadReader ChartEnv,
      MonadThrow
    )

instance MonadFileReader MockChartIO where
  readBinaryFile p =
    if fileName `Set.member` knownFiles
      then liftIO $ readBinaryFile p
      else error $ "readBinaryFile: unexpected: " ++ show fileName
    where
      (_dir, fileName) = OsPath.splitFileName p

      knownFiles =
        Set.fromList
          [ [osp|chart-requests.toml|],
            [osp|runs.toml|]
          ]

instance MonadFileWriter MockChartIO where
  writeBinaryFile _ _ = incIORef (.refsEnv.numFileWriterCalls)

instance MonadPathReader MockChartIO where
  -- Real IO since otherwise parsing the absolute path will fail.
  -- Alternatively, we could override MonadThrow to catch the parse
  -- exception, like in the functional tests.
  canonicalizePath = liftIO . PR.makeAbsolute

  doesDirectoryExist p = case dirName of
    [osp|build|] -> pure False
    [osp|dist|] -> pure True
    [osp|pacer|] -> pure True
    [osp|node_modules|] -> asks (.coreEnv.nodeModulesExists)
    [osp|web|] -> pure True
    other -> do
      -- we need the current directory to return true, but it can be
      -- non-deterministic e.g. for out-of-tree builds, hence this check.
      currDir <- liftIO $ PR.getCurrentDirectory
      if p == currDir
        then pure True
        else error $ "doesDirectoryExist: unexpected: " ++ show other
    where
      dirName = L.last $ OsPath.splitDirectories p

  doesFileExist p =
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

  findExecutable [osp|npm|] =
    asks (.coreEnv.npmExists) <&> \case
      True -> Just [osp|npm_exe|]
      False -> Nothing
  findExecutable [osp|npm.cmd|] =
    asks (.coreEnv.npmExists) <&> \case
      True -> Just [osp|npm_exe|]
      False -> Nothing
  findExecutable p = error $ "findExecutable: unexpected: " ++ show p

  getCurrentDirectory = liftIO PR.getCurrentDirectory

  getXdgDirectory XdgCache p =
    incIORef (.refsEnv.numXdgCacheCalls)
      $> ([ospPathSep|test/unit/data/xdg/cache|] </> p)
  getXdgDirectory XdgConfig p =
    incIORef (.refsEnv.numXdgConfigCalls)
      $> ([ospPathSep|test/unit/data/xdg/config|] </> p)
  getXdgDirectory other _ =
    error $ "getXdgDirectory: unexpected: " ++ show other

  listDirectory p = case dirName of
    [osp|dist|] -> pure []
    other -> error $ "listDirectory: unexpected: " ++ show other
    where
      dirName = L.last $ OsPath.splitDirectories p

  pathIsSymbolicLink _ = pure False

instance MonadPathWriter MockChartIO where
  createDirectory _ = pure ()
  createDirectoryIfMissing _ _ = pure ()

  -- Needed when node_modules exists
  removePathForcibly _ = incIORef (.refsEnv.numRemoveDirectoryCalls)
  setCurrentDirectory _ = pure ()

instance MonadTerminal MockChartIO where
  putStr _ = pure ()

instance MonadTypedProcess MockChartIO where
  readProcess pc = case processConfigToCmd pc of
    "Raw command: npm_exe install --save" ->
      incIORef (.refsEnv.numNpmInstallCalls) $> mockResult
    "Raw command: npm_exe run start" ->
      incIORef (.refsEnv.numNpmBuildCalls) $> mockResult
    other -> error $ "readProcess: unexpected: " ++ other
    where
      processConfigToCmd :: ProcessConfig i o e -> String
      processConfigToCmd = unpackText . T.strip . packText . show

      mockResult = (ExitSuccess, "stdout", "stderr")

incIORef :: (ChartEnv -> IORef Word8) -> MockChartIO ()
incIORef toRef = asks toRef >>= \r -> modifyIORef' r (+ 1)

runMockChartIO ::
  CoreEnv ->
  MockChartIO a ->
  IO RefsEnv
runMockChartIO coreEnv m = do
  numFileWriterCalls <- newIORef 0
  numNpmBuildCalls <- newIORef 0
  numNpmInstallCalls <- newIORef 0
  numRemoveDirectoryCalls <- newIORef 0
  numXdgCacheCalls <- newIORef 0
  numXdgConfigCalls <- newIORef 0

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

  _ <- runReaderT (unMockChartIO m) env
  pure refsEnv

runMockChartIOEx ::
  (Exception e) =>
  CoreEnv ->
  MockChartIO a ->
  IO (Tuple2 RefsEnv e)
runMockChartIOEx @e coreEnv m = do
  numFileWriterCalls <- newIORef 0
  numNpmBuildCalls <- newIORef 0
  numNpmInstallCalls <- newIORef 0
  numRemoveDirectoryCalls <- newIORef 0
  numXdgCacheCalls <- newIORef 0
  numXdgConfigCalls <- newIORef 0

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

  eResult <- try @_ @e $ runReaderT (unMockChartIO m) env

  case eResult of
    Left ex -> pure (refsEnv, ex)
    Right _ -> assertFailure $ "Expected exception, received none"

runCreateCharts :: CoreEnv -> ChartParams -> IO RefsEnv
runCreateCharts coreEnv = runMockChartIO coreEnv . Chart.createCharts

runCreateChartsEx :: (Exception e) => CoreEnv -> ChartParams -> IO (RefsEnv, e)
runCreateChartsEx coreEnv = runMockChartIOEx coreEnv . Chart.createCharts

npmStr :: String
npmStr = case currentOs of
  Windows -> "npm.cmd"
  _ -> "npm"
