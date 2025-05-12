{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Unit.Pacer.Command.Chart (tests) where

import Control.Exception (IOException)
import Data.IORef qualified as Ref
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
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
import FileSystem.Path qualified as Path
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart (ChartPaths)
import Pacer.Command.Chart qualified as Chart
import Pacer.Command.Chart.Data.Chart (Chart (chartData))
import Pacer.Command.Chart.Data.Run
  ( Run
      ( MkRun,
        datetime,
        distance,
        duration,
        labels,
        title
      ),
    SomeRuns (MkSomeRuns),
    SomeRunsKey (MkSomeRunsKey),
  )
import Pacer.Command.Chart.Data.Run qualified as R
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
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
  )
import Pacer.Command.Chart.Params qualified as ChartParams
import Pacer.Configuration.Env.Types
  ( CachedPaths,
    LogEnv
      ( MkLogEnv,
        logLevel,
        logNamespace
      ),
  )
import Pacer.Data.Distance (Distance (MkDistance), DistanceUnit (Kilometer))
import Pacer.Data.Distance qualified as D
import Pacer.Data.Duration (Duration (MkDuration))
import Pacer.Utils qualified as U
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath qualified as OsPath
import Test.Tasty.HUnit (assertEqual)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart"
    [ createChartTests,
      createChartSeqTests,
      updateLabelTests
    ]

-- | This tests effectful logic, not so much the chart output.
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
          runLabelsPath = Nothing,
          runPaths = []
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
          runLabelsPath = Nothing,
          runPaths = []
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
          runLabelsPath = Nothing,
          runPaths = []
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
          runLabelsPath = Nothing,
          runPaths = []
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
          runLabelsPath = Nothing,
          runPaths = []
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
          [ [osp|chart-requests.json|],
            [osp|chart-requests.jsonc|],
            [osp|runs.json|],
            [osp|runs.jsonc|],
            [osp|run-labels.json|],
            [osp|run-labels.jsonc|]
          ]

runFileWriterMock ::
  ( IORefE :> es,
    Reader ChartEnv :> es
  ) =>
  Eff (FileWriter : es) a ->
  Eff es a
runFileWriterMock = interpret_ $ \case
  WriteBinaryFile _ _ -> incIORef (.refsEnv.numFileWriterCalls)
  other -> error $ "runFileWriterMock: unimplemented: " ++ (showEffectCons other)

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
    if
      | fileName `Set.member` knownFiles -> pure True
      | fileName `Set.member` knownMissingFiles -> pure False
      | otherwise -> error $ "doesFileExist: unexpected: '" ++ show fileName ++ "'"
    where
      (_dir, fileName) = OsPath.splitFileName p

      knownFiles =
        Set.fromList
          [ [osp|build_charts.ts|],
            [osp|chart-requests.json|],
            [osp|chart-requests.jsonc|],
            [osp|index.html|],
            [osp|index.ts|],
            [osp|package.json|],
            [osp|package-lock.json|],
            [osp|runs.json|],
            [osp|runs.jsonc|],
            [osp|style.css|],
            [osp|tsconfig.json|],
            [osp|types.ts|],
            [osp|utils.ts|],
            [osp|webpack.config.js|]
          ]
      knownMissingFiles =
        Set.fromList
          [ [osp|Activities.csv|],
            [osp|activities.csv|],
            [osp|run-labels.json|],
            [osp|run-labels.jsonc|]
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
  ListDirectory p ->
    if dirName `Set.member` knownDirs
      then pure []
      else error $ "listDirectory: unexpected: " ++ show dirName
    where
      dirName = L.last $ OsPath.splitDirectories p
  PathIsSymbolicLink _ -> pure False
  other -> error $ "runPathReaderMock: unimplemented: " ++ (showEffectCons other)
  where
    knownDirs =
      Set.fromList
        [ [osp|dist|],
          [osp|pacer|]
        ]

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
  other -> error $ "runPathWriterMock: unimplemented: " ++ (showEffectCons other)

runLoggerMock :: Eff (Logger : es) a -> Eff es a
runLoggerMock = interpret_ $ \case
  LoggerLog _ _ _ _ -> pure ()

runTerminalMock :: Eff (Terminal : es) a -> Eff es a
runTerminalMock = interpret_ $ \case
  PutStr _ -> pure ()
  PutStrLn _ -> pure ()
  other -> error $ "runTerminalMock: unimplemented: " ++ (showEffectCons other)

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
  other -> error $ "runTypedProcessMock: unimplemented: " ++ (showEffectCons other)
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
    Reader LogEnv,
    State CachedPaths,
    IOE
  ]

runTestEff :: ChartEnv -> Eff TestEffects a -> IO a
runTestEff env m =
  runEff
    . evalState env.coreEnv.cachedPaths
    . runReader logEnv
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
  where
    logEnv =
      MkLogEnv
        { logLevel = Nothing,
          logNamespace = mempty
        }

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

createChartSeqTests :: TestTree
createChartSeqTests =
  testGroup
    "createChartSeq"
    [ testCreateChartNormal,
      testCreateChartSumWeek,
      testCreateChartSumMonth,
      testCreateChartSumDays11,
      testCreateChartSumDays50
    ]

testCreateChartNormal :: TestTree
testCreateChartNormal =
  testCreateChartSeqHelper
    "Creates a chart"
    [osp|testCreateChartNormal|]
    [ospPathSep|createChartNormal_rc.json|]

testCreateChartSumWeek :: TestTree
testCreateChartSumWeek =
  testCreateChartSeqHelper
    "Creates a chart that sums by week"
    [osp|testCreateChartSumWeek|]
    [ospPathSep|createChartSumWeek_rc.json|]

testCreateChartSumMonth :: TestTree
testCreateChartSumMonth =
  testCreateChartSeqHelper
    "Creates a chart that sums by month"
    [osp|testCreateChartSumMonth|]
    [ospPathSep|createChartSumMonth_rc.json|]

testCreateChartSumDays11 :: TestTree
testCreateChartSumDays11 =
  testCreateChartSeqHelper
    "Creates a chart that sums by 11 days"
    [osp|testCreateChartSumDays11|]
    [ospPathSep|createChartSumDays11_rc.json|]

testCreateChartSumDays50 :: TestTree
testCreateChartSumDays50 =
  testCreateChartSeqHelper
    "Creates a chart that sums by 50 days"
    [osp|testCreateChartSumDays50|]
    [ospPathSep|createChartSumDays50_rc.json|]

testCreateChartSeqHelper :: TestName -> OsPath -> OsPath -> TestTree
testCreateChartSeqHelper testDesc testName crPath = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc,
          testName,
          runner = do
            paths <- mkPaths
            result <- fmap (.chartData) <$> runCreateChartSeqEff paths
            pure $ toStrictBS $ U.encodePretty result
        }

    mkPaths = runEff $ runPathReader $ do
      cwd <- getCurrentDirectory
      chartRequests <- Path.parseRelFile $ dataDir </> crPath
      activities <- Path.parseRelFile $ dataDir </> [ospPathSep|Activities.csv|]
      pure
        $ ( cwd <</>> chartRequests,
            Nothing,
            (cwd <</>> activities) :| []
          )

    dataDir = [ospPathSep|test/unit/data|]

runCreateChartSeqEff :: ChartPaths -> IO (Seq Chart)
runCreateChartSeqEff paths =
  runEff
    . runReader logEnv
    . runLoggerNS mempty
    . runLoggerMock
    . runFileReader
    $ Chart.createChartSeq paths
  where
    logEnv =
      MkLogEnv
        { logLevel = Nothing,
          logNamespace = mempty
        }

updateLabelTests :: TestTree
updateLabelTests =
  testGroup
    "updateLabels"
    [ testUpdateLabels
    ]

testUpdateLabels :: TestTree
testUpdateLabels = testCase "Updates labels from map" $ do
  let (runsResults, unmatchedTsResults) =
        first
          ( fmap (.unSomeRunsKey)
              . NE.toList
              . NESet.toList
              . (.unSomeRuns)
          )
          $ (Chart.updateLabels labelMap runs)

  -- Need to compare SomeRuns NOT SomeRunsKey as the latter compares via
  -- timestamp only.

  length runsExpected @=? length runsResults
  for_ (zip runsExpected runsResults) $ \(e, r) -> do
    e @=? r

  let unmatchedTsResultList =
        second (NE.toList . NESet.toList) <$> Map.toList unmatchedTsResults

  unmatchedTsExpected @=? unmatchedTsResultList
  where
    -- 1. Runs is a union
    -- 2. Runs do _not_ use overlap logic
    -- 3. timezone utc change?
    runs :: SomeRuns Double
    runs = MkSomeRuns $ unsafeNESet [x1, x2, x3, x4]

    runsExpected = [y1, y2, y3, y4]

    unmatchedTsExpected =
      [ (unsafeTs "2024-08-10", ["bad_label1", "bad_label2"])
      ]

    x1 = mkSrk "Unions labels" (unsafeTs "2024-08-10T12:20:30") ["l1", "l2"]
    x2 = mkSrk "Map labels 1" (unsafeTs "2024-08-10T14:20:30") []
    x3 = mkSrk "No labels" (unsafeTs "2024-08-12") []
    x4 = mkSrk "TZ match" (unsafeTs "2024-08-15T15:00:00-0800") []

    y1 = mkSr "Unions labels" (unsafeTs "2024-08-10T12:20:30") ["l1", "l2", "l3"]
    y2 = mkSr "Map labels 1" (unsafeTs "2024-08-10T14:20:30") ["l5", "l6"]
    y3 = mkSr "No labels" (unsafeTs "2024-08-12") []
    y4 = mkSr "TZ match" (unsafeTs "2024-08-15T15:00:00-0800") ["tz_label"]

    labelMap :: Map Timestamp (NESet Text)
    labelMap =
      Map.fromList
        [ (unsafeTs "2024-08-10T12:20:30", unsafeNESet ["l2", "l3"]),
          (unsafeTs "2024-08-10T14:20:30", unsafeNESet ["l5", "l6"]),
          -- This example overlaps with the top 2, but we want exact equality
          -- hence it should _not_ be chosen.
          (unsafeTs "2024-08-10", unsafeNESet ["bad_label1", "bad_label2"]),
          -- This does not _exactly_ match the intended x4, but once we
          -- account for the timezone it will.
          (unsafeTs "2024-08-15T14:00:00-0900", unsafeNESet ["tz_label"])
        ]

    mkSrk title datetime = MkSomeRunsKey . mkSr title datetime

    mkSr title datetime labels =
      D.hideDistance
        $ MkRun
          { datetime,
            distance = MkDistance @Kilometer (fromℤ 5),
            duration = MkDuration (fromℤ 1200),
            labels = Set.fromList labels,
            title = Just title
          }

    unsafeNESet :: forall a. (Ord a) => List a -> NESet a
    unsafeNESet = NESet.fromList . NE.fromList

    unsafeTs :: Text -> Timestamp
    unsafeTs = errorErr . P.parseAll
