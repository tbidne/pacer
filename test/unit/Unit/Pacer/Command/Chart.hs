{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unit.Pacer.Command.Chart (tests) where

import Data.Char qualified as Ch
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
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
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
import Effectful.Terminal.Dynamic (Terminal (PutStr, PutStrLn))
import FileSystem.IO (readBinaryFileIO)
import FileSystem.OsPath (decodeLenient)
import FileSystem.Path qualified as Path
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart (ChartPaths)
import Pacer.Command.Chart qualified as Chart
import Pacer.Command.Chart.Data.Activity
  ( Activity
      ( MkActivity,
        datetime,
        distance,
        duration,
        labels,
        title
      ),
    SomeActivities (MkSomeActivities),
    SomeActivityKey (MkSomeActivityKey),
  )
import Pacer.Command.Chart.Data.Activity qualified as R
import Pacer.Command.Chart.Data.Activity.ActivityLabel (Label)
import Pacer.Command.Chart.Data.Chart (Charts)
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
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
import Pacer.Command.Chart.Params qualified as ChartParams
import Pacer.Command.Chart.Server (ServerEff, runServerEffMock)
import Pacer.Configuration.Env.Types
  ( CachedPaths,
    LogEnv
      ( MkLogEnv,
        logLevel,
        logNamespace,
        logVerbosity
      ),
  )
import Pacer.Data.Distance (Distance (MkDistance), DistanceUnit (Kilometer))
import Pacer.Data.Distance qualified as D
import Pacer.Data.Duration (Duration (MkDuration))
import Pacer.Utils.Json qualified as Json
import Pacer.Utils.Show qualified as Utils.Show
import System.OsPath qualified as OsPath
import System.OsString qualified as OsString
import Test.Tasty.HUnit (assertEqual)
import Unit.Prelude

newtype CoreEnv = MkCoreEnv
  { cachedPaths :: CachedPaths
  }

makeFieldLabelsNoPrefix ''CoreEnv

data RefsEnv = MkRefsEnv
  { numFileWriterCalls :: IORef Word8,
    numRemoveDirectoryCalls :: IORef Word8,
    numXdgCacheCalls :: IORef Word8,
    numXdgConfigCalls :: IORef Word8
  }

makeFieldLabelsNoPrefix ''RefsEnv

data ChartEnv = MkChartEnv
  { coreEnv :: CoreEnv,
    refsEnv :: RefsEnv
  }

makeFieldLabelsNoPrefix ''ChartEnv

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
    [ createChartSuccessTests
    ]

createChartSuccessTests :: TestTree
createChartSuccessTests =
  testGroup
    "Happy paths"
    [ testDefault,
      testJson,
      testClean
    ]

testDefault :: TestTree
testDefault = testCase "Default" $ do
  refsEnv <- runCreateCharts coreEnv params
  assertEnvRefs refsEnv [0, 0, 1, 1]
  where
    params =
      MkChartParams
        { buildDir = Nothing,
          cleanInstall = False,
          dataDir = Nothing,
          json = False,
          chartRequestsPath = Nothing,
          activityLabelsPath = Nothing,
          activityPaths = [],
          port = Nothing
        }
    coreEnv =
      MkCoreEnv
        { cachedPaths = mempty
        }

testJson :: TestTree
testJson = testCase "With --json" $ do
  refsEnv <- runCreateCharts coreEnv params
  assertEnvRefs refsEnv [1, 0, 0, 1]
  where
    params =
      MkChartParams
        { buildDir = Nothing,
          cleanInstall = False,
          dataDir = Nothing,
          json = True,
          chartRequestsPath = Nothing,
          activityLabelsPath = Nothing,
          activityPaths = [],
          port = Nothing
        }
    coreEnv =
      MkCoreEnv
        { cachedPaths = mempty
        }

testClean :: TestTree
testClean = testCase "With --clean" $ do
  refsEnv <- runCreateCharts coreEnv params
  assertEnvRefs refsEnv [2, 1, 1, 1]
  where
    params =
      MkChartParams
        { buildDir = Nothing,
          cleanInstall = True,
          dataDir = Nothing,
          json = False,
          chartRequestsPath = Nothing,
          activityLabelsPath = Nothing,
          activityPaths = [],
          port = Nothing
        }
    coreEnv =
      MkCoreEnv
        { cachedPaths = mempty
        }

assertEnvRefs :: RefsEnv -> List Word8 -> IO ()
assertEnvRefs refsEnv expecteds = do
  length expecteds @=? length refs
  assertRefs vals
  where
    vals = zipWith (\(d, r) e -> (d, r, e)) refs expecteds

    refs =
      [ ("numFileWriterCalls", refsEnv ^. #numFileWriterCalls),
        ("numRemoveDirectoryCalls", refsEnv ^. #numRemoveDirectoryCalls),
        ("numXdgCacheCalls", refsEnv ^. #numXdgCacheCalls),
        ("numXdgConfigCalls", refsEnv ^. #numXdgConfigCalls)
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
            [osp|activities.json|],
            [osp|activities.jsonc|],
            [osp|activity-labels.json|],
            [osp|activity-labels.jsonc|]
          ]

runFileWriterMock ::
  ( IORefE :> es,
    Reader ChartEnv :> es
  ) =>
  Eff (FileWriter : es) a ->
  Eff es a
runFileWriterMock = interpret_ $ \case
  WriteBinaryFile _ _ -> incIORef (view (#refsEnv % #numFileWriterCalls))
  other -> error $ "runFileWriterMock: unimplemented: " ++ showEffectCons other

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
    [osp|cwd|] -> pure True
    [osp|dist|] -> pure True
    [osp|pacer|] -> pure True
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
      | knownDirFileMissing -> pure False
      | fileName `Set.member` knownFiles -> pure True
      | fileName `Set.member` knownMissingFiles -> pure False
      | otherwise -> error $ "doesFileExist: unexpected: '" ++ show fileName ++ "'"
    where
      (dir, fileName) = OsPath.splitFileName p

      dirt = Utils.Show.showtOsPath $ OsPath.dropTrailingPathSeparator dir

      -- Files where the directory exists but we want the file to not exist.
      -- Have to do it in this convoluted way since the directory may
      -- non-deterministically include the actual absolute path, due to
      -- parseAbs.
      --
      knownDirFileMissing = "cwd" `T.isSuffixOf` dirt

      knownFiles =
        Set.fromList
          [ [osp|bundle.js|],
            [osp|chart-requests.json|],
            [osp|chart-requests.jsonc|],
            [osp|index.html|],
            [osp|activities.json|],
            [osp|activities.jsonc|]
          ]
      knownMissingFiles =
        Set.fromList
          [ [osp|activity-labels.json|],
            [osp|activity-labels.jsonc|],
            [osp|activity_labels.json|],
            [osp|activity_labels.jsonc|],
            [osp|chart_requests.json|],
            [osp|chart_requests.jsonc|]
          ]
  FindExecutable p -> error $ "findExecutable: unexpected: " ++ show p
  GetCurrentDirectory -> pure [osp|cwd|]
  GetXdgDirectory xdg p -> case xdg of
    XdgCache ->
      incIORef (view (#refsEnv % #numXdgCacheCalls))
        $> ([ospPathSep|test/unit/data/xdg/cache|] </> p)
    XdgConfig ->
      incIORef (view (#refsEnv % #numXdgConfigCalls))
        $> ([ospPathSep|test/unit/data/xdg/config|] </> p)
    other ->
      error $ "getXdgDirectory: unexpected: " ++ show other
  ListDirectory p
    | dirName == [osp|pacer|] ->
        pure
          [ [osp|activities.json|],
            [osp|chart-requests.json|]
          ]
    | dirName == [osp|cwd|] -> pure []
    | otherwise -> error $ "listDirectory: unexpected: " ++ show dirName
    where
      dirName = L.last $ OsPath.splitDirectories p
  PathIsSymbolicLink _ -> pure False
  other -> error $ "runPathReaderMock: unimplemented: " ++ showEffectCons other

runPathWriterMock ::
  ( IORefE :> es,
    Reader ChartEnv :> es
  ) =>
  Eff (PathWriter : es) a ->
  Eff es a
runPathWriterMock = interpret_ $ \case
  CreateDirectory _ -> pure ()
  CreateDirectoryIfMissing _ _ -> pure ()
  RemovePathForcibly _ -> incIORef (view (#refsEnv % #numRemoveDirectoryCalls))
  SetCurrentDirectory _ -> pure ()
  other -> error $ "runPathWriterMock: unimplemented: " ++ showEffectCons other

runLoggerMock :: Eff (Logger : es) a -> Eff es a
runLoggerMock = interpret_ $ \case
  LoggerLog {} -> pure ()

runTerminalMock :: Eff (Terminal : es) a -> Eff es a
runTerminalMock = interpret_ $ \case
  PutStr _ -> pure ()
  PutStrLn _ -> pure ()
  other -> error $ "runTerminalMock: unimplemented: " ++ showEffectCons other

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
    ServerEff,
    Terminal,
    Logger,
    LoggerNS,
    IORefE,
    Reader ChartEnv,
    Reader LogEnv,
    State CachedPaths,
    IOE
  ]

runTestEff :: ChartEnv -> Eff TestEffects a -> IO a
runTestEff env =
  runEff
    . evalState (env ^. #coreEnv % #cachedPaths)
    . runReader logEnv
    . runReader env
    . runIORef
    . runLoggerNS mempty
    . runLoggerMock
    . runTerminalMock
    . runServerEffMock
    . runPathWriterMock
    . runPathReaderMock
    . runFileWriterMock
    . runFileReaderMock
  where
    logEnv =
      MkLogEnv
        { logLevel = Nothing,
          logNamespace = mempty,
          logVerbosity = mempty
        }

runMockChartIO ::
  CoreEnv ->
  Eff TestEffects a ->
  IO RefsEnv
runMockChartIO coreEnv m = do
  numFileWriterCalls <- Ref.newIORef 0
  numRemoveDirectoryCalls <- Ref.newIORef 0
  numXdgCacheCalls <- Ref.newIORef 0
  numXdgConfigCalls <- Ref.newIORef 0

  let refsEnv =
        MkRefsEnv
          { numFileWriterCalls,
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

runCreateCharts :: CoreEnv -> ChartParamsArgs -> IO RefsEnv
runCreateCharts coreEnv params = runMockChartIO coreEnv $ do
  params' <- ChartParams.evolvePhase params Nothing
  -- call handle since that is the actual entry point. createCharts
  -- merely creates the Charts, for later file writing or serving.
  Chart.handle Nothing params'

-- NOTE: The Activities.csv file currently warns due to an activity (cycling)
-- w/ distance 0.0. Previously this did nothing since we were throwing away
-- non-Running activities. Now, it _would_ log a warning, but our mock
-- logger does nothing. So this is actually fine, and arguably nice to have
-- since it proves the tests do not fail for a single parse failure,
-- which is what we want. Still, we document it here in case just in case.
createChartSeqTests :: TestTree
createChartSeqTests =
  testGroup
    "createChartSeq"
    [ testCreateChartNormal,
      testCreateChartSumWeek,
      testCreateChartSumMonth,
      testCreateChartSumDays11,
      testCreateChartSumDays50,
      testCreateChartFilterSet
    ]

testCreateChartNormal :: TestTree
testCreateChartNormal =
  testCreateChartSeqHelper
    "Creates a chart"
    [osp|createChartNormal|]

testCreateChartSumWeek :: TestTree
testCreateChartSumWeek =
  testCreateChartSeqHelper
    "Creates a chart that sums by week"
    [osp|createChartSumWeek|]

testCreateChartSumMonth :: TestTree
testCreateChartSumMonth =
  testCreateChartSeqHelper
    "Creates a chart that sums by month"
    [osp|createChartSumMonth|]

testCreateChartSumDays11 :: TestTree
testCreateChartSumDays11 =
  testCreateChartSeqHelper
    "Creates a chart that sums by 11 days"
    [osp|createChartSumDays11|]

testCreateChartSumDays50 :: TestTree
testCreateChartSumDays50 =
  testCreateChartSeqHelper
    "Creates a chart that sums by 50 days"
    [osp|createChartSumDays50|]

testCreateChartFilterSet :: TestTree
testCreateChartFilterSet =
  testCreateChartSeqHelper
    "Creates a chart with set filters"
    [osp|createChartFilterSet|]

testCreateChartSeqHelper :: TestName -> OsPath -> TestTree
testCreateChartSeqHelper testDesc testSuffix = testGoldenParams params
  where
    fullTestName = case OsString.uncons testSuffix of
      Nothing ->
        error "testCreateChartSeqHelper: test suffix cannot be empty"
      Just (c, str) ->
        [osp|test|]
          <> OsString.cons
            (OsString.unsafeFromChar . Ch.toUpper . OsString.toChar $ c)
            str

    params =
      MkGoldenParams
        { testDesc,
          testName = fullTestName,
          runner = do
            paths <- mkPaths
            result <-
              fmap (view #chartData)
                . view #charts
                <$> runCreateChartSeqEff paths
            pure $ toStrictBS $ Json.encodePretty result
        }

    mkPaths = runEff $ runPathReader $ do
      cwd <- getCurrentDirectory

      let crPath = dataDir </> testSuffix <> [osp|_rc.json|]
          labelsPath = dataDir </> testSuffix <> [osp|_labels.json|]

      chartRequests <- Path.parseRelFile crPath

      labelsExists <- PR.doesFileExist labelsPath
      labels <-
        if labelsExists
          then Just <$> Path.parseRelFile labelsPath
          else pure Nothing

      activities <- Path.parseRelFile $ dataDir </> [ospPathSep|Activities.csv|]
      pure
        ( cwd <</>> chartRequests,
          (cwd <</>>) <$> labels,
          (cwd <</>> activities) :<|| []
        )

    dataDir = [ospPathSep|test/unit/data|]

runCreateChartSeqEff :: ChartPaths -> IO Charts
runCreateChartSeqEff paths =
  runEff
    . runReader logEnv
    . runLoggerNS mempty
    . runLoggerMock
    . runFileReader
    $ Chart.createChartSeq Nothing paths
  where
    logEnv =
      MkLogEnv
        { logLevel = Nothing,
          logNamespace = mempty,
          logVerbosity = mempty
        }

updateLabelTests :: TestTree
updateLabelTests =
  testGroup
    "updateLabels"
    [ testUpdateLabels
    ]

testUpdateLabels :: TestTree
testUpdateLabels = testCase "Updates labels from map" $ do
  let (activitiesResults, unmatchedTsResults) =
        first
          ( fmap (view #unSomeActivityKey)
              . NE.toList
              . NESet.toList
              . view #unSomeActivities
          )
          (Chart.updateLabels labelMap activities)

  -- Need to compare SomeActivities NOT SomeActivityKey as the latter compares via
  -- timestamp only.

  length activitiesExpected @=? length activitiesResults
  for_ (zip activitiesExpected activitiesResults) $ \(e, r) -> do
    e @=? r

  let unmatchedTsResultList =
        second (NE.toList . NESet.toList) <$> Map.toList unmatchedTsResults

  unmatchedTsExpected @=? unmatchedTsResultList
  where
    -- 1. Activities is a union
    -- 2. Activities do _not_ use overlap logic
    -- 3. timezone utc change?
    activities :: SomeActivities Double
    activities = MkSomeActivities $ unsafeNESet [x1, x2, x3, x4]

    activitiesExpected = [y1, y2, y3, y4]

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

    labelMap :: Map Timestamp (NESet Label)
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

    mkSrk title datetime = MkSomeActivityKey . mkSr title datetime

    mkSr title datetime labels =
      D.hideDistance
        $ MkActivity
          { atype = Nothing,
            datetime,
            distance = MkDistance @Kilometer (fromℤ 5),
            duration = MkDuration (fromℤ 1200),
            labels = Set.fromList labels,
            title = Just title
          }

    unsafeNESet :: forall a. (Ord a) => List a -> NESet a
    unsafeNESet = NESet.fromList . NE.fromList

    unsafeTs :: Text -> Timestamp
    unsafeTs = errorErr . P.parseAll
