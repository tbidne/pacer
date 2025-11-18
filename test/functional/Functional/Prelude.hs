{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.Prelude
  ( module X,

    -- * HUnit
    (<@=?>),

    -- * Runners
    runMultiArgs,
    runArgs,
    runException,
    runAppArgs,

    -- * Golden
    GoldenOutput (..),
    GoldenParams (..),
    testGoldenParams,
    testChart,
    testChartOs,

    -- * Env
    FuncEnv (..),
  )
where

import Data.IORef qualified as Ref
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    ZonedTime (ZonedTime),
    utc,
  )
import Effectful.FileSystem.FileWriter.Dynamic
  ( FileWriter (WriteBinaryFile),
  )
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
import Effectful.FileSystem.PathReader.Static qualified as PRS
import Effectful.Terminal.Dynamic (Terminal (PutBinary, PutStr, PutStrLn))
import Effectful.Time.Dynamic (Time (GetSystemZonedTime))
import FileSystem.IO (writeBinaryFileIO)
import FileSystem.OsPath (decodeLenient, unsafeDecode, unsafeEncode)
import FileSystem.OsPath qualified as FS.OsPath
import FileSystem.UTF8 qualified as UTF8
import Hedgehog as X
  ( Gen,
    Property,
    PropertyName,
    PropertyT,
    Range,
    annotate,
    annotateShow,
    assert,
    failure,
    forAll,
    property,
    withTests,
    (/==),
    (===),
  )
import Pacer.Command.Chart.Server (Server, runServerMock)
import Pacer.Driver (displayInnerMatchKnown, runApp)
import Pacer.Prelude as X hiding (IO)
import Pacer.Utils.Json (AesonE (MkAesonE))
import System.Environment (withArgs)
import System.FilePath (FilePath)
import System.IO as X (IO)
import System.OsPath qualified as OsPath
import Test.Tasty as X (TestName, TestTree, testGroup)
import Test.Tasty.Golden as X (goldenVsFileDiff)
import Test.Tasty.HUnit as X
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog as X (testPropertyNamed)

-- | Golden test output.
data GoldenOutput
  = -- | The golden output is based on an output file.
    GoldenOutputFile OsPath
  | -- | The golden output is based on logs and output file.
    GoldenOutputFileLogs OsPath
  | -- | The golden output is based on an output file, but we also run
    -- an assertion on the logs. This is needed when the logs contain
    -- data that is hard to test for exact equality e.g. non-deterministic
    -- filepaths, timestamps.
    GoldenOutputFileAssertLogs OsPath (Text -> Bool)

-- | Parameters for golden tests.
data GoldenParams = MkGoldenParams
  { -- | Functions to make the CLI arguments. The parameter is the tmp test
    -- directory.
    mkArgs :: OsPath -> List String,
    -- | Output.
    outFileName :: GoldenOutput,
    -- | Test string description.
    testDesc :: TestName,
    -- | Test function name, for creating unique file paths.
    testName :: OsPath
  }

makeFieldLabelsNoPrefix ''GoldenParams

(<@=?>) :: (Eq a, Show a) => a -> IO a -> Assertion
x <@=?> my = do
  y <- my
  x @=? y

infix 1 <@=?>

data FuncEnv = MkFuncEnv
  { logsRef :: IORef Text,
    outFilesMapRef :: IORef (Map OsPath ByteString),
    xdgConfigCallsRef :: IORef Word8
  }

makeFieldLabelsNoPrefix ''FuncEnv

runPathReaderMock ::
  ( IOE :> es,
    IORefE :> es,
    Reader FuncEnv :> es
  ) =>
  Eff (PathReader : es) a ->
  Eff es a
runPathReaderMock = reinterpret_ PRS.runPathReader $ \case
  CanonicalizePath p -> PRS.canonicalizePath p
  DoesDirectoryExist p -> PRS.doesDirectoryExist p
  DoesFileExist p -> PRS.doesFileExist p
  GetCurrentDirectory -> PRS.getCurrentDirectory
  GetXdgDirectory d p -> case d of
    XdgConfig -> do
      incIORef (view #xdgConfigCallsRef)
      pure $ baseName </> [osp|config|] </> p
    other -> error $ "Unexpected xdg type: " ++ show other
    where
      baseName = [ospPathSep|test/functional/data/xdg|]
  ListDirectory p -> PRS.listDirectory p
  other -> error $ "runPathReaderMock: unimplemented: " ++ showEffectCons other

type TestEffects =
  [ FileReader,
    FileWriter,
    Optparse,
    PathReader,
    PathWriter,
    Server,
    Terminal,
    IORefE,
    Time,
    Concurrent,
    Reader FuncEnv,
    IOE
  ]

runTestEff :: FuncEnv -> Eff TestEffects a -> IO a
runTestEff env m = do
  runEff
    . runReader @FuncEnv env
    . runConcurrent
    . runTimeMock
    . runIORef
    . runTerminalMock
    . runServerMock
    . runPathWriter
    . runPathReaderMock
    . runOptparse
    . runFileWriterMock
    . runFileReader
    $ m
    `catch` \(MkAesonE mPath err) ->
      -- HACK: The 'Duplicate date error' chart test throws an AesonE,
      -- which is a problem for the golden tests because it includes a
      -- non-deterministic absolute path. Thus we catch the exception and
      -- make the path relative. If this ends up being flaky, we can simply
      -- use the file name, which should be good enough.
      case mPath of
        Just p -> do
          p' <- makeRelativeToCurrentDirectoryIO p
          throwM $ MkAesonE (Just p') err
        Nothing -> throwM $ MkAesonE Nothing err
  where
    makeRelativeToCurrentDirectoryIO =
      liftIO
        . runEff
        . PRS.runPathReader
        . PRS.makeRelativeToCurrentDirectory

runTimeMock ::
  Eff (Time : es) a ->
  Eff es a
runTimeMock = interpret $ \_ -> \case
  GetSystemZonedTime -> pure zonedTime
  other -> error $ "runTimeMock: unimplemented: " ++ showEffectCons other
  where
    localTime :: LocalTime
    localTime = LocalTime day tod
      where
        day = fromOrdinalDate 2022 39
        tod = TimeOfDay 10 20 5

    zonedTime :: ZonedTime
    zonedTime = ZonedTime localTime utc

runFuncIO :: Eff TestEffects a -> IO FuncEnv
runFuncIO m = do
  logsRef <- Ref.newIORef ""
  outFilesMapRef <- Ref.newIORef Map.empty
  xdgConfigCallsRef <- Ref.newIORef 0
  let env =
        MkFuncEnv
          { logsRef,
            outFilesMapRef,
            xdgConfigCallsRef
          }

  -- temporarily catch the exception, so we can verify assertions that we
  -- always want to be true.
  eResult <- trySync $ runTestEff env m

  xdgConfigCalls <- Ref.readIORef xdgConfigCallsRef
  assertBool
    ("xdg config calls should be <= 1: " ++ show xdgConfigCalls)
    (xdgConfigCalls <= 1)

  -- rethrow the exception if it exists.
  firstA throwM eResult

  pure env

runFileWriterMock ::
  ( IORefE :> es,
    Reader FuncEnv :> es
  ) =>
  Eff (FileWriter : es) a ->
  Eff es a
runFileWriterMock = interpret_ $ \case
  WriteBinaryFile path bs -> do
    outFilesMapRef <- asks @FuncEnv (view #outFilesMapRef)
    modifyIORef' outFilesMapRef (Map.insert path' bs)
    where
      path' = takeParentAndName path
  other -> error $ "runFileWriterMock: unimplemented: " ++ showEffectCons other

runTerminalMock ::
  ( IORefE :> es,
    Reader FuncEnv :> es
  ) =>
  Eff (Terminal : es) a ->
  Eff es a
runTerminalMock = interpret_ $ \case
  PutStr s -> do
    logsRef <- asks @FuncEnv (view #logsRef)
    modifyIORef' logsRef (<> packText s)
  PutStrLn s -> do
    logsRef <- asks @FuncEnv (view #logsRef)
    modifyIORef' logsRef (<> packText s)
  PutBinary bs -> do
    logsRef <- asks @FuncEnv (view #logsRef)
    modifyIORef' logsRef (<> UTF8.decodeUtf8Lenient bs)
  other -> error $ "runTerminalMock: unimplemented: " ++ showEffectCons other

runMultiArgs :: (a -> List String) -> List (Tuple2 Word8 (Tuple2 a Text)) -> IO Unit
runMultiArgs mkArgs vals =
  for_ vals $ \(idx, (a, e)) -> do
    let args = mkArgs a
    runArgs (Just idx) args e

-- | Runs pacer with the arguments and compares the log output against the
-- expected text.
runArgs :: Maybe Word8 -> List String -> Text -> IO Unit
runArgs mIdx args expected = do
  funcEnv <- runAppArgs args
  result <- Ref.readIORef (funcEnv ^. #logsRef)

  let (idxTxt, indentTxt) = case mIdx of
        Just idx -> (showt idx <> ". ", "   ")
        Nothing -> ("", "")

  let msg =
        mconcat
          [ idxTxt,
            "Args: ",
            showt args,
            "\n",
            indentTxt,
            "Diff: ",
            expected,
            " /= ",
            result
          ]
  assertBool (unpackText msg) (expected == result)

runException :: forall e. (Exception e) => TestName -> Text -> List String -> TestTree
runException @e desc expected args = testCase desc $ do
  eFuncEnv <- try @_ @e $ withArgs args $ runFuncIO runApp
  eResult <- secondA (\x -> Ref.readIORef (x ^. #logsRef)) eFuncEnv

  case eResult of
    Right r -> assertFailure $ unpackText $ "Expected exception, received: " <> r
    Left ex -> expected @=? displayExceptiont ex

-- | Low level runner. Does nothing except runs pacer w/ the args.
runAppArgs :: List String -> IO FuncEnv
runAppArgs args = withArgs args $ runFuncIO runApp

-- | Given a text description and testName OsPath, creates a golden test.
-- Expects the following to exist:
--
-- - @data\/testName_runs.json@
-- - @data\/testName_chart-requests.json@
-- - @goldens\/testName.golden@
--
-- Note that, confusingly, the 'TestName' type is __not__ what we are
-- referring to as testName. Rather, tasty takes in a string test
-- __description__, which has type 'TestName'. We call this description
--
-- By 'test name', we refer to the actual function name e.g. in
--
-- @
-- testFoo = testChart "some description" [osp|testFoo|]
-- @
--
-- testFoo is the 'test name'.
testChart :: TestName -> OsPath -> IO OsPath -> TestTree
testChart = testChartOs False

-- | Like 'testChart', except it includes to determine if we take the current
-- OS into account.
testChartOs ::
  -- | If true, we will append posix/windows to the end of the golden test
  -- path e.g. testName_posix.golden.
  Bool ->
  -- | Test description.
  TestName ->
  -- | Test name.
  OsPath ->
  -- | Retrieves the current directory.
  IO OsPath ->
  TestTree
testChartOs osSwitch testDesc testName getTestDir = testGoldenParams getTestDir params
  where
    -- This is the path to the golden files. If osSwitch is false, then it
    -- is the same as testName e.g. testName.golden. If the switch is active,
    -- then we need to append the os e.g. testName_posix.golden.
    goldenName =
      if osSwitch
        then testName <> unsafeEncode ("_" ++ posixWindowsStr)
        else testName

    params =
      MkGoldenParams
        { mkArgs = \p ->
            [ "chart",
              "--data",
              dataDir,
              "--json",
              "--build-dir",
              buildDir p
            ],
          -- The chart tests will all write an output file charts.json
          outFileName = GoldenOutputFile [ospPathSep|build/charts.json|],
          testDesc,
          testName = goldenName
        }

    -- These are always based on the testName, since all Os's share the same
    -- CLI inputs.
    basePath = [ospPathSep|test/functional/data|]
    buildDir p = unsafeDecode $ p </> [osp|build|]
    dataDir = unsafeDecode $ basePath </> testName

testGoldenParams :: IO OsPath -> GoldenParams -> TestTree
testGoldenParams getTestDir goldenParams =
  goldenDiff (goldenParams ^. #testDesc) goldenPath actualPath $ do
    testDir <- getTestDir

    let args = (goldenParams ^. #mkArgs) testDir
    eFuncEnv <- trySync $ withArgs args $ runFuncIO runApp
    case eFuncEnv of
      Left err -> writeActualFile $ exToBs err
      Right funcEnv ->
        case goldenParams ^. #outFileName of
          -- We expect a file to have been written to a given path. This is
          -- what the golden test is based on.
          GoldenOutputFile expectedName ->
            mkFileOutput funcEnv expectedName >>= writeActualFile
          GoldenOutputFileLogs expectedName -> do
            logsBs <- encodeUtf8 <$> mkLogOutput funcEnv
            fileBs <- mkFileOutput funcEnv expectedName
            writeActualFile $ logsBs <> "\n\n" <> fileBs
          GoldenOutputFileAssertLogs expectedName onLogs -> do
            logsTxt <- mkLogOutput funcEnv
            unless (onLogs logsTxt) (throwText $ "Logs failed assertion: " <> logsTxt)

            fileBs <- mkFileOutput funcEnv expectedName
            writeActualFile fileBs
  where
    outputPathStart =
      FS.OsPath.unsafeDecode
        $ [ospPathSep|test/functional/goldens|]
        </> (goldenParams ^. #testName)

    exToBs = encodeUtf8 . packText . displayInnerMatchKnown

    writeActualFile :: ByteString -> IO Unit
    writeActualFile =
      writeBinaryFileIO (FS.OsPath.unsafeEncode actualPath)
        . (<> "\n")

    actualPath = outputPathStart <> ".actual"
    goldenPath = outputPathStart <> ".golden"

    mkLogOutput funcEnv = Ref.readIORef $ funcEnv ^. #logsRef

    mkFileOutput funcEnv expectedName = do
      outFilesMap <- Ref.readIORef $ funcEnv ^. #outFilesMapRef
      case Map.lookup expectedName outFilesMap of
        -- Found the write attempt in our env's map. Write it to compare
        -- it with the golden expectation.
        Just bs -> do
          let fileNameBs =
                encodeUtf8
                  -- normalize slashes for windows
                  . T.replace "\\" "/"
                  . packText
                  . decodeLenient
                  $ expectedName
          pure $ fileNameBs <> "\n\n" <> bs
        -- Expected file not found, failure.
        Nothing -> do
          let msg =
                mconcat
                  [ "functional.writeBinaryFile: Unexpected path: '",
                    packText (decodeLenient expectedName),
                    "'.\n\nKnown paths:\n\n",
                    showt $ Map.keysSet outFilesMap
                  ]
          pure $ encodeUtf8 msg

incIORef ::
  ( IORefE :> es,
    Reader FuncEnv :> es
  ) =>
  (FuncEnv -> IORef Word8) ->
  Eff es Unit
incIORef toRef = asks toRef >>= \r -> modifyIORef' r (+ 1)

-- NOTE: [Golden test diffing]
--
-- We use a custom diff (git diff) over the default (goldenVsFile)
-- because the former will print the diff in the actual test output, while
-- the latter just says something like "files are different" and fails.
-- This means we need to check the actual files to see the diff ourselves.
--
-- This is minor annoyance locally but it's a real issue on CI, since we have
-- to upload the files (ensuring paths don't collide), download them, then
-- diff locally.
--
-- This way is much simpler, and only requires git.
goldenDiff :: TestName -> FilePath -> FilePath -> IO Unit -> TestTree
goldenDiff x = goldenVsFileDiff x diffArgs
  where
    -- Apparently, the 'diff' program exists for windows and unix on CI. Thus
    -- the arguments ["diff", "-u" "--color=always", ref, new] also seem fine.
    -- Nevertheless, we use git as it is also likely to always be around.
    diffArgs ref new =
      [ "git",
        "diff",
        "--exit-code",
        "--color=always",
        "--no-index",
        ref,
        new
      ]

takeParentAndName :: OsPath -> OsPath
takeParentAndName path = parentDir </> fileName
  where
    takeParentDir = L.last . OsPath.splitDirectories . OsPath.takeDirectory
    parentDir = takeParentDir path
    fileName = OsPath.takeFileName path
