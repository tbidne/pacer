{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.Prelude
  ( module X,

    -- * HUnit
    (<@=?>),

    -- * Runners
    runMultiArgs,
    runArgs,
    runException,

    -- * Golden
    GoldenParams (..),
    testGoldenParams,
    testChart,
    testChartPosix,
  )
where

import Control.Exception (throwIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import Effects.FileSystem.PathReader qualified as PR
import FileSystem.OsPath (decodeLenient, unsafeDecode, unsafeEncode)
import FileSystem.OsPath qualified as FS.OsPath
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
import Pacer.Driver (runApp)
import Pacer.Exception (TomlE (MkTomlE), displayInnerMatchKnown)
import Pacer.Prelude as X hiding (IO)
import System.Environment (withArgs)
import System.IO as X (IO)
import System.OsPath qualified as OsPath
import Test.Tasty as X (TestName, TestTree, testGroup)
import Test.Tasty.Golden as X (goldenVsFile)
import Test.Tasty.HUnit as X
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog as X (testPropertyNamed)

(<@=?>) :: (Eq a, Show a) => a -> IO a -> Assertion
x <@=?> my = do
  y <- my
  x @=? y

infix 1 <@=?>

data FuncEnv = MkFuncEnv
  { logsRef :: IORef Text,
    outFilesMapRef :: IORef (Map OsPath ByteString)
  }

newtype FuncIO a = MkFuncIO {unFuncIO :: ReaderT FuncEnv IO a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadFileReader,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadOptparse,
      MonadPathWriter,
      MonadPathReader,
      MonadReader FuncEnv,
      MonadTypedProcess
    )

-- HACK: The 'Duplicate date error' chart test throws a TomlE, which is
-- a problem for the golden tests because it includes a non-deterministic
-- absolute path. Thus we override MonadThrow s.t. -- in this case -- we
-- make the path relative. If this ends up being flaky, we can simply use the
-- file name, which should be good enough.
instance MonadThrow FuncIO where
  throwM e = case fromException (toException e) of
    Just (MkTomlE p err) -> liftIO $ do
      p' <- PR.makeRelativeToCurrentDirectory p
      throwIO $ MkTomlE p' err
    Nothing -> liftIO $ throwIO e

runFuncIO :: FuncIO a -> IO FuncEnv
runFuncIO m = do
  logsRef <- newIORef ""
  outFilesMapRef <- newIORef Map.empty
  let env =
        MkFuncEnv
          { logsRef,
            outFilesMapRef
          }

  _ <- runReaderT (unFuncIO m) env
  pure env

instance MonadFileWriter FuncIO where
  -- When a test tries to write a file, record it in the map i.e.
  --
  -- writeBinaryFile p bs --> Map.insert p bs
  writeBinaryFile path bs = do
    outFilesMapRef <- asks (.outFilesMapRef)
    liftIO $ modifyIORef' outFilesMapRef (Map.insert fileName bs)
    where
      fileName = OsPath.takeFileName path

instance MonadTerminal FuncIO where
  putStrLn s = do
    logsRef <- asks (.logsRef)
    liftIO $ modifyIORef' logsRef (<> packText s)

runMultiArgs :: (a -> List String) -> List (Word8, (a, Text)) -> IO ()
runMultiArgs mkArgs vals =
  for_ vals $ \(idx, (a, e)) -> do
    let args = mkArgs a
    runArgs (Just idx) args e

runArgs :: Maybe Word8 -> List String -> Text -> IO ()
runArgs mIdx args expected = do
  funcEnv <- withArgs args $ runFuncIO runApp
  result <- readIORef funcEnv.logsRef

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
runException desc expected args = testCase desc $ do
  eFuncEnv <- try @_ @e $ withArgs args $ runFuncIO runApp
  eResult <- secondA (\x -> readIORef x.logsRef) eFuncEnv

  case eResult of
    Right r -> assertFailure $ unpackText $ "Expected exception, received: " <> r
    Left ex -> expected @=? displayExceptiont ex

-- | Parameters for golden tests.
data GoldenParams = MkGoldenParams
  { -- | Functions to make the CLI arguments. The parameter is the tmp test
    -- directory.
    mkArgs :: OsPath -> List String,
    -- | Output file.
    outFileName :: Maybe OsPath,
    -- | Test string description.
    testDesc :: TestName,
    -- | Test function name, for creating unique file paths.
    testName :: OsPath
  }

-- | Given a text description and testName OsPath, creates a golden test.
-- Expects the following to exist:
--
-- - @data\/testName_runs.toml@
-- - @data\/testName_chart-requests.toml@
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
testChart = testChartPosix False

-- | Like 'testChart', except it includes to determine if we take the current
-- OS into account.
testChartPosix ::
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
testChartPosix osSwitch testDesc testName getTestDir = testGoldenParams getTestDir params
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
        { mkArgs =
            const
              [ "chart",
                "--data",
                dataDir,
                "--json"
              ],
          -- The chart tests will all write an output file charts.json
          outFileName = Just [osp|charts.json|],
          testDesc,
          testName = goldenName
        }

    -- These are always based on the testName, since all Os's share the same
    -- CLI inputs.
    basePath = [ospPathSep|test/functional/data|]
    dataDir = unsafeDecode $ basePath </> testName

testGoldenParams :: IO OsPath -> GoldenParams -> TestTree
testGoldenParams getTestDir goldenParams = do
  goldenVsFile goldenParams.testDesc goldenPath actualPath $ do
    testDir <- getTestDir

    let args = goldenParams.mkArgs testDir
    eFuncEnv <- trySync $ withArgs args $ runFuncIO runApp
    case eFuncEnv of
      Left err -> writeActualFile $ exToBs err
      Right funcEnv ->
        case goldenParams.outFileName of
          -- No out file, golden test uses the logs (stdout)
          Nothing -> do
            bs <- encodeUtf8 <$> readIORef funcEnv.logsRef
            writeActualFile bs
          -- We expect a file to have been written to a given path. This is
          -- what the golden test is based on.
          Just expectedName -> do
            outFilesMap <- readIORef funcEnv.outFilesMapRef
            case Map.lookup expectedName outFilesMap of
              -- Found the write attempt in our env's map. Write it to compare
              -- it with the golden expectation.
              Just bs -> writeActualFile bs
              -- Expected file not found, failure.
              Nothing -> do
                let msg =
                      mconcat
                        [ "functional.writeBinaryFile: Unexpected path: '",
                          packText (decodeLenient expectedName),
                          "'.\n\nKnown paths:\n\n",
                          showt $ Map.keysSet outFilesMap
                        ]
                writeActualFile $ encodeUtf8 msg
  where
    outputPathStart =
      FS.OsPath.unsafeDecode
        $ [ospPathSep|test/functional/goldens|]
        </> goldenParams.testName

    exToBs = encodeUtf8 . packText . displayInnerMatchKnown

    writeActualFile :: ByteString -> IO ()
    writeActualFile =
      writeBinaryFile (FS.OsPath.unsafeEncode actualPath)
        . (<> "\n")

    actualPath = outputPathStart <> ".actual"
    goldenPath = outputPathStart <> ".golden"
