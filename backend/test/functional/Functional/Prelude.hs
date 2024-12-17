{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

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
  )
where

import Data.Word (Word8)
import FileSystem.OsPath (unsafeDecode)
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
import Pacer.Driver (runAppWith)
import Pacer.Prelude as X hiding (IO)
import System.Environment (withArgs)
import System.IO as X (IO)
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

runMultiArgs :: (a -> List String) -> List (Word8, (a, Text)) -> IO ()
runMultiArgs mkArgs vals =
  for_ vals $ \(idx, (a, e)) -> do
    let args = mkArgs a
    runArgs (Just idx) args e

runArgs :: Maybe Word8 -> List String -> Text -> IO ()
runArgs mIdx args expected = do
  result <- withArgs args $ runAppWith pure

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
  eResult <- try @e $ withArgs args $ runAppWith pure

  case eResult of
    Right r -> assertFailure $ unpackText $ "Expected exception, received: " <> r
    Left ex -> expected @=? displayExceptiont ex

-- | Parameters for golden tests.
data GoldenParams = MkGoldenParams
  { -- | Functions to make the CLI arguments. The parameter is the tmp test
    -- directory.
    mkArgs :: OsPath -> List String,
    -- | Test string description.
    testDesc :: TestName,
    -- | Test function name, for creating unique file paths.
    testName :: OsPath,
    -- | Function that creates the bytes to be written to the golden file.
    -- The inputs are the tmp test directory, and the CLI's stdout.
    resultToBytes :: OsPath -> Text -> IO ByteString
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
testChart testDesc testName getTestDir = testGoldenParams getTestDir params
  where
    params =
      MkGoldenParams
        { mkArgs = \testDir ->
            [ "chart",
              "--runs",
              runsPath,
              "--chart-requests",
              chartRequestsPath,
              "--json",
              unsafeDecode (mkJsonPath testDir)
            ],
          testDesc,
          testName,
          -- NOTE: It would be nice to test the txt output here i.e. the
          -- second arg. Alas, it includes the path of the output json file,
          -- which is non-deterministic, as it includes the tmp dir.
          --
          -- Thus for now we ignore it, since the json output is the main
          -- part we care about.
          resultToBytes = \path _ -> readBinaryFileIO . mkJsonPath $ path
        }

    basePath = [ospPathSep|test/functional/data|]
    chartRequestsPath = unsafeDecode $ basePath </> testName <> [osp|_chart-requests.toml|]
    runsPath = unsafeDecode $ basePath </> testName <> [osp|_runs.toml|]
    mkJsonPath testDir = testDir </> testName <> [ospPathSep|_charts.json|]

testGoldenParams :: IO OsPath -> GoldenParams -> TestTree
testGoldenParams getTestDir goldenParams = do
  goldenVsFile goldenParams.testDesc goldenPath actualPath $ do
    testDir <- getTestDir
    let args = goldenParams.mkArgs testDir
    trySync (withArgs args $ runAppWith pure) >>= \case
      Left err -> writeActualFile $ exToBs err
      Right txt -> do
        bs <- goldenParams.resultToBytes testDir txt
        writeActualFile bs
  where
    outputPathStart =
      FS.OsPath.unsafeDecode
        $ [ospPathSep|test/functional/goldens|]
        </> goldenParams.testName

    exToBs = encodeUtf8 . packText . displayInnerMatchKnown

    writeActualFile :: ByteString -> IO ()
    writeActualFile =
      writeBinaryFileIO (FS.OsPath.unsafeEncode actualPath)
        . (<> "\n")

    actualPath = outputPathStart <> ".actual"
    goldenPath = outputPathStart <> ".golden"
