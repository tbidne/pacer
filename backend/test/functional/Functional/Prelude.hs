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
import Pacer.Chart qualified as Chart
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

data GoldenParams = MkGoldenParams
  { testDesc :: TestName,
    testName :: OsPath,
    runner :: IO ByteString
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
testChart :: TestName -> OsPath -> TestTree
testChart desc testName = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = desc,
          testName,
          runner =
            toStrictByteString
              <$> Chart.createChartsJsonBS
                (Just runsPath)
                (Just chartRequestsPath)
        }
    basePath = [ospPathSep|test/functional/data|]
    chartRequestsPath = basePath </> testName <> [osp|_chart-requests.toml|]
    runsPath = basePath </> testName <> [osp|_runs.toml|]

testGoldenParams :: GoldenParams -> TestTree
testGoldenParams goldenParams =
  goldenVsFile goldenParams.testDesc goldenPath actualPath $ do
    bs <- goldenParams.runner
    writeActualFile bs
  where
    outputPathStart =
      FS.OsPath.unsafeDecode
        $ [ospPathSep|test/functional/goldens|]
        </> goldenParams.testName

    writeActualFile :: ByteString -> IO ()
    writeActualFile = writeBinaryFileIO (FS.OsPath.unsafeEncode actualPath)

    actualPath = outputPathStart <> ".actual"
    goldenPath = outputPathStart <> ".golden"
