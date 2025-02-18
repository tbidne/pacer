{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Prelude
  ( module X,

    -- * Hedgehog
    (.===),
    (./==),
    annotateUnpack,
    hdiff,
    testProp,
    testProp1,

    -- * HUnit
    (@/=?),
    assertErr,

    -- * Golden
    GoldenParams (..),
    testGoldenParams,
    testGoldenParamsOs,

    -- * Parsing
    parseOrDie,
    parseOrDieM,
    parseOrDieM_,

    -- * Constructors
    mkDistanceD,
    mkSomeDistanceD,
    mkDurationD,
    mkPaceD,
    mkSomePaceD,

    -- * Misc
    pShowBS,
  )
where

import FileSystem.IO (writeBinaryFileIO)
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
import Hedgehog qualified as H
import Pacer.Class.Parser (Parser)
import Pacer.Class.Parser qualified as Parser
import Pacer.Command.Chart.Data.Time (Moment, (./=), (.==))
import Pacer.Data.Distance
  ( Distance (MkDistance),
    SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance.Units (SDistanceUnit)
import Pacer.Data.Duration (Duration (MkDuration))
import Pacer.Data.Pace (Pace (MkPace), PaceDistF, SomePace (MkSomePace))
import Pacer.Prelude as X hiding (IO)
import System.FilePath (FilePath)
import System.IO as X (IO)
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
import Text.Pretty.Simple qualified as Pretty

-- | Concise alias for @testPropertyNamed . property@
testProp :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp name desc = testPropertyNamed name desc . property

-- | 'testProp' that only runs a single test. Used for when we'd really want
-- HUnit's testCase, but with a better diff.
testProp1 :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp1 name desc = testPropertyNamed name desc . withTests 1 . property

annotateUnpack :: Text -> PropertyT IO ()
annotateUnpack = annotate . unpackText

(.===) :: Moment -> Moment -> PropertyT IO ()
x .=== y = H.diff x (.==) y

infix 4 .===

(./==) :: Moment -> Moment -> PropertyT IO ()
x ./== y = H.diff x (./=) y

infix 4 ./==

(@/=?) :: (Eq a, Show a) => a -> a -> Assertion
x @/=? y = assertBool msg (x /= y)
  where
    msg =
      mconcat
        [ show x,
          " == ",
          show y
        ]

infix 1 @/=?

assertErr :: (Show a) => List Char -> Result e a -> Assertion
assertErr _ (Err _) = pure ()
assertErr s (Ok x) = assertFailure err
  where
    err =
      mconcat
        [ s,
          ": Expected Err, received Ok '",
          show x,
          "'"
        ]

parseOrDie :: (HasCallStack, Parser a) => Text -> a
parseOrDie = errorErr . Parser.parse

parseOrDieM :: forall a m. (MonadFail m, Parser a) => Text -> m a
parseOrDieM t = do
  case Parser.parse t of
    Ok r -> pure r
    Err err -> fail err

parseOrDieM_ :: forall a m. (MonadFail m, Parser a) => Text -> m ()
parseOrDieM_ = void . parseOrDieM @a

hdiff :: (Show a, Show b) => a -> (a -> b -> Bool) -> b -> PropertyT IO ()
hdiff = H.diff

mkDistanceD :: forall t. Double -> Distance t Double
mkDistanceD = MkDistance . unsafePositive

mkSomeDistanceD :: SDistanceUnit d -> Double -> SomeDistance Double
mkSomeDistanceD s = MkSomeDistance s . mkDistanceD

mkDurationD :: Double -> Duration Double
mkDurationD = MkDuration . unsafePositive

mkPaceD :: forall d. (PaceDistF d) => Double -> Pace d Double
mkPaceD = MkPace . MkDuration . unsafePositive

mkSomePaceD :: forall d. (PaceDistF d) => SDistanceUnit d -> Double -> SomePace Double
mkSomePaceD s = MkSomePace s . mkPaceD

data GoldenParams = MkGoldenParams
  { testDesc :: TestName,
    testName :: OsPath,
    runner :: IO ByteString
  }

testGoldenParamsOs :: GoldenParams -> TestTree
testGoldenParamsOs goldenParams = testGoldenParams goldenParams'
  where
    goldenParams' =
      goldenParams
        { testName = goldenParams.testName <> ([osp|_|] <> posixWindowsOsPath)
        }

testGoldenParams :: GoldenParams -> TestTree
testGoldenParams goldenParams =
  goldenDiff goldenParams.testDesc goldenPath actualPath $ do
    trySync goldenParams.runner >>= \case
      Left err -> writeActualFile $ exToBs err
      Right bs -> writeActualFile bs
  where
    outputPathStart =
      FS.OsPath.unsafeDecode
        $ [ospPathSep|test/unit/goldens|]
        </> goldenParams.testName

    exToBs = encodeUtf8 . displayExceptiont

    writeActualFile :: ByteString -> IO ()
    writeActualFile =
      writeBinaryFileIO (FS.OsPath.unsafeEncode actualPath)
        . (<> "\n")

    actualPath = outputPathStart <> ".actual"
    goldenPath = outputPathStart <> ".golden"

-- Note that an alternative to pretty-simple would be to use aeson's
-- ToJSON and print that. Of course that requires ToJSON instances.
pShowBS :: (Show a) => a -> ByteString
pShowBS = encodeUtf8 . toStrictText . Pretty.pShowOpt opts
  where
    opts =
      Pretty.defaultOutputOptionsNoColor
        { Pretty.outputOptionsIndentAmount = 2
        }

-- See NOTE: [Golden test diffing]
goldenDiff :: TestName -> FilePath -> FilePath -> IO () -> TestTree
goldenDiff x = goldenVsFileDiff x diffArgs
  where
    diffArgs ref new =
      [ "git",
        "diff",
        "--exit-code",
        "--color=always",
        "--no-index",
        ref,
        new
      ]
