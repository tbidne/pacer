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
    testPropMaxN,
    testPropN,

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
    TestLimit,
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
import Test.Tasty as X (TestName, TestTree, askOption, localOption, testGroup)
import Test.Tasty.Golden as X (goldenVsFileDiff)
import Test.Tasty.HUnit as X
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog as X (HedgehogTestLimit (HedgehogTestLimit), testPropertyNamed)
import Text.Pretty.Simple qualified as Pretty

-- | Concise alias for @testPropertyNamed . property@
testProp :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp name desc = testPropertyNamed name desc . property

-- | 'testProp' that only runs a single test. Used for when we'd really want
-- HUnit's testCase, but with a better diff.
testProp1 :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp1 name desc = testPropN 1 name desc

-- | If a limit option exists (i.e. either cli --hedgehog-tests or manually
-- setting HedgehogTestLimit), then we take the minimum of limit and the
-- given maxLimit. This allows the following behavior:
--
-- - If a low limit is given (e.g. because we want tests to be fast), use it.
-- - If a higher limit is given, use the maxLimit, so we may end up running
--   more tests than usual, but not so many that the test takes forever.
--
-- Providing no limit means we use the default (100), which is hopefully a
-- reasonable mix of speed / robustness.
testPropMaxN ::
  TestLimit ->
  TestName ->
  PropertyName ->
  PropertyT IO () ->
  TestTree
testPropMaxN maxLimit name desc p =
  askOption $ \(HedgehogTestLimit mLimit) ->
    case mLimit of
      -- If no CLI arg is passed, use the default limit.
      Nothing -> testPropN 100 name desc p
      -- If a CLI arg is passed, take the min.
      Just cliLimit -> testPropN (min cliLimit maxLimit) name desc p

-- | 'testProp' that runs for specified N times.
testPropN :: TestLimit -> TestName -> PropertyName -> PropertyT IO () -> TestTree
testPropN numTests name desc =
  -- NOTE: Have to use localOption here as it overrides withTests. That is,
  -- hedgehog's withTests has NO effect here, so don't use it!
  localOption (HedgehogTestLimit (Just numTests))
    . testProp name desc

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
