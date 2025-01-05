{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Prelude
  ( module X,

    -- * Hedgehog
    annotateUnpack,
    hdiff,
    testProp,
    testProp1,

    -- * HUnit
    (@/=?),
    assertLeft,

    -- * Golden
    GoldenParams (..),
    testGoldenParams,

    -- * Parsing
    parseOrDie,
    parseOrDieM,
    parseOrDieM_,

    -- * Constructors
    mkDistanceD,
    mkDistancePD,
    mkSomeDistanceD,
    mkSomeDistancePD,
    mkDurationD,
    mkDurationPD,
    mkSomeDurationD,
    mkSomeDurationPD,
    mkPaceD,
    mkPacePD,
    mkSomePaceD,
    mkSomePacePD,

    -- * Misc
    pShowBS,
  )
where

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
import Pacer.Data.Distance
  ( Distance (MkDistance),
    SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance.Units (SDistanceUnit)
import Pacer.Data.Duration
  ( Duration (MkDuration),
    SomeDuration (MkSomeDuration),
  )
import Pacer.Data.Duration.Units (STimeUnit)
import Pacer.Data.Pace (Pace (MkPace), PaceDistF, SomePace (MkSomePace))
import Pacer.Prelude as X hiding (IO)
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

assertLeft :: (Show b) => List Char -> Either a b -> Assertion
assertLeft _ (Left _) = pure ()
assertLeft s (Right x) = assertFailure err
  where
    err =
      mconcat
        [ s,
          ": Expected Left, received Right '",
          show x,
          "'"
        ]

parseOrDie :: (HasCallStack, Parser a) => Text -> a
parseOrDie = errorMapLeft unpackText . Parser.parse

parseOrDieM :: forall a m. (MonadFail m, Parser a) => Text -> m a
parseOrDieM t = do
  case Parser.parse t of
    Right r -> pure r
    Left err -> fail (unpackText err)

parseOrDieM_ :: forall a m. (MonadFail m, Parser a) => Text -> m ()
parseOrDieM_ = void . parseOrDieM @a

hdiff :: (Show a, Show b) => a -> (a -> b -> Bool) -> b -> PropertyT IO ()
hdiff = H.diff

mkDistanceD :: forall t. Double -> Distance t Double
mkDistanceD = MkDistance

mkDistancePD :: forall t. Double -> Distance t PDouble
mkDistancePD = MkDistance . unsafePositive

mkSomeDistanceD :: SDistanceUnit d -> Double -> SomeDistance Double
mkSomeDistanceD s = MkSomeDistance s . mkDistanceD

mkSomeDistancePD :: SDistanceUnit d -> Double -> SomeDistance PDouble
mkSomeDistancePD s = MkSomeDistance s . mkDistancePD

mkDurationD :: forall t. Double -> Duration t Double
mkDurationD = MkDuration

mkDurationPD :: forall t. Double -> Duration t PDouble
mkDurationPD = MkDuration . unsafePositive

mkSomeDurationD :: STimeUnit s -> Double -> SomeDuration Double
mkSomeDurationD s = MkSomeDuration s . mkDurationD

mkSomeDurationPD :: STimeUnit s -> Double -> SomeDuration PDouble
mkSomeDurationPD s = MkSomeDuration s . mkDurationPD

mkPaceD :: forall d. (PaceDistF d) => Double -> Pace d Double
mkPaceD = MkPace . MkDuration

mkPacePD :: forall d. (PaceDistF d) => Double -> Pace d PDouble
mkPacePD = MkPace . MkDuration . unsafePositive

mkSomePaceD :: forall d. (PaceDistF d) => SDistanceUnit d -> Double -> SomePace Double
mkSomePaceD s = MkSomePace s . mkPaceD

mkSomePacePD :: forall d. (PaceDistF d) => SDistanceUnit d -> Double -> SomePace PDouble
mkSomePacePD s = MkSomePace s . mkPacePD

data GoldenParams = MkGoldenParams
  { testDesc :: TestName,
    testName :: OsPath,
    runner :: IO ByteString
  }

testGoldenParams :: GoldenParams -> TestTree
testGoldenParams goldenParams =
  goldenVsFile goldenParams.testDesc goldenPath actualPath $ do
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
      writeBinaryFile (FS.OsPath.unsafeEncode actualPath)
        . (<> "\n")

    actualPath = outputPathStart <> ".actual"
    goldenPath = outputPathStart <> ".golden"

-- Note that an alternative to pretty-simple would be to use aeson's
-- ToJSON and print that. Of course that requires ToJSON instances.
pShowBS :: (Show a) => a -> ByteString
pShowBS = encodeUtf8 . toStrictText . Pretty.pShowNoColor
