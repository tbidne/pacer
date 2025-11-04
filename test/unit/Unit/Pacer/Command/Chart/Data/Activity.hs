{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.Activity (tests) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Activity
  ( Activity (MkActivity),
    SomeActivity (MkSomeActivity),
  )
import Pacer.Command.Chart.Data.Activity qualified as Activity
import Pacer.Command.Chart.Data.Activity.ActivityLabel (Label (MkLabel))
import Pacer.Command.Chart.Data.Activity.ActivityType
  ( ActivityType (MkActivityType),
  )
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
import Pacer.Configuration.Env.Types (LogEnv, runLoggerMock, runReaderLogEnvMock)
import Pacer.Data.Distance
  ( Distance (MkDistance),
    DistanceUnit (Kilometer, Meter, Mile),
  )
import Pacer.Data.Distance qualified as D
import Pacer.Data.Distance.Units (SDistanceUnit (SKilometer, SMeter, SMile))
import Pacer.Data.Duration (Duration (MkDuration))
import Pacer.Data.Result qualified as Result
import Pacer.Utils.Json qualified as Json
import Unit.Pacer.Data.Distance.Units qualified as Dist
import Unit.Prelude
import Unit.TestUtils qualified as UT

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Activity"
    [ testParseExampleActivitiesJson,
      testParseSomeActivityRoundtrip,
      testParseBadDateError,
      mkTests
    ]

testParseExampleActivitiesJson :: TestTree
testParseExampleActivitiesJson = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = "Parses example activities.jsonc",
          testName = [osp|testParseExampleActivitiesJson|],
          runner = do
            -- Getting the current dir since we are using readActivitiesJson,
            -- which requires an absolute path. Alternatively, we could
            -- relax it to allow relative paths.
            cwd <- runnerEff getCurrentDirectory

            results <- runnerEff (Activity.readActivitiesJson @LogEnv [] $ cwd <</>> path)
            pure $ pShowBS results
        }
    path = [relfilePathSep|examples/activities.jsonc|]

testParseBadDateError :: TestTree
testParseBadDateError = testCase desc $ do
  -- Verifies that the error message includes the field name. Still could
  -- probably be better.
  "Error in $.datetime: empty" @=? p bs
  where
    desc = "Tests bad date parse error"

    bs =
      mconcat
        [ "{",
          "\"activities\": [{",
          "\"datetime\": \"bad_date\"",
          "}]}"
        ]

    p :: ByteString -> Text
    p =
      -- Expecting exactly one error.
      Result.onResult
        displayExceptiont
        ( \case
            [Err e] -> packText e
            other -> error $ "Unexpected: " ++ show other
        )
        . Json.decodeJsonP (Activity.parseSomeActivityListJson @Double [])

runnerEff :: Eff [PathReader, Reader LogEnv, Logger, FileReader, IOE] a -> IO a
runnerEff =
  runEff
    . runFileReader
    . runLoggerMock
    . runReaderLogEnvMock
    . runPathReader

testParseSomeActivityRoundtrip :: TestTree
testParseSomeActivityRoundtrip = testPropertyNamed name desc $ property $ do
  sr <- forAll genSomeActivity

  let encoded = Json.encodePretty sr
  annotateShow encoded

  let parseSomeActivity = Activity.parseSomeActivityMaybeJson []

  decoded <- case Json.decodeJsonP parseSomeActivity (toStrictBS encoded) of
    Ok (Just r) -> pure r
    Ok Nothing -> annotate "Filtered activity type" *> failure
    Err r -> annotate (displayException r) *> failure

  sr === decoded
  where
    name = "testParseSomeActivityRoundtrip"
    desc = "fromJSON . toJSON is a round trip"

mkTests :: TestTree
mkTests =
  testGroup
    "mkSomeActivities"
    [ testMkSomeActivitiesSuccess,
      testMkSomeActivitiesDuplicateDate,
      testMkSomeActivitiesDuplicateTime,
      testMkSomeActivitiesDuplicateZoned,
      testMkSomeActivitiesDuplicateZonedConvert,
      testMkSomeActivitiesDateTimeOverlap,
      testMkSomeActivitiesTimeDateOverlap,
      testMkSomeActivitiesDateZonedOverlap,
      testMkSomeActivitiesZonedDateOverlap,
      testMkSomeActivitiesTimeZonedOverlap,
      testMkSomeActivitiesZonedTimeOverlap
    ]

testMkSomeActivitiesSuccess :: TestTree
testMkSomeActivitiesSuccess = testCase "Successfully creates SomeActivities" $ do
  case Activity.mkSomeActivities activities of
    Err err -> assertFailure $ displayException err
    Ok result -> expected @=? Activity.someActivitiesToList result
  where
    activities = r1 :| [r2, r3, r4, r5, r6]
    expected = [r5, r6, r2, r1, r3, r4]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-09"
    r3 = mkSr "r3" "2024-08-12T14:30:00"
    r4 = mkSr "r4" "2024-08-12T14:35:00"
    r5 = mkSr "r5" "2024-08-07T10:20:00-0800"
    r6 = mkSr "r6" "2024-08-07T10:20:00-0900"

testMkSomeActivitiesDuplicateDate :: TestTree
testMkSomeActivitiesDuplicateDate = testCase "Fails for duplicate date" $ do
  case Activity.mkSomeActivities activities of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    activities = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10",
          " - r2: 2024-08-10"
        ]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-10"

testMkSomeActivitiesDuplicateTime :: TestTree
testMkSomeActivitiesDuplicateTime = testCase "Fails for duplicate time" $ do
  case Activity.mkSomeActivities activities of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    activities = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10T12:15:30",
          " - r2: 2024-08-10T12:15:30"
        ]

    r1 = mkSr "r1" "2024-08-10T12:15:30"
    r2 = mkSr "r2" "2024-08-10T12:15:30"

testMkSomeActivitiesDuplicateZoned :: TestTree
testMkSomeActivitiesDuplicateZoned = testCase "Fails for duplicate zoned time" $ do
  case Activity.mkSomeActivities activities of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    activities = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10T12:15:30-0800",
          " - r2: 2024-08-10T12:15:30-0800"
        ]

    r1 = mkSr "r1" "2024-08-10T12:15:30-0800"
    r2 = mkSr "r2" "2024-08-10T12:15:30-0800"

testMkSomeActivitiesDuplicateZonedConvert :: TestTree
testMkSomeActivitiesDuplicateZonedConvert = testCase desc $ do
  case Activity.mkSomeActivities activities of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    desc = "Fails for duplicate converted zoned time"
    activities = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10T13:15:30-0700",
          " - r2: 2024-08-10T12:15:30-0800"
        ]

    r1 = mkSr "r1" "2024-08-10T13:15:30-0700"
    r2 = mkSr "r2" "2024-08-10T12:15:30-0800"

testMkSomeActivitiesDateTimeOverlap :: TestTree
testMkSomeActivitiesDateTimeOverlap = testCase "Fails for date time overlap" $ do
  case Activity.mkSomeActivities activities of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    activities = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10",
          " - r2: 2024-08-10T12:14:00"
        ]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-10T12:14:00"

-- Same as testMkSomeActivitiesDateTimeOverlap but with the order swapped. We want
-- success/failure to be irrespective of order, so this is not
-- necessarily trivial.
testMkSomeActivitiesTimeDateOverlap :: TestTree
testMkSomeActivitiesTimeDateOverlap = testCase "Fails for time date overlap" $ do
  case Activity.mkSomeActivities activities of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    activities = r2 :| [r1]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10",
          " - r2: 2024-08-10T12:14:00"
        ]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-10T12:14:00"

testMkSomeActivitiesDateZonedOverlap :: TestTree
testMkSomeActivitiesDateZonedOverlap = testCase "Fails for date zoned overlap" $ do
  case Activity.mkSomeActivities activities of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    activities = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10",
          " - r2: 2024-08-10T12:14:00-0800"
        ]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-10T12:14:00-0800"

testMkSomeActivitiesZonedDateOverlap :: TestTree
testMkSomeActivitiesZonedDateOverlap = testCase "Fails for zoned date overlap" $ do
  case Activity.mkSomeActivities activities of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    activities = r2 :| [r1]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10",
          " - r2: 2024-08-10T12:14:00-0800"
        ]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-10T12:14:00-0800"

testMkSomeActivitiesTimeZonedOverlap :: TestTree
testMkSomeActivitiesTimeZonedOverlap = testCase "Fails for time zoned overlap" $ do
  case Activity.mkSomeActivities activities of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    activities = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10T12:14:00",
          " - r2: 2024-08-10T12:14:00-0800"
        ]

    r1 = mkSr "r1" "2024-08-10T12:14:00"
    r2 = mkSr "r2" "2024-08-10T12:14:00-0800"

testMkSomeActivitiesZonedTimeOverlap :: TestTree
testMkSomeActivitiesZonedTimeOverlap = testCase "Fails for zoned time overlap" $ do
  case Activity.mkSomeActivities activities of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    activities = r2 :| [r1]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10T12:14:00",
          " - r2: 2024-08-10T12:14:00-0800"
        ]

    r1 = mkSr "r1" "2024-08-10T12:14:00"
    r2 = mkSr "r2" "2024-08-10T12:14:00-0800"

mkSr :: Text -> Text -> SomeActivity Double
mkSr title ts =
  D.hideDistance
    $ MkActivity
      { atype = Nothing,
        datetime = unsafeTs ts,
        distance = MkDistance @Kilometer (fromℤ 5),
        duration = MkDuration (fromℤ 1200),
        labels = mempty,
        title = Just title
      }
  where
    unsafeTs :: Text -> Timestamp
    unsafeTs = errorErr . P.parseAll

genSomeActivity :: Gen (SomeActivity Double)
genSomeActivity = do
  distUnit <- Dist.genDistanceUnit
  case distUnit of
    Meter -> MkSomeActivity SMeter <$> genActivity
    Kilometer -> MkSomeActivity SKilometer <$> genActivity
    Mile -> MkSomeActivity SMile <$> genActivity

genActivity :: Gen (Activity d Double)
genActivity = do
  datetime <- UT.genTimestamp
  distance <- MkDistance <$> UT.genDoublePos
  -- 120 seconds, so that rounding down to 0 (e.g. 1 second) doesn't break
  -- positive check.
  duration <- MkDuration <$> UT.genDoubleMinPos 120
  labels <-
    Set.fromList
      . fmap MkLabel
      <$> G.list (R.linearFrom 0 0 5) UT.genText
  title <- G.maybe UT.genText

  atype <- fmap MkActivityType <$> G.maybe UT.genText

  pure
    $ MkActivity
      { atype,
        datetime,
        distance,
        duration,
        labels,
        title
      }
