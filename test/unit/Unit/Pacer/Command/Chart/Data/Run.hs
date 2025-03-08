{-# LANGUAGE QuasiQuotes #-}

module Unit.Pacer.Command.Chart.Data.Run (tests) where

import Data.Text qualified as T
import FileSystem.IO (readBinaryFileIO)
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Run (Run (MkRun), SomeRun, SomeRuns)
import Pacer.Command.Chart.Data.Run qualified as R
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
import Pacer.Data.Distance (Distance (MkDistance), DistanceUnit (Kilometer))
import Pacer.Data.Distance qualified as D
import Pacer.Data.Duration (Duration (MkDuration))
import Pacer.Utils qualified as Utils
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Run"
    [ testParseExampleRunsJson,
      mkTests
    ]

testParseExampleRunsJson :: TestTree
testParseExampleRunsJson = testGoldenParams params
  where
    params =
      MkGoldenParams
        { testDesc = "Parses example runs.jsonc",
          testName = [osp|testParseExampleRunsJson|],
          runner = do
            contents <- readBinaryFileIO path
            case Utils.decodeJson @(SomeRuns Double) contents of
              Ok result -> pure $ pShowBS result
              Err err -> throwM err
        }
    path = [ospPathSep|examples/runs.jsonc|]

mkTests :: TestTree
mkTests =
  testGroup
    "mkSomeRuns"
    [ testMkSomeRunsSuccess,
      testMkSomeRunsDuplicateDate,
      testMkSomeRunsDuplicateTime,
      testMkSomeRunsDuplicateZoned,
      testMkSomeRunsDuplicateZonedConvert,
      testMkSomeRunsDateTimeOverlap,
      testMkSomeRunsTimeDateOverlap,
      testMkSomeRunsDateZonedOverlap,
      testMkSomeRunsZonedDateOverlap,
      testMkSomeRunsTimeZonedOverlap,
      testMkSomeRunsZonedTimeOverlap
    ]

testMkSomeRunsSuccess :: TestTree
testMkSomeRunsSuccess = testCase "Successfully creates SomeRuns" $ do
  case R.mkSomeRuns runs of
    Err err -> assertFailure $ displayException err
    Ok result -> expected @=? R.someRunsToList result
  where
    runs = r1 :| [r2, r3, r4, r5, r6]
    expected = [r5, r6, r2, r1, r3, r4]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-09"
    r3 = mkSr "r3" "2024-08-12T14:30:00"
    r4 = mkSr "r4" "2024-08-12T14:35:00"
    r5 = mkSr "r5" "2024-08-07T10:20:00-0800"
    r6 = mkSr "r6" "2024-08-07T10:20:00-0900"

testMkSomeRunsDuplicateDate :: TestTree
testMkSomeRunsDuplicateDate = testCase "Fails for duplicate date" $ do
  case R.mkSomeRuns runs of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    runs = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10",
          " - r2: 2024-08-10"
        ]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-10"

testMkSomeRunsDuplicateTime :: TestTree
testMkSomeRunsDuplicateTime = testCase "Fails for duplicate time" $ do
  case R.mkSomeRuns runs of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    runs = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10T12:15:30",
          " - r2: 2024-08-10T12:15:30"
        ]

    r1 = mkSr "r1" "2024-08-10T12:15:30"
    r2 = mkSr "r2" "2024-08-10T12:15:30"

testMkSomeRunsDuplicateZoned :: TestTree
testMkSomeRunsDuplicateZoned = testCase "Fails for duplicate zoned time" $ do
  case R.mkSomeRuns runs of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    runs = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10T12:15:30-0800",
          " - r2: 2024-08-10T12:15:30-0800"
        ]

    r1 = mkSr "r1" "2024-08-10T12:15:30-0800"
    r2 = mkSr "r2" "2024-08-10T12:15:30-0800"

testMkSomeRunsDuplicateZonedConvert :: TestTree
testMkSomeRunsDuplicateZonedConvert = testCase desc $ do
  case R.mkSomeRuns runs of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    desc = "Fails for duplicate converted zoned time"
    runs = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10T13:15:30-0700",
          " - r2: 2024-08-10T12:15:30-0800"
        ]

    r1 = mkSr "r1" "2024-08-10T13:15:30-0700"
    r2 = mkSr "r2" "2024-08-10T12:15:30-0800"

testMkSomeRunsDateTimeOverlap :: TestTree
testMkSomeRunsDateTimeOverlap = testCase "Fails for date time overlap" $ do
  case R.mkSomeRuns runs of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    runs = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10",
          " - r2: 2024-08-10T12:14:00"
        ]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-10T12:14:00"

-- Same as testMkSomeRunsDateTimeOverlap but with the order swapped. We want
-- success/failure to be irrespective of order, so this is not
-- necessarily trivial.
testMkSomeRunsTimeDateOverlap :: TestTree
testMkSomeRunsTimeDateOverlap = testCase "Fails for time date overlap" $ do
  case R.mkSomeRuns runs of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    runs = r2 :| [r1]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10",
          " - r2: 2024-08-10T12:14:00"
        ]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-10T12:14:00"

testMkSomeRunsDateZonedOverlap :: TestTree
testMkSomeRunsDateZonedOverlap = testCase "Fails for date zoned overlap" $ do
  case R.mkSomeRuns runs of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    runs = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10",
          " - r2: 2024-08-10T12:14:00-0800"
        ]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-10T12:14:00-0800"

testMkSomeRunsZonedDateOverlap :: TestTree
testMkSomeRunsZonedDateOverlap = testCase "Fails for zoned date overlap" $ do
  case R.mkSomeRuns runs of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    runs = r2 :| [r1]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10",
          " - r2: 2024-08-10T12:14:00-0800"
        ]

    r1 = mkSr "r1" "2024-08-10"
    r2 = mkSr "r2" "2024-08-10T12:14:00-0800"

testMkSomeRunsTimeZonedOverlap :: TestTree
testMkSomeRunsTimeZonedOverlap = testCase "Fails for time zoned overlap" $ do
  case R.mkSomeRuns runs of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    runs = r1 :| [r2]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10T12:14:00",
          " - r2: 2024-08-10T12:14:00-0800"
        ]

    r1 = mkSr "r1" "2024-08-10T12:14:00"
    r2 = mkSr "r2" "2024-08-10T12:14:00-0800"

testMkSomeRunsZonedTimeOverlap :: TestTree
testMkSomeRunsZonedTimeOverlap = testCase "Fails for zoned time overlap" $ do
  case R.mkSomeRuns runs of
    Err err -> expected @=? displayExceptiont err
    Ok result ->
      assertFailure $ "Expected failure, received: " ++ show result
  where
    runs = r2 :| [r1]
    expected =
      T.intercalate
        "\n"
        [ "Found overlapping timestamps",
          " - r1: 2024-08-10T12:14:00",
          " - r2: 2024-08-10T12:14:00-0800"
        ]

    r1 = mkSr "r1" "2024-08-10T12:14:00"
    r2 = mkSr "r2" "2024-08-10T12:14:00-0800"

mkSr :: Text -> Text -> SomeRun Double
mkSr title ts =
  D.hideDistance
    $ MkRun
      { datetime = unsafeTs ts,
        distance = MkDistance @Kilometer (fromℤ 5),
        duration = MkDuration (fromℤ 1200),
        labels = mempty,
        title = Just title
      }
  where
    unsafeTs :: Text -> Timestamp
    unsafeTs = errorErr . P.parseAll
