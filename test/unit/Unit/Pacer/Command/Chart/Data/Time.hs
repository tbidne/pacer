module Unit.Pacer.Command.Chart.Data.Time (tests) where

import Data.Enum (Enum (toEnum))
import Data.Text qualified as T
import Data.Time
  ( Day (ModifiedJulianDay),
    LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    TimeZone,
    ZonedTime (ZonedTime),
  )
import Data.Time.Zones qualified as TZ
import Data.Time.Zones.All (TZLabel)
import Data.Time.Zones.All qualified as TZ.All
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Time
  ( Moment (MomentMonth, MomentTimestamp, MomentYear),
    Month,
    Timestamp (TimestampDate, TimestampTime, TimestampZoned),
    Year,
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Time"
    [ timestampTests,
      momentTests
    ]

-- TODO: Consider roundtrip tests, if we implement any non-Show toString
-- functions.

timestampTests :: TestTree
timestampTests =
  testGroup
    "Timestamp"
    [ testParseTimestampProp,
      testParseTimestampCases,
      testTimestampEqLaws,
      testTimestampOrdLaws
    ]

testParseTimestampProp :: TestTree
testParseTimestampProp = testPropertyNamed name desc $ property $ do
  txt <- forAll genTimestampTxt
  case P.parse @Timestamp txt of
    Right _ -> pure ()
    Left err -> do
      annotate (unpackText err)
      failure
  where
    name = "testParseTimestamp"
    desc = "Parses text"

testParseTimestampCases :: TestTree
testParseTimestampCases = testCase "Parses cases" $ do
  assertTime "1950-01-01T00:00:00"
  where
    assertTime t = case P.parse @Timestamp t of
      Left err ->
        assertFailure $ "Failed parsing time: " ++ unpackText err
      Right (TimestampTime _) -> pure ()
      Right bad ->
        assertFailure $ "Expected time, received: " ++ show bad

testTimestampEqLaws :: TestTree
testTimestampEqLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genTimestamp
  y <- forAll genTimestamp
  -- TODO: We should re-enable these tests once we make the Timestamp/Moment
  -- instances lawful. We also want _some_ law tests for the hypothetical
  --
  --   ovelaps :: Timestamp -> Timestamp -> Bool
  --   satisfies :: (forall x. Ord x => x -> x -> Bool) -> Moment -> Timestamp -> Bool
  --
  -- Though we cannot have all laws e.g. transitivity.
  --
  -- z <- forAll genTimestamp

  -- reflexivity
  x === x

  -- symmetry
  (x == y) === (y == x)

  -- transitivity
  -- when (x == y && y == z) (x === z)

  -- negation
  (x /= y) === not (x == y)
  where
    name = "testTimestampEqLaws"
    desc = "Eq laws"

testTimestampOrdLaws :: TestTree
testTimestampOrdLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genTimestamp
  y <- forAll genTimestamp
  -- z <- forAll genTimestamp

  -- comparability
  let xLteY = x <= y
      yLteX = y <= x

  assert (xLteY || yLteX)

  -- transitivity
  -- let yLteZ = y <= z
  -- when (xLteY && yLteZ) $ assert (x <= z)

  -- reflexivity
  assert (x <= x)

  -- antisymmetry
  when (xLteY && yLteX) $ assert (x == y)
  where
    name = "testTimestampOrdLaws"
    desc = "Ord laws"

momentTests :: TestTree
momentTests =
  testGroup
    "Moment"
    [ testParseMomentProp,
      testMomentParseCases,
      testMomentEqTotal,
      testMomentEqLaws,
      testMomentEqCases,
      testMomentOrdTotal,
      testMomentOrdLaws,
      testMomentOrdCases
    ]

testParseMomentProp :: TestTree
testParseMomentProp = testPropertyNamed name desc $ property $ do
  txt <- forAll genMomentTxt
  case P.parse @Moment txt of
    Right _ -> pure ()
    Left err -> do
      annotate (unpackText err)
      failure
  where
    name = "testParseMomentProp"
    desc = "Parses text"

testMomentParseCases :: TestTree
testMomentParseCases = testCase desc $ do
  isYear "2024"
  isMonth "2023-12"
  isTimestamp "2020-06-01"
  isTimestamp "2021-08-21"
  isTimestamp "2021-08-21T06:23:46"
  isTimestamp "2021-08-21T06:23:46+0800"
  where
    desc = "Parses test cases"

    isYear t = case P.parse @Moment t of
      Right (MomentYear _) -> pure ()
      Right other ->
        assertFailure $ "Expected year, received: " ++ show other
      Left err ->
        assertFailure $ "Unexpected parse failure: " ++ unpackText err

    isMonth t = case P.parse @Moment t of
      Right (MomentMonth _ _) -> pure ()
      Right other ->
        assertFailure $ "Expected year/month, received: " ++ show other
      Left err ->
        assertFailure $ "Unexpected parse failure: " ++ unpackText err

    isTimestamp t = case P.parse @Moment t of
      Right (MomentTimestamp _) -> pure ()
      Right other ->
        assertFailure $ "Expected year/month, received: " ++ show other
      Left err ->
        assertFailure $ "Unexpected parse failure: " ++ unpackText err

testMomentEqTotal :: TestTree
testMomentEqTotal = testPropertyNamed name desc $ property $ do
  m1 <- forAll genMoment
  m2 <- forAll genMoment
  evalF (==) m1 m2
  where
    name = "testMomentEqTotal"
    desc = "Eq is total"

testMomentEqLaws :: TestTree
testMomentEqLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genMoment
  y <- forAll genMoment
  -- z <- forAll genMoment

  -- reflexivity
  x === x

  -- symmetry
  (x == y) === (y == x)

  -- transitivity
  -- when (x == y && y == z) (x === z)

  -- negation
  (x /= y) === not (x == y)
  where
    name = "testMomentEqLaws"
    desc = "Eq laws"

testMomentEqCases :: TestTree
testMomentEqCases = testProp1 "testMomentEqCases" desc $ do
  -- Year / Year
  go "2023" (/=) "2022"
  go "2023" (==) "2023"
  go "2023" (/=) "2024"

  -- Year / Month
  go "2023" (/=) "2022-12"
  go "2023" (==) "2023-12"
  go "2023" (/=) "2024-12"

  -- Year / Day
  go "2023" (/=) "2022-12-08"
  go "2023" (==) "2023-12-08"
  go "2023" (/=) "2024-12-08"

  -- Year / LocalTime
  go "2023" (/=) "2022-12-08T14:13:20"
  go "2023" (==) "2023-12-08T14:13:20"
  go "2023" (/=) "2024-12-08T14:13:20"

  -- Year / Zoned
  go "2023" (/=) "2022-12-08T14:13:20+0800"
  go "2023" (==) "2023-12-08T14:13:20+0800"
  go "2023" (/=) "2024-12-08T14:13:20+0800"

  -- Month / Month
  go "2023-12" (/=) "2023-11"
  go "2023-12" (==) "2023-12"
  go "2023-12" (/=) "2024-01"

  -- Month / Day
  go "2023-12" (/=) "2023-11-08"
  go "2023-12" (==) "2023-12-08"
  go "2023-12" (/=) "2024-01-08"

  -- Month / LocalTime
  go "2023-12" (/=) "2023-11-08T14:13:20"
  go "2023-12" (==) "2023-12-08T14:13:20"
  go "2023-12" (/=) "2024-01-08T14:13:20"

  -- Month / Zoned
  go "2023-12" (/=) "2023-11-08T14:13:20+0800"
  go "2023-12" (==) "2023-12-08T14:13:20+0800"
  go "2023-12" (/=) "2024-01-08T14:13:20+0800"
  where
    desc = "Eq cases"

    go x op y =
      H.diff (unsafeParse @Moment x) op (unsafeParse y)

testMomentOrdTotal :: TestTree
testMomentOrdTotal = testPropertyNamed name desc $ property $ do
  m1 <- forAll genMoment
  m2 <- forAll genMoment
  evalF (<=) m1 m2
  where
    name = "testMomentOrdTotal"
    desc = "Ord is total"

testMomentOrdLaws :: TestTree
testMomentOrdLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genMoment
  y <- forAll genMoment
  -- z <- forAll genMoment

  -- comparability
  let xLteY = x <= y
      yLteX = y <= x

  assert (xLteY || yLteX)

  -- transitivity
  -- let yLteZ = y <= z
  -- when (xLteY && yLteZ) $ assert (x <= z)

  -- reflexivity
  assert (x <= x)

  -- antisymmetry
  when (xLteY && yLteX) $ assert (x == y)
  where
    name = "testMomentOrdLaws"
    desc = "Ord laws"

testMomentOrdCases :: TestTree
testMomentOrdCases = testProp1 "testMomentOrdCases" desc $ do
  go "2023" (<=) "2023"
  go "2023" (<=) "2024"
  go "2023" (<) "2024"
  go "2023" (<) "2024"
  go "2020-06-01" (<) "2023"
  go "2020-06-01" (<=) "2023"
  go "2023" (>) "2020-06-01"
  go "2020-06-01" (\x y -> not (x > y)) "2023"
  where
    desc = "Ord cases"

    go x op y =
      H.diff (unsafeParse @Moment x) op (unsafeParse y)

evalF :: (NFData c) => (a -> b -> c) -> a -> b -> PropertyT IO ()
evalF f x y = void $ H.evalNF (x `f` y)

genMoment :: Gen Moment
genMoment =
  Gen.choice
    [ MomentYear <$> genYear,
      MomentMonth <$> genYear <*> genMonth,
      MomentTimestamp <$> genTimestamp
    ]

genYear :: Gen Year
genYear = Gen.enumBounded

genMonth :: Gen Month
genMonth = Gen.enumBounded

genTimestamp :: Gen Timestamp
genTimestamp =
  Gen.choice
    [ TimestampDate <$> genDay,
      TimestampTime <$> genLocalTime,
      TimestampZoned <$> genZonedTime
    ]

genZonedTime :: Gen ZonedTime
genZonedTime = do
  lt <- genLocalTime
  tz <- genTimeZone
  pure $ ZonedTime lt tz

genTimeZone :: Gen TimeZone
genTimeZone =
  (\l -> TZ.timeZoneForPOSIX (TZ.All.tzByLabel l) 0)
    <$> tzLabels
  where
    tzLabels :: Gen TZLabel
    tzLabels = Gen.enumBounded

genLocalTime :: Gen LocalTime
genLocalTime = do
  d <- genDay
  tod <- genTimeOfDay
  pure $ LocalTime d tod

genDay :: Gen Day
genDay =
  Gen.element
    [ ModifiedJulianDay 33282, -- 1950-01-01
      ModifiedJulianDay 88068 -- 2099-12-31
    ]

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = do
  h <- Gen.integral (Range.linear 0 23)
  m <- Gen.integral (Range.linear 0 59)
  s <- Gen.integral (Range.linear 0 60) -- 60 == leap second
  -- REVIEW: Is toEnum legit here?
  pure $ TimeOfDay h m (toEnum s)

genMomentTxt :: Gen Text
genMomentTxt = do
  Gen.choice
    [ genYearTxt,
      genYearMonth,
      genTimestampTxt
    ]
  where
    genYearMonth =
      (\y m -> y <> "-" <> m) <$> genYearTxt <*> genMonthTxt

genTimestampTxt :: Gen Text
genTimestampTxt = do
  Gen.choice
    [ genDateTxt,
      genLocalTimeTxt,
      genZonedTxt
    ]

genZonedTxt :: Gen Text
genZonedTxt = do
  lt <- genLocalTimeTxt
  tz <- genTzOffsetTxt
  pure $ lt <> tz

genLocalTimeTxt :: Gen Text
genLocalTimeTxt = do
  dateTxt <- genDateTxt
  timeTxt <- genTimeTxt
  pure $ (dateTxt <> "T" <> timeTxt)

genTzOffsetTxt :: Gen Text
genTzOffsetTxt = do
  h <- Gen.integral @_ @Word8 (Range.linear 0 23)
  m <- Gen.integral @_ @Word8 (Range.linear 0 59)
  d <- Gen.element ['+', '-']
  pure
    $ mconcat
      [ T.singleton d,
        showtPad2 h,
        showtPad2 m
      ]

genTimeTxt :: Gen Text
genTimeTxt = do
  h <- Gen.integral @_ @Word8 (Range.linear 0 23)
  m <- Gen.integral @_ @Word8 (Range.linear 0 59)
  s <- Gen.integral @_ @Word8 (Range.linear 0 59)
  pure $ T.intercalate ":" (showtPad2 <$> [h, m, s])

genDateTxt :: Gen Text
genDateTxt = do
  y <- genYearTxt
  m <- genMonthTxt
  d <- genDayTxt m
  pure $ T.intercalate "-" [y, m, d]

genYearTxt :: Gen Text
genYearTxt = do
  y <- Gen.integral @_ @Word16 (Range.linear 1950 2099)
  pure $ showt y

genMonthTxt :: Gen Text
genMonthTxt = do
  y <- Gen.integral @_ @Word8 (Range.linear 1 12)
  pure $ showtPad2 y

genDayTxt :: Text -> Gen Text
genDayTxt monthTxt = do
  let maxN = maxDay monthTxt
  y <- Gen.integral @_ @Word8 (Range.linear 1 maxN)
  pure $ showtPad2 y
  where
    maxDay "01" = 31
    maxDay "02" = 28
    maxDay "03" = 31
    maxDay "04" = 30
    maxDay "05" = 31
    maxDay "06" = 30
    maxDay "07" = 31
    maxDay "08" = 31
    maxDay "09" = 30
    maxDay "10" = 31
    maxDay "11" = 30
    maxDay "12" = 31
    maxDay d = error $ "Unexpected day: " ++ unpackText d

showtPad2 :: (Show a) => a -> Text
showtPad2 = pad2 . showt

pad2 :: Text -> Text
pad2 t =
  if T.length t == 1
    then T.singleton '0' <> t
    else t

unsafeParse :: (HasCallStack) => (P.Parser a) => Text -> a
unsafeParse @a = errorMapLeft unpackText . P.parse @a
