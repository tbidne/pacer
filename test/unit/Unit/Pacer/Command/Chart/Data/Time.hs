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
import Pacer.Class.Parser (Parser)
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Time
  ( Moment (MomentMonth, MomentTimestamp, MomentYear),
    Month,
    Timestamp (TimestampDate, TimestampTime, TimestampZoned),
    Year,
    (./=),
    (.<),
    (.<=),
    (.==),
    (.>),
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Time"
    [ timestampTests,
      momentTests
    ]

timestampTests :: TestTree
timestampTests =
  testGroup
    "Timestamp"
    [ testParseTimestampProp,
      testParseTimestampCases,
      testTimestampEqLaws,
      testTimestampOrdLaws,
      testTimestampOrdCases
    ]

testParseTimestampProp :: TestTree
testParseTimestampProp = testPropertyNamed name desc $ property $ do
  txt <- forAll genTimestampTxt
  case P.parse @Timestamp txt of
    Ok _ -> pure ()
    Err err -> do
      annotate err
      failure
  where
    name = "testParseTimestamp"
    desc = "Parses text"

testParseTimestampCases :: TestTree
testParseTimestampCases = testCase "Parses cases" $ do
  assertDate "1950-01-01"
  assertTime "1950-01-01T00:00:00"
  assertTime "1950-01-01 00:00:00"
  assertZoned "1970-01-01T12:00:00-0800"
  assertZoned "1970-01-01 12:00:00-0800"
  assertZoned "1970-01-01T12:00:00 -0800"
  assertZoned "1970-01-01 12:00:00 -0800"
  where
    assertDate t = case P.parseAll @Timestamp t of
      Err err ->
        assertFailure $ "Failed parsing date: " ++ err
      Ok (TimestampDate _) -> pure ()
      Ok bad ->
        assertFailure $ "Expected date, received: " ++ show bad

    assertTime t = case P.parseAll @Timestamp t of
      Err err ->
        assertFailure $ "Failed parsing time: " ++ err
      Ok (TimestampTime _) -> pure ()
      Ok bad ->
        assertFailure $ "Expected time, received: " ++ show bad

    assertZoned t = case P.parseAll @Timestamp t of
      Err err ->
        assertFailure $ "Failed parsing zoned: " ++ err
      Ok (TimestampZoned _) -> pure ()
      Ok bad ->
        assertFailure $ "Expected zoned, received: " ++ show bad

testTimestampEqLaws :: TestTree
testTimestampEqLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genTimestamp
  y <- forAll genTimestamp
  z <- forAll genTimestamp

  -- reflexivity
  x === x

  -- symmetry
  (x == y) === (y == x)

  -- transitivity
  when (x == y && y == z) (x === z)

  -- negation
  (x /= y) === not (x == y)
  where
    name = "testTimestampEqLaws"
    desc = "Eq laws"

testTimestampOrdLaws :: TestTree
testTimestampOrdLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genTimestamp
  y <- forAll genTimestamp
  z <- forAll genTimestamp

  -- comparability
  let xLteY = x <= y
      yLteX = y <= x

  assert (xLteY || yLteX)

  -- transitivity
  let yLteZ = y <= z
  when (xLteY && yLteZ) $ assert (x <= z)

  -- reflexivity
  assert (x <= x)

  -- antisymmetry
  when (xLteY && yLteX) $ assert (x == y)
  where
    name = "testTimestampOrdLaws"
    desc = "Ord laws"

testTimestampOrdCases :: TestTree
testTimestampOrdCases = testProp1 "testTimestampOrdCases" desc $ do
  go
    [ ("2099-12-31T02:00:00+0800", (<=), "2099-12-31T00:00:00+0000")
    ]
  where
    desc = "Ord cases"

    go vals = for_ (zip [1 ..] vals) $ \(i, (x, op, y)) ->
      assertDiff @Timestamp i x op y

momentTests :: TestTree
momentTests =
  testGroup
    "Moment"
    [ testParseMomentProp,
      testMomentParseCases,
      testMomentEqLaws,
      testMomentEqOpTotal,
      testMomentEqOpLaws,
      testMomentEqOpCases,
      testMomentOrdOpTotal,
      testMomentOrdOpLaws,
      testMomentOrdOpCases
    ]

testParseMomentProp :: TestTree
testParseMomentProp = testPropertyNamed name desc $ property $ do
  txt <- forAll genMomentTxt
  case P.parse @Moment txt of
    Ok _ -> pure ()
    Err err -> do
      annotate err
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
      Ok (MomentYear _) -> pure ()
      Ok other ->
        assertFailure $ "Expected year, received: " ++ show other
      Err err ->
        assertFailure $ "Unexpected parse failure: " ++ err

    isMonth t = case P.parse @Moment t of
      Ok (MomentMonth _ _) -> pure ()
      Ok other ->
        assertFailure $ "Expected year/month, received: " ++ show other
      Err err ->
        assertFailure $ "Unexpected parse failure: " ++ err

    isTimestamp t = case P.parse @Moment t of
      Ok (MomentTimestamp _) -> pure ()
      Ok other ->
        assertFailure $ "Expected timestamp, received: " ++ show other
      Err err ->
        assertFailure $ "Unexpected parse failure: " ++ err

testMomentEqLaws :: TestTree
testMomentEqLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genMoment
  y <- forAll genMoment
  z <- forAll genMoment

  -- reflexivity
  x === x

  -- symmetry
  (x == y) === (y == x)

  -- transitivity
  when (x == y && y == z) (x === z)

  -- negation
  (x /= y) === not (x == y)
  where
    name = "testMomentEqLaws"
    desc = "Eq laws"

testMomentEqOpTotal :: TestTree
testMomentEqOpTotal = testPropertyNamed name desc $ property $ do
  m1 <- forAll genMoment
  m2 <- forAll genMoment
  evalF (==) m1 m2
  where
    name = "testMomentEqOpTotal"
    desc = "Eq operator is total"

testMomentEqOpLaws :: TestTree
testMomentEqOpLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genMoment
  y <- forAll genMoment

  -- reflexivity
  x .=== x

  -- symmetry
  (x .== y) === (y .== x)

  -- negation
  (x ./= y) === not (x .== y)
  where
    name = "testMomentEqOpLaws"
    desc = "Eq operator laws"

testMomentEqOpCases :: TestTree
testMomentEqOpCases = testProp1 "testMomentEqOpCases" desc $ do
  go
    [ -- Year / Year
      ("2023", (./=), "2022"),
      ("2023", (.==), "2023"),
      ("2023", (./=), "2024"),
      -- Year / Month
      ("2023", (./=), "2022-12"),
      ("2023", (.==), "2023-12"),
      ("2023", (./=), "2024-12"),
      -- Year / Day
      ("2023", (./=), "2022-12-08"),
      ("2023", (.==), "2023-12-08"),
      ("2023", (./=), "2024-12-08"),
      -- Year / LocalTime
      ("2023", (./=), "2022-12-08T14:13:20"),
      ("2023", (.==), "2023-12-08T14:13:20"),
      ("2023", (./=), "2024-12-08T14:13:20"),
      -- Year / Zoned
      ("2023", (./=), "2022-12-08T14:13:20+0800"),
      ("2023", (.==), "2023-12-08T14:13:20+0800"),
      ("2023", (./=), "2024-12-08T14:13:20+0800"),
      -- Month / Month
      ("2023-12", (./=), "2023-11"),
      ("2023-12", (.==), "2023-12"),
      ("2023-12", (./=), "2024-01"),
      -- Month / Day
      ("2023-12", (./=), "2023-11-08"),
      ("2023-12", (.==), "2023-12-08"),
      ("2023-12", (./=), "2024-01-08"),
      -- Month / LocalTime
      ("2023-12", (./=), "2023-11-08T14:13:20"),
      ("2023-12", (.==), "2023-12-08T14:13:20"),
      ("2023-12", (./=), "2024-01-08T14:13:20"),
      -- Month / Zoned
      ("2023-12", (./=), "2023-11-08T14:13:20+0800"),
      ("2023-12", (.==), "2023-12-08T14:13:20+0800"),
      ("2023-12", (./=), "2024-01-08T14:13:20+0800")
    ]
  where
    desc = "Eq operator cases"

    go vals = for_ (zip [1 ..] vals) $ \(i, (x, op, y)) ->
      assertDiff @Moment i x op y

testMomentOrdOpTotal :: TestTree
testMomentOrdOpTotal = testPropertyNamed name desc $ property $ do
  m1 <- forAll genMoment
  m2 <- forAll genMoment
  evalF (.<=) m1 m2
  where
    name = "testMomentOrdOpTotal"
    desc = "Ord operator is total"

testMomentOrdOpLaws :: TestTree
testMomentOrdOpLaws = testPropertyNamed name desc $ property $ do
  x <- forAll genMoment
  y <- forAll genMoment

  -- comparability
  let xLteY = x .<= y
      yLteX = y .<= x

  assert (xLteY || yLteX)

  -- reflexivity
  assert (x .<= x)

  -- antisymmetry
  when (xLteY && yLteX) $ assert (x .== y)
  where
    name = "testMomentOrdOpLaws"
    desc = "Ord operator laws"

testMomentOrdOpCases :: TestTree
testMomentOrdOpCases = testProp1 "testMomentOrdOpCases" desc $ do
  go
    [ ("2023", (.<=), "2023"),
      ("2023", (.<=), "2024"),
      ("2023", (.<), "2024"),
      ("2023", (.<), "2024"),
      ("2020-06-01", (.<), "2023"),
      ("2020-06-01", (.<=), "2023"),
      ("2023", (.>), "2020-06-01"),
      ("2020-06-01", (\x y -> not (x .> y)), "2023")
    ]
  where
    desc = "Ord operator cases"

    go vals = for_ (zip [1 ..] vals) $ \(i, (x, op, y)) ->
      assertDiff @Moment i x op y

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
  sep <- Gen.element ['T', ' ']
  pure $ (dateTxt <> T.singleton sep <> timeTxt)

genTzOffsetTxt :: Gen Text
genTzOffsetTxt = do
  h <- Gen.integral @_ @Word8 (Range.linear 0 23)
  m <- Gen.integral @_ @Word8 (Range.linear 0 59)
  s <- Gen.element ["", " "]
  d <- Gen.element ['+', '-']
  pure
    $ mconcat
      [ s,
        T.singleton d,
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

assertDiff ::
  ( Parser a,
    Show a
  ) =>
  Word8 ->
  Text ->
  (a -> a -> Bool) ->
  Text ->
  PropertyT IO ()
assertDiff @a i x op y = do
  annotateShow i
  hdiff (parseOrDie @a x) op (parseOrDie y)
