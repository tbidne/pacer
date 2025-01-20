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
      testParseTimestampCases
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
    desc = "Parses timestamp text"

testParseTimestampCases :: TestTree
testParseTimestampCases = testCase "Parses timestamp cases" $ do
  assertTime "1950-01-01T00:00:00"
  where
    assertTime t = case P.parse @Timestamp t of
      Left err ->
        assertFailure $ "Failed parsing time: " ++ unpackText err
      Right (TimestampTime _) -> pure ()
      Right bad ->
        assertFailure $ "Expected time, received: " ++ show bad

momentTests :: TestTree
momentTests =
  testGroup
    "Moment"
    [ testParseMomentProp,
      testMomentEqTotal,
      testMomentOrdTotal
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
    desc = "Parses moment text"

testMomentEqTotal :: TestTree
testMomentEqTotal = testPropertyNamed name desc $ property $ do
  m1 <- forAll genMoment
  m2 <- forAll genMoment
  evalF (==) m1 m2
  where
    name = "testMomentEqTotal"
    desc = "Moment Eq is total"

testMomentOrdTotal :: TestTree
testMomentOrdTotal = testPropertyNamed name desc $ property $ do
  m1 <- forAll genMoment
  m2 <- forAll genMoment
  evalF (<=) m1 m2
  where
    name = "testMomentOrdTotal"
    desc = "Moment Ord is total"

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
