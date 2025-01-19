module Unit.Pacer.Command.Chart.Data.Time (tests) where

import Data.Enum (Enum (toEnum))
import Data.Time (Day (ModifiedJulianDay), LocalTime (LocalTime), TimeOfDay (TimeOfDay), TimeZone, ZonedTime (ZonedTime))
import Data.Time.Zones qualified as TZ
import Data.Time.Zones.All (TZLabel)
import Data.Time.Zones.All qualified as TZ.All
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pacer.Command.Chart.Data.Time
  ( Moment (MomentMonth, MomentTimestamp, MomentYear),
    Month,
    Timestamp (TimestampDay, TimestampTime, TimestampZoned),
    Year,
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Pacer.Command.Chart.Data.Time"
    [ testMomentEqTotal,
      testMomentOrdTotal
    ]

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
    [ TimestampDay <$> genDay,
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
