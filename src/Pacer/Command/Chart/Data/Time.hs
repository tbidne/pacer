module Pacer.Command.Chart.Data.Time
  ( -- * Timestamp
    Timestamp (..),

    -- ** Conversions
    timestampToYear,
    timestampToYearMonth,

    -- ** Predicates
    overlaps,
    sameYear,
    sameMonth,
    sameWeek,

    -- ** Rounding
    roundTimestampYear,
    roundTimestampMonth,
    roundTimestampWeek,

    -- ** Formatting
    fmtTimestamp,

    -- ** Misc
    strictOverlaps,

    -- * Month
    Month (..),
    monthToTimeMoY,

    -- * Year
    Year (..),
    yearToTime,

    -- * Moment
    Moment (..),

    -- ** Operators
    (.==),
    (./=),
    (.<=),
    (.<),
    (.>=),
    (.>),
  )
where

import Data.Char qualified as Ch
import Data.Enum (Enum (fromEnum, toEnum))
import Data.Time
  ( LocalTime (LocalTime),
    UTCTime (UTCTime),
    ZonedTime (ZonedTime),
  )
import Data.Time qualified as Time
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.WeekDate qualified as Week
import Data.Time.Format qualified as Format
import Data.Time.LocalTime qualified as LT
import Numeric.Data.Interval.Algebra
  ( Interval,
    IntervalBound (Closed),
  )
import Numeric.Data.Interval.Algebra qualified as Interval
import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Prelude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-------------------------------------------------------------------------------
--                                 Timestamp                                 --
-------------------------------------------------------------------------------

-- | Timestamp for runs.
data Timestamp
  = -- | A date like 2010-03-06
    TimestampDate Day
  | -- | A local time like 2010-03-06T14:23:05
    TimestampTime LocalTime
  | -- | A zoned time like 2010-03-06T14:23:05+0800
    TimestampZoned ZonedTime
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

-- Eq/Ord is based on the following idea: Data with less precision should
-- compare less when otherwise equal. For instance,
--
--   2013-08-19 < 2013-08-19T12:40:20
--
-- This is to allow a total, "sensible" order i.e.
--
--   2013-08-19T12:40:20 > 2012-08-19T12:40:20
--
-- while also respecting Eq/Ord's laws.
--
-- Notice that this means we need to take care to handle timezones. To wit,
-- we always convert timezones to UTC.

-- Eq: Only identical constructors can be Eq.

instance Eq Timestamp where
  TimestampDate d1 == TimestampDate d2 = d1 == d2
  TimestampTime l1 == TimestampTime l2 = l1 == l2
  TimestampZoned z1 == TimestampZoned z2 =
    Time.zonedTimeToUTC z1 == Time.zonedTimeToUTC z2
  _ == _ = False

-- Ord: To ensure we respect laws, notice that whenever we have a type
-- with more precision on the LHS of (<=), we require it to be strictly less
-- for (<=) to be true.
--
-- For instance, in order for
--
--   TimestampTime (LocalTime d1) <= TimestampDate d2
--
-- we require d1 < d2, __not__ d1 <= d2. This is because we want LocalTimes
-- to never equal Dates, hence the only time d1 can be less than or equal d2
-- is when it is strictly less.

instance Ord Timestamp where
  TimestampDate d1 <= TimestampDate d2 = d1 <= d2
  TimestampDate d1 <= TimestampTime (LocalTime d2 _) = d1 <= d2
  TimestampDate d1 <= TimestampZoned z2 =
    -- d1 <= d2
    let UTCTime d2 _ = Time.zonedTimeToUTC z2
     in d1 <= d2
  TimestampTime (LocalTime d1 _) <= TimestampDate d2 = d1 < d2
  TimestampTime l1 <= TimestampTime l2 = l1 <= l2
  TimestampTime l1 <= TimestampZoned z2 =
    -- l1 <= l2
    let UTCTime d2 diff2 = Time.zonedTimeToUTC z2
        l2 = LocalTime d2 (LT.timeToTimeOfDay diff2)
     in l1 <= l2
  TimestampZoned z1 <= TimestampDate d2 =
    let UTCTime d1 _ = Time.zonedTimeToUTC z1
     in d1 < d2
  TimestampZoned z1 <= TimestampTime l2 =
    let UTCTime d1 diff1 = Time.zonedTimeToUTC z1
        l1 = LocalTime d1 (LT.timeToTimeOfDay diff1)
     in l1 < l2
  TimestampZoned z1 <= TimestampZoned z2 =
    Time.zonedTimeToUTC z1 <= Time.zonedTimeToUTC z2

-------------------------------------------------------------------------------
--                               Serialization                               --
-------------------------------------------------------------------------------

instance Display Timestamp where
  displayBuilder = \case
    TimestampDate d -> displayBuilder $ show d
    TimestampTime d -> displayBuilder $ show d
    TimestampZoned d -> displayBuilder $ show d

instance Parser Timestamp where
  parser = do
    asum
      [ TimestampZoned <$> MP.try parser,
        TimestampTime <$> MP.try parser,
        TimestampDate <$> parser
      ]

instance FromJSON Timestamp where
  parseJSON v =
    asum
      [ fmap TimestampDate . parseJSON $ v,
        fmap TimestampTime . parseJSON $ v,
        fmap TimestampZoned . parseJSON $ v
      ]

instance ToJSON Timestamp where
  toJSON (TimestampDate d) = toJSON d
  toJSON (TimestampTime lt) = toJSON lt
  toJSON (TimestampZoned zt) = toJSON zt

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | Returns true if two timestamps "overlap", essentially a relaxed version
-- of (==), where two timestamps are considered overlapping when their
-- "greatest common data" is equal.
overlaps :: Timestamp -> Timestamp -> Bool
overlaps (TimestampDate d1) (TimestampDate d2) = d1 == d2
overlaps (TimestampDate d1) (TimestampTime (LocalTime d2 _)) = d1 == d2
overlaps (TimestampDate d1) (TimestampZoned (ZonedTime (LocalTime d2 _) _)) = d1 == d2
overlaps (TimestampTime (LocalTime d1 _)) (TimestampDate d2) = d1 == d2
overlaps (TimestampTime l1) (TimestampTime l2) = l1 == l2
overlaps (TimestampTime l1) (TimestampZoned (ZonedTime l2 _)) = l1 == l2
overlaps (TimestampZoned (ZonedTime (LocalTime d1 _) _)) (TimestampDate d2) = d1 == d2
overlaps (TimestampZoned (ZonedTime l1 _)) (TimestampTime l2) = l1 == l2
overlaps (TimestampZoned z1) (TimestampZoned z2) =
  Time.zonedTimeToUTC z1 == Time.zonedTimeToUTC z2

fmtTimestamp :: Timestamp -> Text
fmtTimestamp =
  packText <<< \case
    TimestampDate d -> Format.formatTime l dfmt d
    TimestampTime lt -> Format.formatTime l (dfmt ++ tfmt) lt
    TimestampZoned zt -> Format.formatTime l (dfmt ++ tfmt ++ zfmt) zt
  where
    dfmt = "%Y-%m-%d"
    tfmt = "T%H:%M:%S"
    zfmt = "%z"
    l = Format.defaultTimeLocale

-- | Returns a list of timestamps that overlap with this one. Strict in the
-- sense that the timestamp itself is not included.
strictOverlaps :: Timestamp -> List Timestamp
strictOverlaps (TimestampDate _) = []
strictOverlaps (TimestampTime (LocalTime d _)) = [TimestampDate d]
strictOverlaps (TimestampZoned (ZonedTime l@(LocalTime d _) _)) =
  [TimestampDate d, TimestampTime l]

-------------------------------------------------------------------------------
--                                   Month                                   --
-------------------------------------------------------------------------------

-- | Represents a calendar month.
data Month
  = Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                               Serialization                               --
-------------------------------------------------------------------------------

instance Parser Month where
  parser = do
    txt <- MP.takeWhile1P (Just "01-12") (\c -> Ch.isDigit c)
    case txt of
      "01" -> pure Jan
      "02" -> pure Feb
      "03" -> pure Mar
      "04" -> pure Apr
      "05" -> pure May
      "06" -> pure Jun
      "07" -> pure Jul
      "08" -> pure Aug
      "09" -> pure Sep
      "10" -> pure Oct
      "11" -> pure Nov
      "12" -> pure Dec
      other ->
        fail $ "Expected a month in 01 - 12, received: " ++ unpackText other

instance Display Month where
  displayBuilder = \case
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    Apr -> "04"
    May -> "05"
    Jun -> "06"
    Jul -> "07"
    Aug -> "08"
    Sep -> "09"
    Oct -> "10"
    Nov -> "11"
    Dec -> "12"

monthToTimeMoY :: Month -> Int
monthToTimeMoY Jan = 1
monthToTimeMoY Feb = 2
monthToTimeMoY Mar = 3
monthToTimeMoY Apr = 4
monthToTimeMoY May = 5
monthToTimeMoY Jun = 6
monthToTimeMoY Jul = 7
monthToTimeMoY Aug = 8
monthToTimeMoY Sep = 9
monthToTimeMoY Oct = 10
monthToTimeMoY Nov = 11
monthToTimeMoY Dec = 12

-------------------------------------------------------------------------------
--                                   Year                                    --
-------------------------------------------------------------------------------

-- | Represents a year.
newtype Year = MkYear
  { unYear :: Interval (Closed 1950) (Closed 2099) Word16
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

instance Bounded Year where
  minBound = MkYear $ Interval.unsafeInterval 1950
  maxBound = MkYear $ Interval.unsafeInterval 2099

instance Enum Year where
  fromEnum = fromIntegral . (.unInterval) . (.unYear)
  toEnum = MkYear . Interval.unsafeInterval . fromIntegral

instance Display Year where
  displayBuilder = displayBuilder . Interval.unInterval . unYear

instance Parser Year where
  parser = do
    word <- parser
    case Interval.mkInterval word of
      Just y -> pure $ MkYear y
      Nothing ->
        fail $ "Expected a year in 1950 - 2099, received: " ++ show word

yearToTime :: Year -> Integer
yearToTime y = fromIntegral @Word16 @Integer y.unYear.unInterval

-------------------------------------------------------------------------------
--                                 Timestamp                                 --
-------------------------------------------------------------------------------

-- | Generalized 'Timestamp', for run filtering.
data Moment
  = -- | A year like 2013.
    MomentYear Year
  | -- | A year and month like 2013-08.
    MomentMonth Year Month
  | -- A Timestamp.
    --
    -- NOTE: These timestamps should be in between 1950 and 2099. Should we
    -- enforce this? E.g. do not export the bare constructor, and make sure
    -- parsing enforces this.
    --
    -- Probably the most robust course of action would be to make Timestamp
    -- enforce the constraint, then we'd inherit it here. We'd probably
    -- want Year and Timestamp defined in the same module so we could share
    -- the validation.
    MomentTimestamp Timestamp
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance Display Moment where
  displayBuilder = \case
    MomentYear x -> displayBuilder x
    MomentMonth x y -> displayBuilder x <> "-" <> displayBuilder y
    MomentTimestamp x -> displayBuilder x

instance FromJSON Moment where
  parseJSON = asnWithText "Moment" (failErr . P.parseAll)

instance Parser Moment where
  parser = do
    asum
      [ MomentTimestamp <$> MP.try parser,
        MP.try parseYearMonth,
        parseYear
      ]
    where
      parseYearMonth = do
        year <- parser
        MPC.char '-'
        month <- parser
        pure $ MomentMonth year month

      parseYear = MomentYear <$> parser

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- Our general interest in Moment Ord comparisons is when filtering Runs e.g.
-- we want to filter runs w/ Run.datetime >= Moment. In particular, we need
-- to compare "low precision" moments like years w/ higher precision
-- timestamps. For example, we may want to takes runs with a timestamp >=
-- 2013.
--
-- To do this, we introduce the below Eq/Ord inspired functions that compare
-- based on the "lowest upper bound" e.g. 2013-08-10 and 2013 are compared
-- via the year, 2013.
--
-- Note that this is not lawful (breaks transitivity, specifically), hence
-- are not part of Eq/Ord instances.

-- | Relaxed equals.
(.==) :: Moment -> Moment -> Bool
MomentYear y1 .== MomentYear y2 = y1 == y2
MomentYear y1 .== MomentMonth y2 _ = y1 == y2
MomentYear y1 .== MomentTimestamp t2 = y1 == timestampToYear t2
MomentMonth y1 _ .== MomentYear y2 = y1 == y2
MomentMonth y1 m1 .== MomentMonth y2 m2 = y1 == y2 && m1 == m2
MomentMonth y1 m1 .== MomentTimestamp t2 = (y1, m1) == timestampToYearMonth t2
MomentTimestamp t1 .== MomentYear y2 = timestampToYear t1 == y2
MomentTimestamp t1 .== MomentMonth y2 m2 = timestampToYearMonth t1 == (y2, m2)
MomentTimestamp t1 .== MomentTimestamp t2 = overlaps t1 t2

infix 4 .==

-- | Relaxed "not equals".
(./=) :: Moment -> Moment -> Bool
x ./= y = not (x .== y)

infix 4 ./=

-- | Relaxes (<=).
(.<=) :: Moment -> Moment -> Bool
MomentYear y1 .<= MomentYear y2 = y1 <= y2
MomentYear y1 .<= MomentMonth y2 _ = y1 <= y2
MomentYear y1 .<= MomentTimestamp t2 = y1 <= timestampToYear t2
MomentMonth y1 _ .<= MomentYear y2 = y1 <= y2
MomentMonth y1 m1 .<= MomentMonth y2 m2
  | y1 < y2 = True
  | y1 == y2 = m1 <= m2
  | otherwise = False
MomentMonth y1 m1 .<= MomentTimestamp t2 =
  let (y2, m2) = timestampToYearMonth t2
   in if
        | y1 < y2 -> True
        | y1 == y2 -> m1 <= m2
        | otherwise -> False
MomentTimestamp t1 .<= MomentYear y2 = timestampToYear t1 <= y2
MomentTimestamp t1 .<= MomentMonth y2 m2 =
  let (y1, m1) = timestampToYearMonth t1
   in if
        | y1 < y2 -> True
        | y1 == y2 -> m1 <= m2
        | otherwise -> False
MomentTimestamp t1 .<= MomentTimestamp t2 = tsLte t1 t2
  where
    tsLte (TimestampDate d1) (TimestampDate d2) = d1 <= d2
    tsLte (TimestampDate d1) (TimestampTime (LocalTime d2 _)) = d1 <= d2
    tsLte (TimestampDate d1) (TimestampZoned (ZonedTime (LocalTime d2 _) _)) = d1 <= d2
    tsLte (TimestampTime (LocalTime d1 _)) (TimestampDate d2) = d1 <= d2
    tsLte (TimestampTime l1) (TimestampTime l2) = l1 <= l2
    tsLte (TimestampTime l1) (TimestampZoned (ZonedTime l2 _)) = l1 <= l2
    tsLte (TimestampZoned (ZonedTime (LocalTime d1 _) _)) (TimestampDate d2) = d1 <= d2
    tsLte (TimestampZoned (ZonedTime l1 _)) (TimestampTime l2) = l1 <= l2
    tsLte (TimestampZoned z1) (TimestampZoned z2) =
      Time.zonedTimeToUTC z1 <= Time.zonedTimeToUTC z2

infix 4 .<=

-- | Relaxes (>=).
(.>=) :: Moment -> Moment -> Bool
x .>= y = y .<= x

infix 4 .>=

-- | Relaxes (>).
(.>) :: Moment -> Moment -> Bool
x .> y = not (x .<= y)

infix 4 .>

-- | Relaxes (<).
(.<) :: Moment -> Moment -> Bool
x .< y = not (y .<= x)

infix 4 .<

timestampToYear :: Timestamp -> Year
timestampToYear = f . timestampToDay
  where
    f = fst . errorErr . dayToYearMonth

roundTimestampYear :: Timestamp -> Timestamp
roundTimestampYear ts = TimestampDate newDay
  where
    day = timestampToDay ts
    (y, _, _) = Cal.toGregorian day

    newDay = Cal.fromGregorian y 1 1

roundTimestampMonth :: Timestamp -> Timestamp
roundTimestampMonth ts = TimestampDate newDay
  where
    day = timestampToDay ts
    (y, moy, _) = Cal.toGregorian day

    newDay = Cal.fromGregorian y moy 1

roundTimestampWeek :: Timestamp -> Timestamp
roundTimestampWeek ts = TimestampDate newDay
  where
    day = timestampToDay ts

    (y, woy, _) =
      Week.toWeekCalendar Week.FirstWholeWeek Week.Monday day

    newDay =
      Week.fromWeekCalendar
        Week.FirstWholeWeek
        Week.Monday
        y
        woy
        Week.Monday

timestampToYearMonth :: Timestamp -> Tuple2 Year Month
timestampToYearMonth = errorErr . dayToYearMonth . timestampToDay

timestampToDay :: Timestamp -> Day
timestampToDay = \case
  (TimestampDate d) -> d
  (TimestampTime (LocalTime d _)) -> d
  (TimestampZoned (ZonedTime (LocalTime d _) _)) -> d

sameYear :: Timestamp -> Timestamp -> Bool
sameYear ts1 ts2 = y1 == y2
  where
    y1 = timestampToYear ts1
    y2 = timestampToYear ts2

sameMonth :: Timestamp -> Timestamp -> Bool
sameMonth ts1 ts2 = ym1 == ym2
  where
    ym1 = timestampToYearMonth ts1
    ym2 = timestampToYearMonth ts2

sameWeek :: Timestamp -> Timestamp -> Bool
sameWeek ts1 ts2 = y1 == y2 && w1 == w2
  where
    (y1, w1, _) = timestampToYearMonthWeek ts1
    (y2, w2, _) = timestampToYearMonthWeek ts2

    timestampToYearMonthWeek = Week.toWeekDate . timestampToDay

dayToYearMonth :: Day -> ResultDefault (Tuple2 Year Month)
dayToYearMonth =
  (\(y, m, _) -> liftA2 (,) (intToYear y) (moyToMonth m))
    . Cal.toGregorian

intToYear :: Integer -> ResultDefault Year
intToYear i = case f i of
  Nothing -> fail $ "Unexpected year: " ++ show i
  Just x -> pure $ MkYear x
  where
    f =
      Interval.mkInterval
        . fromIntegral @Integer @Word16

moyToMonth :: Cal.MonthOfYear -> ResultDefault Month
moyToMonth 1 = pure Jan
moyToMonth 2 = pure Feb
moyToMonth 3 = pure Mar
moyToMonth 4 = pure Apr
moyToMonth 5 = pure May
moyToMonth 6 = pure Jun
moyToMonth 7 = pure Jul
moyToMonth 8 = pure Aug
moyToMonth 9 = pure Sep
moyToMonth 10 = pure Oct
moyToMonth 11 = pure Nov
moyToMonth 12 = pure Dec
moyToMonth other =
  fail
    $ "Expected month in 1-12: "
    ++ show other
