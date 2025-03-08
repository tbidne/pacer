module Pacer.Command.Chart.Data.Time.Timestamp
  ( -- * Timestamp
    Timestamp (..),

    -- ** Conversions
    timestampToYear,
    timestampToYearMonth,

    -- ** Predicates
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
  )
where

import Data.Hashable (Hashable (hashWithSalt))
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
import Pacer.Class.IOrd (IEq ((~~)))
import Pacer.Class.Parser (Parser (parser))
import Pacer.Command.Chart.Data.Time.Month (Month, mkMonth)
import Pacer.Command.Chart.Data.Time.Year (Year, mkYear)
import Pacer.Prelude
import Text.Megaparsec qualified as MP

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

instance Hashable Timestamp where
  hashWithSalt i = \case
    TimestampDate d -> hashWithSalt i d
    TimestampTime t -> hashWithSalt i t
    TimestampZoned z -> hashWithSalt i (Time.zonedTimeToUTC z)

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

-- Relaxed (intrasitive) equals, implementing "overlaps". This differs from
-- the usual Eq in that different constructors _can_ be considered equal, when
-- they "overlap".
instance IEq Timestamp where
  TimestampDate d1 ~~ TimestampDate d2 = d1 == d2
  TimestampDate d1 ~~ TimestampTime (LocalTime d2 _) = d1 == d2
  TimestampDate d1 ~~ TimestampZoned (ZonedTime (LocalTime d2 _) _) = d1 == d2
  TimestampTime (LocalTime d1 _) ~~ TimestampDate d2 = d1 == d2
  TimestampTime l1 ~~ TimestampTime l2 = l1 == l2
  TimestampTime l1 ~~ TimestampZoned (ZonedTime l2 _) = l1 == l2
  TimestampZoned (ZonedTime (LocalTime d1 _) _) ~~ TimestampDate d2 = d1 == d2
  TimestampZoned (ZonedTime l1 _) ~~ TimestampTime l2 = l1 == l2
  TimestampZoned z1 ~~ TimestampZoned z2 =
    Time.zonedTimeToUTC z1 == Time.zonedTimeToUTC z2

-- Ord: To ensure we respect laws, notice that whenever we have a type
-- with more precision on the LHS of (<=), we require it to be strictly less
-- for (<=) to be true.
--
-- For instance, in order for
--
--   TimestampTime (LocalTime d1 _) <= TimestampDate d2
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
      [ fmap TimestampZoned . parseJSON $ v,
        fmap TimestampTime . parseJSON $ v,
        fmap TimestampDate . parseJSON $ v
      ]

instance ToJSON Timestamp where
  toJSON (TimestampDate d) = toJSON d
  toJSON (TimestampTime lt) = toJSON lt
  toJSON (TimestampZoned zt) = toJSON zt

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

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
--                                    Misc                                   --
-------------------------------------------------------------------------------

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
  (\(y, m, _) -> liftA2 (,) (mkYear y) (mkMonth m))
    . Cal.toGregorian
