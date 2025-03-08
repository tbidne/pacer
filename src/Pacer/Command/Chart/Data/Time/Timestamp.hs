module Pacer.Command.Chart.Data.Time.Timestamp
  ( -- * Timestamp
    Timestamp,

    -- * Construction
    Internal.fromLocalTime,

    -- * Elimination
    Internal.timestampToYear,
    timestampToYearMonth,
    Internal.timestampToDay,

    -- * Predicates
    sameYear,
    sameMonth,
    sameWeek,

    -- * Rounding
    roundTimestampYear,
    roundTimestampMonth,
    roundTimestampWeek,

    -- * Formatting
    fmtTimestamp,

    -- * Misc
    strictOverlaps,
  )
where

import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.WeekDate qualified as Week
import Data.Time.Format qualified as Format
import Pacer.Command.Chart.Data.Time.Month (Month)
import Pacer.Command.Chart.Data.Time.Timestamp.Internal
  ( Timestamp
      ( TimestampDate,
        TimestampTime,
        TimestampZoned
      ),
  )
import Pacer.Command.Chart.Data.Time.Timestamp.Internal qualified as Internal
import Pacer.Command.Chart.Data.Time.Year (Year)
import Pacer.Prelude

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

roundTimestampYear :: Timestamp -> Timestamp
roundTimestampYear ts = TimestampDate newDay
  where
    day = Internal.timestampToDay ts
    (y, _, _) = Cal.toGregorian day

    newDay = Cal.fromGregorian y 1 1

roundTimestampMonth :: Timestamp -> Timestamp
roundTimestampMonth ts = TimestampDate newDay
  where
    day = Internal.timestampToDay ts
    (y, moy, _) = Cal.toGregorian day

    newDay = Cal.fromGregorian y moy 1

roundTimestampWeek :: Timestamp -> Timestamp
roundTimestampWeek ts = TimestampDate newDay
  where
    day = Internal.timestampToDay ts

    (y, woy, _) =
      Week.toWeekCalendar Week.FirstWholeWeek Week.Monday day

    newDay =
      Week.fromWeekCalendar
        Week.FirstWholeWeek
        Week.Monday
        y
        woy
        Week.Monday

timestampToYearMonth :: (MonadFail m) => Timestamp -> m (Tuple2 Year Month)
timestampToYearMonth = Internal.dayToYearMonth . Internal.timestampToDay

sameYear :: (HasCallStack) => Timestamp -> Timestamp -> Bool
sameYear ts1 ts2 = y1 == y2
  where
    y1 = errorErr $ Internal.timestampToYear ts1
    y2 = errorErr $ Internal.timestampToYear ts2

sameMonth :: (HasCallStack) => Timestamp -> Timestamp -> Bool
sameMonth ts1 ts2 = ym1 == ym2
  where
    ym1 = errorErr $ timestampToYearMonth ts1
    ym2 = errorErr $ timestampToYearMonth ts2

sameWeek :: Timestamp -> Timestamp -> Bool
sameWeek ts1 ts2 = y1 == y2 && w1 == w2
  where
    (y1, w1, _) = timestampToYearMonthWeek ts1
    (y2, w2, _) = timestampToYearMonthWeek ts2

    timestampToYearMonthWeek = Week.toWeekDate . Internal.timestampToDay
