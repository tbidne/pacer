{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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
    sameInterval,

    -- * Rounding
    roundTimestampYear,
    roundTimestampMonth,
    roundTimestampWeek,
    roundInterval,

    -- * Formatting
    fmtTimestamp,

    -- * Misc
    strictOverlaps,
  )
where

import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time.Calendar (Day (ModifiedJulianDay))
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

-- Start day for an interval. Naturally this should be associated with an
-- end date (either on the data or derived from a period), but because
-- this type is internal and we do need actually need the end date for
-- anything, we do not have it.
--
-- If we ever _do_ need the end date, either add @end :: Date@ or maybe add
-- the period in the type i.e. @Interval (p :: Nat)@.
newtype Interval = MkInterval {start :: Day}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Interval

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
--                                  Rounding                                 --
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

-- | Rounds the timestamp down to the start of its interval. That is,
-- finds the interval containing the given timestamp, and returns the
-- interval's start day.
roundInterval :: Day -> PWord16 -> Timestamp -> Timestamp
roundInterval totalStart period =
  TimestampDate
    . (view #start)
    . findInterval totalStart period

-------------------------------------------------------------------------------
--                              Group functions                              --
-------------------------------------------------------------------------------

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

-- | Determines if two timestamps belong to the same interval @vi@ where
-- @vi@ is an element in @d, d + p, d + 2p, ...@. In other words, each
-- timestamp will belong in some interval @d + pk@. sameInterval determines
-- if they are the same.
sameInterval ::
  -- | Start day d
  Day ->
  -- | Interval period (i.e. number of days) p
  PWord16 ->
  -- | ts1
  Timestamp ->
  -- | ts2
  Timestamp ->
  Bool
sameInterval totalStart period ts1 ts2 =
  findInterval totalStart period ts1
    == findInterval totalStart period ts2

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | For @findInterval d1 p d2, d1 <= d2@, returns interval @[s, e]@ s.t.
--
-- * @s = d1 * pk@
--
-- * @e = d1 * p(k + 1)@
--
-- * @s <= d2 <= e@
findInterval :: Day -> PWord16 -> Timestamp -> Interval
findInterval totalStart period ts = MkInterval {start}
  where
    -- NOTE: With time >= 1.14, we could instead use #toModifiedJulianDay,
    -- as time's Day gained a Generic instance, allowing generic optics.
    getDay (ModifiedJulianDay d) = d

    -- d1
    dayℤ = getDay totalStart

    -- d2
    tsDayℤ = getDay (Internal.timestampToDay ts)

    -- p
    periodℤ = fromIntegral @Word16 @Integer (period ^. #unPositive)

    -- k = (d2 - d1) / p i.e. largest k s.t. d1 + kp <= d2.
    k = (tsDayℤ - dayℤ) .%. periodℤ

    -- s = d1 + pk
    start = ModifiedJulianDay $ dayℤ + (periodℤ * k)

timestampToYearMonth :: (MonadFail m) => Timestamp -> m (Tuple2 Year Month)
timestampToYearMonth = Internal.dayToYearMonth . Internal.timestampToDay
