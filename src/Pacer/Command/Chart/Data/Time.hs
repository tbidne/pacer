module Pacer.Command.Chart.Data.Time
  ( -- * Timestamp
    Timestamp (..),
    overlaps,
    fmtTimestamp,

    -- * Month
    Month (..),

    -- * Year
    Year (..),

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

import Data.Aeson (ToJSON (toJSON))
import Data.Char qualified as Ch
import Data.Enum (Enum (fromEnum, toEnum))
import Data.Time
  ( LocalTime (LocalTime),
    UTCTime (UTCTime),
    ZonedTime (ZonedTime),
  )
import Data.Time qualified as Time
import Data.Time.Calendar qualified as Cal
import Data.Time.Format qualified as Format
import Data.Time.LocalTime qualified as LT
import Numeric.Data.Interval.Algebra
  ( Interval,
    IntervalBound (Closed),
  )
import Numeric.Data.Interval.Algebra qualified as Interval
import Pacer.Class.Parser (Parser (parser))
import Pacer.Prelude
import TOML (DecodeTOML (tomlDecoder))
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

instance DecodeTOML Timestamp where
  tomlDecoder =
    TimestampDate
      <$> tomlDecoder
      <|> TimestampTime
      <$> tomlDecoder
      <|> TimestampZoned
      <$> tomlDecoder

instance Parser Timestamp where
  -- reuse toml instance since it is already done for us upstream.
  parser = do
    MP.choice
      [ TimestampZoned <$> MP.try parser,
        TimestampTime <$> MP.try parser,
        TimestampDate <$> parser
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
-- "least upper bound" is equal.
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
    txt <- MP.takeWhile1P Nothing (\c -> Ch.isDigit c)
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

instance Parser Year where
  parser = do
    word <- parser
    case Interval.mkInterval word of
      Just y -> pure $ MkYear y
      Nothing ->
        fail $ "Expected a year in 1950 - 2099, received: " ++ show word

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

instance Parser Moment where
  parser = do
    MP.choice
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
timestampToYear = \case
  (TimestampDate d) -> toYear d
  (TimestampTime (LocalTime d _)) -> toYear d
  (TimestampZoned (ZonedTime (LocalTime d _) _)) -> toYear d
  where
    toYear :: Day -> Year
    toYear =
      MkYear
        . Interval.unsafeInterval
        . fromIntegral @Integer @Word16
        . (\(y, _, _) -> y)
        . Cal.toGregorian

timestampToYearMonth :: Timestamp -> Tuple2 Year Month
timestampToYearMonth = \case
  (TimestampDate d) -> toYearMonth d
  (TimestampTime (LocalTime d _)) -> toYearMonth d
  (TimestampZoned (ZonedTime (LocalTime d _) _)) -> toYearMonth d
  where
    toYearMonth :: Day -> Tuple2 Year Month
    toYearMonth =
      (\(y, m, _) -> (toYear y, toMonth m))
        . Cal.toGregorian

    toYear :: Integer -> Year
    toYear =
      MkYear
        . Interval.unsafeInterval
        . fromIntegral @Integer @Word16

    toMonth :: Cal.MonthOfYear -> Month
    toMonth 1 = Jan
    toMonth 2 = Feb
    toMonth 3 = Mar
    toMonth 4 = Apr
    toMonth 5 = May
    toMonth 6 = Jun
    toMonth 7 = Jul
    toMonth 8 = Aug
    toMonth 9 = Sep
    toMonth 10 = Oct
    toMonth 11 = Nov
    toMonth 12 = Dec
    toMonth other =
      error
        $ "Expected month in 1-12: "
        ++ show other
