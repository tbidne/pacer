module Pacer.Command.Chart.Data.Time
  ( -- * Timestamp
    Timestamp (..),
    fmtTimestamp,

    -- * Month
    Month (..),

    -- * Year
    Year (..),

    -- * Moment
    Moment (..),
  )
where

import Data.Aeson (ToJSON (toJSON))
import Data.Char qualified as Ch
import Data.Enum (Enum (fromEnum, toEnum))
import Data.Time (LocalTime (LocalTime), ZonedTime (ZonedTime))
import Data.Time qualified as Time
import Data.Time.Calendar qualified as Cal
import Data.Time.Format qualified as Format
import Data.Word (Word16)
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

instance Eq Timestamp where
  TimestampDate d1 == t2 = d1 == toDay t2
  TimestampTime (LocalTime d1 _) == TimestampDate d2 = d1 == d2
  TimestampTime l1 == TimestampTime l2 = l1 == l2
  TimestampTime l1 == TimestampZoned (ZonedTime l2 _) = l1 == l2
  TimestampZoned (ZonedTime (LocalTime d1 _) _) == TimestampDate d2 = d1 == d2
  TimestampZoned (ZonedTime l1 _) == TimestampTime l2 = l1 == l2
  TimestampZoned z1 == TimestampZoned z2 =
    Time.zonedTimeToUTC z1 == Time.zonedTimeToUTC z2

instance Ord Timestamp where
  TimestampDate d1 <= t2 = d1 <= toDay t2
  TimestampTime (LocalTime d1 _) <= TimestampDate d2 = d1 <= d2
  TimestampTime l1 <= TimestampTime l2 = l1 <= l2
  TimestampTime l1 <= TimestampZoned (ZonedTime l2 _) = l1 <= l2
  TimestampZoned (ZonedTime (LocalTime d1 _) _) <= TimestampDate d2 = d1 <= d2
  TimestampZoned (ZonedTime l1 _) <= TimestampTime l2 = l1 <= l2
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

-- TODO: Why does this fail???
--
-- TOML.decode @Timestamp "2024-10-20T14:30:00"

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

toDay :: Timestamp -> Day
toDay (TimestampDate d) = d
toDay (TimestampTime (LocalTime d _)) = d
toDay (TimestampZoned (ZonedTime (LocalTime d _) _)) = d

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

-- | Generalized 'Timestamp'.
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
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance Eq Moment where
  MomentYear y1 == mt2 = y1 == momentToYear mt2
  MomentMonth y1 _ == MomentYear y2 = y1 == y2
  MomentMonth y1 m1 == MomentMonth y2 m2 = y1 == y2 && m1 == m2
  MomentMonth y1 m1 == MomentTimestamp t2 =
    let (y2, m2) = timestampToYearMonth t2
     in y1 == y2 && m1 == m2
  MomentTimestamp t1 == MomentYear y2 =
    let y1 = timestampToYear t1
     in y1 == y2
  MomentTimestamp t1 == MomentMonth y2 m2 =
    let (y1, m1) = timestampToYearMonth t1
     in y1 == y2 && m1 == m2
  MomentTimestamp t1 == MomentTimestamp t2 = t1 == t2

instance Ord Moment where
  MomentYear y1 <= mt2 = y1 <= momentToYear mt2
  MomentMonth y1 _ <= MomentYear y2 = y1 <= y2
  MomentMonth y1 m1 <= MomentMonth y2 m2 = yearMonthLte y1 m1 y2 m2
  MomentMonth y1 m1 <= MomentTimestamp t2 =
    let (y2, m2) = timestampToYearMonth t2
     in yearMonthLte y1 m1 y2 m2
  MomentTimestamp t1 <= MomentYear y2 =
    let y1 = timestampToYear t1
     in y1 == y2
  MomentTimestamp t1 <= MomentMonth y2 m2 =
    let (y1, m1) = timestampToYearMonth t1
     in yearMonthLte y1 m1 y2 m2
  MomentTimestamp t1 <= MomentTimestamp t2 = t1 <= t2

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

yearMonthLte :: Year -> Month -> Year -> Month -> Bool
yearMonthLte y1 m1 y2 m2
  | y1 < y2 = True
  | y1 == y2 = m1 <= m2
  | otherwise = False

momentToYear :: Moment -> Year
momentToYear (MomentYear y) = y
momentToYear (MomentMonth y _) = y
momentToYear (MomentTimestamp ts) = timestampToYear ts

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
