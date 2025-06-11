module Pacer.Command.Chart.Data.Time.Timestamp.Internal
  ( -- * Timestamp
    Timestamp (..),

    -- * Construction
    fromLocalTime,

    -- * Elimination
    timestampToYear,
    timestampToDay,

    -- * Functions
    dayToYearMonth,
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
import Data.Time.LocalTime qualified as LT
import Pacer.Class.IOrd (IEq ((~~)))
import Pacer.Class.Parser (Parser (parser))
import Pacer.Command.Chart.Data.Time.Month (Month, mkMonth)
import Pacer.Command.Chart.Data.Time.Year (Year, mkYear)
import Pacer.Prelude
import Pacer.Utils.Json (FromJSON (parseJSON), ToJSON (toJSON))
import Text.Megaparsec qualified as MP

-------------------------------------------------------------------------------
--                                 Timestamp                                 --
-------------------------------------------------------------------------------

-- | Timestamp for activities.
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
    ts <-
      asum
        [ TimestampZoned <$> MP.try parser,
          TimestampTime <$> MP.try parser,
          TimestampDate <$> parser
        ]

    -- Assert year in range.
    void $ timestampToYear ts

    pure ts

instance FromJSON Timestamp where
  parseJSON v = do
    ts <-
      asum
        [ fmap TimestampZoned . parseJSON $ v,
          fmap TimestampTime . parseJSON $ v,
          fmap TimestampDate . parseJSON $ v
        ]

    -- Assert year in range.
    void $ timestampToYear ts

    pure ts

instance ToJSON Timestamp where
  toJSON (TimestampDate d) = toJSON d
  toJSON (TimestampTime lt) = toJSON lt
  toJSON (TimestampZoned zt) = toJSON zt

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

fromLocalTime :: (MonadFail m) => LocalTime -> m Timestamp
fromLocalTime lt = do
  let ts = TimestampTime lt

  -- Assert year in range.
  void $ timestampToYear ts

  pure ts

timestampToYear :: (MonadFail m) => Timestamp -> m Year
timestampToYear = fmap fst . dayToYearMonth . timestampToDay

timestampToDay :: Timestamp -> Day
timestampToDay = \case
  (TimestampDate d) -> d
  (TimestampTime (LocalTime d _)) -> d
  (TimestampZoned (ZonedTime (LocalTime d _) _)) -> d

dayToYearMonth :: (MonadFail m) => Day -> m (Tuple2 Year Month)
dayToYearMonth =
  (\(y, m, _) -> liftA2 (,) (mkYear y) (mkMonth m))
    . Cal.toGregorian
