module Pacer.Command.Chart.Data.Time.Year
  ( -- * Type
    Year (..),

    -- * Construction
    mkYear,

    -- * Elimination
    yearToTime,
  )
where

import Data.Enum (Enum (fromEnum, toEnum))
import Numeric.Data.Interval.Algebra
  ( Interval,
    IntervalBound (Closed),
  )
import Numeric.Data.Interval.Algebra qualified as Interval
import Pacer.Class.Parser (Parser (parser))
import Pacer.Prelude

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
    mkYear @_ @Word16 word

-- | Eliminates 'Year' to "Data.Time" compatible 'Integer'.
yearToTime :: Year -> Integer
yearToTime y = toℤ y.unYear.unInterval

-- | Creates a 'Year' in the expected range or fails.
mkYear :: (MonadFail m, Show a, Toℤ a) => a -> m Year
mkYear i = case f i of
  Nothing ->
    fail $ "Expected a year in 1950 - 2099, received: " ++ show i
  Just x -> pure $ MkYear x
  where
    f = Interval.mkInterval . fromℤ . toℤ
