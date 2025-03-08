module Pacer.Command.Chart.Data.Time.Year
  ( Year (..),
    mkYear,
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
    case Interval.mkInterval word of
      Just y -> pure $ MkYear y
      Nothing ->
        fail $ "Expected a year in 1950 - 2099, received: " ++ show word

yearToTime :: Year -> Integer
yearToTime y = toℤ y.unYear.unInterval

mkYear :: (Show a, Toℤ a) => a -> ResultDefault Year
mkYear i = case f i of
  Nothing -> fail $ "Unexpected year: " ++ show i
  Just x -> pure $ MkYear x
  where
    f = Interval.mkInterval . fromℤ . toℤ
