{-# LANGUAGE AllowAmbiguousTypes #-}

module Running.Data.Duration.Units
  ( TimeUnit (..),
    baseFactor,

    -- * Singletons
    STimeUnit (..),
    withSingleton,
    fromSingleton,
    fromSingleton',
    SingTimeUnit (..),
  )
where

import Data.Kind (Constraint)
import Data.Text.Display (Display (displayBuilder))

data TimeUnit
  = Second
  | Minute
  | Hour
  deriving stock (Eq, Show)

instance Display TimeUnit where
  displayBuilder Second = "s"
  displayBuilder Minute = "m"
  displayBuilder Hour = "h"

-- | Ratio of unit/second.
baseFactor :: (Fractional a) => TimeUnit -> a
baseFactor Second = 1
baseFactor Minute = 60
baseFactor Hour = 3_600

data STimeUnit (t :: TimeUnit) where
  SSecond :: STimeUnit Second
  SMinute :: STimeUnit Minute
  SHour :: STimeUnit Hour

type SingTimeUnit :: TimeUnit -> Constraint
class SingTimeUnit (t :: TimeUnit) where
  singTimeUnit :: STimeUnit t

instance SingTimeUnit Second where
  singTimeUnit = SSecond

instance SingTimeUnit Minute where
  singTimeUnit = SMinute

instance SingTimeUnit Hour where
  singTimeUnit = SHour

withSingleton :: STimeUnit t -> ((SingTimeUnit t) => r) -> r
withSingleton t x = case t of
  SSecond -> x
  SMinute -> x
  SHour -> x

fromSingleton :: STimeUnit t -> TimeUnit
fromSingleton SSecond = Second
fromSingleton SMinute = Minute
fromSingleton SHour = Hour

fromSingleton' :: forall t. (SingTimeUnit t) => TimeUnit
fromSingleton' = fromSingleton $ singTimeUnit @t
