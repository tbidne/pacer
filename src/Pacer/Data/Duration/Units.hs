-- | Units for time.
module Pacer.Data.Duration.Units
  ( TimeUnit (..),

    -- * Singletons
    STimeUnit (..),
  )
where

import Pacer.Class.Units (Units (baseFactor))
import Pacer.Prelude

-- | Time unit.
data TimeUnit
  = Second
  | Minute
  | Hour
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance Display TimeUnit where
  displayBuilder Second = "s"
  displayBuilder Minute = "m"
  displayBuilder Hour = "h"

instance Units TimeUnit where
  baseFactor Second = fromZ 1
  baseFactor Minute = fromZ 60
  baseFactor Hour = fromZ 3_600

-- | Singleton for 'TimeUnit'.
data STimeUnit (t :: TimeUnit) where
  SSecond :: STimeUnit Second
  SMinute :: STimeUnit Minute
  SHour :: STimeUnit Hour

deriving stock instance Show (STimeUnit t)

type instance Sing = STimeUnit

instance SingI Second where
  sing = SSecond

instance SingI Minute where
  sing = SMinute

instance SingI Hour where
  sing = SHour

instance SingKind TimeUnit where
  type Demote TimeUnit = TimeUnit

  fromSing SSecond = Second
  fromSing SMinute = Minute
  fromSing SHour = Hour

  toSing Second = SomeSing SSecond
  toSing Minute = SomeSing SMinute
  toSing Hour = SomeSing SHour
