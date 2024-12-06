-- | Units for distance.
module Running.Data.Distance.Units
  ( DistanceUnit (..),

    -- * Singletons
    SDistanceUnit (..),
  )
where

import Running.Class.Parser (Parser (parser))
import Running.Class.Units (Units (baseFactor))
import Running.Prelude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | Distance unit.
data DistanceUnit
  = Meter
  | Kilometer
  | Mile
  deriving stock (Bounded, Enum, Eq, Show)

instance Display DistanceUnit where
  displayBuilder Meter = "m"
  displayBuilder Kilometer = "km"
  displayBuilder Mile = "mi"

instance Parser DistanceUnit where
  -- NOTE: [Parsing Common prefixes]
  --
  -- Say s1 is a prefix of s2. In that case, we need to ensure s2 is attempted
  -- first because otherwise s1 will succeed and we will never try s2. This
  -- also means s2 sill fail because the remaining s2 chars will not be
  -- parsed.
  --
  -- Alternatively, we could check that the parse ends after s1
  -- (e.g. whitespace), but we leave that to "upstream" parsers, for now.
  parser =
    MP.choice
      [ MPC.string "meters" $> Meter,
        MPC.string "km" $> Kilometer,
        MPC.string "kilometers" $> Kilometer,
        MPC.string "miles" $> Mile,
        MPC.string "mi" $> Mile,
        MPC.char 'm' $> Meter
      ]

instance Units DistanceUnit where
  baseFactor Meter = fromZ 1
  baseFactor Kilometer = fromZ 1_000
  baseFactor Mile = fromZ 1_609

-- | Singleton for 'DistanceUnit'.
data SDistanceUnit (d :: DistanceUnit) where
  SMeter :: SDistanceUnit Meter
  SKilometer :: SDistanceUnit Kilometer
  SMile :: SDistanceUnit Mile

deriving stock instance Show (SDistanceUnit d)

type instance Sing = SDistanceUnit

instance SingI Meter where
  sing = SMeter

instance SingI Kilometer where
  sing = SKilometer

instance SingI Mile where
  sing = SMile

instance SingKind DistanceUnit where
  type Demote DistanceUnit = DistanceUnit

  fromSing SMeter = Meter
  fromSing SKilometer = Kilometer
  fromSing SMile = Mile

  toSing Meter = SomeSing SMeter
  toSing Kilometer = SomeSing SKilometer
  toSing Mile = SomeSing SMile
