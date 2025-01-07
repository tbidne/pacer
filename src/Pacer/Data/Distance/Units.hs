{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Units for distance.
module Pacer.Data.Distance.Units
  ( -- * Primary type
    DistanceUnit (..),

    -- * Singletons
    SDistanceUnit (..),

    -- * Conversion
    ConvertDistance (..),
    convertDistance,
    convertToMeters,
    convertToKilometers
  )
where

{- ORMOLU_ENABLE -}

import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Class.Units (Units (baseFactor))
import Pacer.Prelude
import TOML (DecodeTOML (tomlDecoder))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-------------------------------------------------------------------------------
--                                    Core                                   --
-------------------------------------------------------------------------------

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

instance DecodeTOML DistanceUnit where
  tomlDecoder = tomlDecoder >>= P.parseFail

instance Units DistanceUnit where
  baseFactor Meter = fromℤ 1
  baseFactor Kilometer = fromℤ 1_000
  baseFactor Mile = fromℤ 1_609

-------------------------------------------------------------------------------
--                                 Singleton                                 --
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
--                                 Conversion                                --
-------------------------------------------------------------------------------

-- NOTE: [ConvertDistance and constraints]
--
-- The simplest way to define ConvertDistance is something like:
--
--     class ConvertDistance a where
--       type ToConstraints (e :: DistanceUnit) a :: Constraint
--       type ConvertedDistance (e :: DistanceUnit) a
--
--       convertDistance_ :: (SingI e, ToConstraints e a) => a -> ConvertedDistance e a
--
-- Alas, while this works for most types, unfortunately it does not work for
-- Pace, because Pace must also include PaceDistF on its return type e.
-- We cannot include type constraints on e directly, so a workaround is:
--
--     class ConvertDistance a where
--       type ToConstraints (e :: DistanceUnit) a :: Constraint
--       type ConvertedDistance (e :: DistanceUnit) a
--
--       convertDistance :: (SingI e, ToConstraints e a) => Proxy e -> a -> ConvertedDistance e a
--
-- That is, we include a second type family for possible constraints. Most
-- types would leave this as () (empty constraint), but Pace would have:
--
--     instance (FromInteger a, PaceDistF d, Semifield a, SingI d) => ConvertDistance (Pace d a) where
--       type ToConstraints e (Pace d a) = (PaceDistF e)
--       type ConvertDistance e (Pace d a) = Pace e a
--
--       convertDistance :: forall e. (PaceDistF e, SingI e) => Proxy e -> Pace d a -> Pace e a
--       convertDistance _ = MkPace . (.% fromBase) . (.* toBase) . (.unPace)
--         where
--           toBase = singFactor @_ @d
--           fromBase = singFactor @_ @e
--
-- The Proxy is due to needing to pass e into fromBase (cannot be inferred,
-- apparently), and unforunately we cannot use TypeAbstraction to recover
-- the type until at least GHC 9.10 (e.g. convertDistance @e).
--
-- Due to the Proxy and forall weirdness, we instead go with the simpler MPTC
-- solution. Once we require GHC 9.10+, we can try the associated type
-- solution.

-- | Common interface for converting distance units.
type ConvertDistance :: Type -> DistanceUnit -> Constraint
class ConvertDistance a e where
  type ConvertedDistance a e

  convertDistance_ :: a -> ConvertedDistance a e

-- | Convert to meters.
convertToMeters :: (ConvertDistance a Meter) => a -> ConvertedDistance a Meter
convertToMeters = convertDistance Meter

-- | Convert to kilometers.
convertToKilometers ::
  (ConvertDistance a Kilometer) =>
  a ->
  ConvertedDistance a Kilometer
convertToKilometers = convertDistance Kilometer

-- | Converts distance with visible forall.
convertDistance ::
  forall e ->
  forall a.
  (ConvertDistance a e) =>
  a ->
  ConvertedDistance a e
convertDistance e @a = convertDistance_ @a @e
