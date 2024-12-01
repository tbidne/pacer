{-# LANGUAGE AllowAmbiguousTypes #-}

module Running.Data.Distance.Units
  ( DistanceUnit (..),
    toFactor,

    -- * Singletons
    SDistanceUnit (..),
    withSingleton,
    fromSingleton,
    fromSingleton',
    SingDistanceUnit (..),
  )
where

import Data.Kind (Constraint)
import Data.Text.Display (Display (displayBuilder))
import Running.Data.SI
  ( SI,
    SSI,
    SingSI (singSI),
    ssiToSI,
  )
import Running.Data.SI qualified as SI

data DistanceUnit
  = Meter SI
  | Mile
  deriving stock (Eq, Show)

instance Display DistanceUnit where
  displayBuilder (Meter si) = displayBuilder si <> "m"
  displayBuilder Mile = "mi"

toFactor :: (Fractional a) => DistanceUnit -> a
toFactor (Meter si) = SI.baseFactor si
toFactor Mile = 1609

data SDistanceUnit (d :: DistanceUnit) where
  SMeter :: (SingSI s) => SSI s -> SDistanceUnit (Meter s)
  SMile :: SDistanceUnit Mile

type SingDistanceUnit :: DistanceUnit -> Constraint
class SingDistanceUnit (d :: DistanceUnit) where
  singDistanceUnit :: SDistanceUnit d

instance (SingSI s) => SingDistanceUnit (Meter s) where
  singDistanceUnit = SMeter (singSI @s)

instance SingDistanceUnit Mile where
  singDistanceUnit = SMile

withSingleton :: SDistanceUnit d -> ((SingDistanceUnit d) => r) -> r
withSingleton d x = case d of
  SMeter _ -> x
  SMile -> x

fromSingleton :: SDistanceUnit d -> DistanceUnit
fromSingleton (SMeter si) = Meter (ssiToSI si)
fromSingleton SMile = Mile

fromSingleton' :: forall d. (SingDistanceUnit d) => DistanceUnit
fromSingleton' = fromSingleton $ singDistanceUnit @d
