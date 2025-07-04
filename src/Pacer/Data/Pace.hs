{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Data.Pace
  ( -- * Pace
    Pace (..),
    PaceDistF,

    -- ** Creation
    mkPace,

    -- ** Elimination
    unPace,

    -- * SomePace
    SomePace (..),

    -- ** Creation
    mkSomePace,

    -- ** Elimination
    unSomePace,
  )
where

import GHC.TypeError (Unsatisfiable)
import GHC.TypeError qualified as TE
import GHC.TypeLits (symbolVal)
import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Class.Units (singFactor)
import Pacer.Data.Distance
  ( Distance (MkDistance),
    DistanceUnit (Kilometer, Meter, Mile),
    HasDistance
      ( DistanceVal,
        HideDistance,
        distanceOf,
        distanceUnitOf,
        hideDistance
      ),
    SomeDistance,
  )
import Pacer.Data.Distance.Units
  ( ConvertDistance (ConvertedDistance, convertDistance_),
    SDistanceUnit (SKilometer, SMile),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration (Duration (MkDuration))
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import Text.Megaparsec.Char qualified as MPC

-------------------------------------------------------------------------------
--                                    Pace                                   --
-------------------------------------------------------------------------------

-- | Represents a duration per distance e.g. 4'30" per kilometer. We only
-- allow the distance to be kilometers or miles. Meters are disallowed as
-- they chance for mistakes are high (i.e. almost all paces per meter are
-- zero since they are so low).
type Pace :: DistanceUnit -> Type -> Type
data Pace d a where
  MkPace :: (PaceDistF d) => Duration a -> Pace d a

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance (NFData a) => NFData (Pace d a) where
  rnf (MkPace d) = d `deepseq` ()

-- Getter just so we do not have to add PaceDistF to a bunch of functions that
-- only require elimination.
instance
  ( k ~ A_Getter,
    x ~ (Duration a),
    y ~ (Duration a)
  ) =>
  LabelOptic "unPace" k (Pace d a) (Pace d a) x y
  where
  labelOptic = to unPace

-- NOTE:
--
-- - Duration is monomorphized to Second. There is no technical requirement
--   for this (indeed, a previous iteration had the type param exposed).
--   But this is both simpler internally and, more importantly, there does
--   not appear to be any reason why we would want to support time units.

type PaceDistF :: DistanceUnit -> Constraint
type family PaceDistF d where
  PaceDistF Meter = Unsatisfiable (TE.Text Utils.PaceMetersErrMsg)
  PaceDistF Kilometer = CUnit
  PaceDistF Mile = CUnit

instance
  ( Eq a,
    Fromℤ a,
    MetricSpace a,
    MGroup a,
    SingI d
  ) =>
  Eq (Pace d a)
  where
  x == y = x ^. #unPace == y ^. #unPace

instance
  ( Fromℤ a,
    MetricSpace a,
    MGroup a,
    Ord a,
    SingI d
  ) =>
  Ord (Pace d a)
  where
  x <= y = x ^. #unPace <= y ^. #unPace

instance (Show a, SingI d) => Show (Pace d a) where
  showsPrec i (MkPace p) =
    showParen
      (i >= 11)
      ( showString "MkPace "
          . showsPrec 11 p
          . showSpace
          . showsPrec 11 d
      )
    where
      d = fromSingI @_ @d

instance
  ( Display a,
    Fromℤ a,
    MSemigroup a,
    SingI d,
    Toℚ a
  ) =>
  Display (Pace d a)
  where
  displayBuilder (MkPace x) =
    mconcat
      [ displayBuilder x,
        " /",
        displayBuilder d
      ]
    where
      d = fromSingI @_ @d

-------------------------------------------------------------------------------
--                                   Algebra                                 --
-------------------------------------------------------------------------------

instance (MSemigroup a) => MSemiSpace (Pace d a) (Positive a) where
  MkPace x .* k = MkPace (x .* k)

instance (MGroup a) => MSpace (Pace d a) (Positive a) where
  MkPace x .% k = MkPace (x .% k)

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance
  ( Fromℤ a,
    Ord a,
    PaceDistF d,
    Semifield a,
    Show a,
    SingI d
  ) =>
  ConvertDistance (Pace d a)
  where
  type ConvertedDistance (Pace d a) e = Pace e a
  type ToConstraints (Pace d a) e = PaceDistF e

  convertDistance_ :: (PaceDistF e, SingI e) => Pace d a -> Pace e a
  -- Note this is backwards from distance (.% fromBase) . (.* toBase) because
  -- our units are a divisor, not a multiplier (i.e. 4 /km vs. 4 km).
  convertDistance_ @e = MkPace . (.* fromBase) . (.% toBase) . (view #unPace)
    where
      toBase = singFactor @_ @d
      fromBase = singFactor @_ @e

instance (MMonoid a, PaceDistF d, SingI d) => HasDistance (Pace d a) where
  type DistanceVal (Pace d a) = Distance d a
  type HideDistance (Pace d a) = SomePace a

  distanceUnitOf _ = fromSingI @_ @d

  distanceOf _ = MkDistance one

  hideDistance = MkSomePace (sing @d)

-------------------------------------------------------------------------------
--                                   Parsing                                 --
-------------------------------------------------------------------------------

-- NOTE: [Pace Parsing]
--
-- Parses a pace like "1h 4'30\"", where each h/m/s component is optional,
-- but we must have at least one.
--
-- Like Duration, we have separate instances for 'a' and 'Positive a'.
--
-- See NOTE: [SomePace Parsing]

instance
  ( AMonoid a,
    Fromℤ a,
    Ord a,
    PaceDistF d,
    Show a
  ) =>
  Parser (Pace d a)
  where
  parser = MkPace <$> parser

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | Creates a pace from a duration.
mkPace :: (PaceDistF d) => Duration a -> Pace d a
mkPace = MkPace

-- | Eliminates a pace to the underlying duration.
unPace :: Pace d a -> Duration a
unPace (MkPace d) = d

-------------------------------------------------------------------------------
--                                  SomePace                                 --
-------------------------------------------------------------------------------

-- | Pace, existentially quantifying the distance unit.
type SomePace :: Type -> Type
data SomePace a where
  MkSomePace :: (PaceDistF d) => Sing d -> Pace d a -> SomePace a

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance (NFData a) => NFData (SomePace a) where
  rnf (MkSomePace s p) = s `deepseq` p `deepseq` ()

instance
  ( Eq a,
    Fromℤ a,
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  Eq (SomePace a)
  where
  (==) = applySomePace2 (==)

instance
  ( Fromℤ a,
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  Ord (SomePace a)
  where
  (<=) = applySomePace2 (<=)

instance HasField "unSomePace" (SomePace a) (Duration a) where
  getField = unSomePace

instance (Show a) => Show (SomePace a) where
  showsPrec i (MkSomePace s x) =
    showParen
      (i >= 11)
      ( showString "MkSomePace "
          . showsPrec 11 s
          . showSpace
          . withSingI s showsPrec 11 x
      )

instance
  ( Display a,
    Fromℤ a,
    MSemigroup a,
    Toℚ a
  ) =>
  Display (SomePace a)
  where
  displayBuilder (MkSomePace s x) = withSingI s displayBuilder x

-------------------------------------------------------------------------------
--                                   Algebra                                 --
-------------------------------------------------------------------------------

instance (MSemigroup a) => MSemiSpace (SomePace a) (Positive a) where
  MkSomePace s x .* k = MkSomePace s (x .* k)

instance (MGroup a) => MSpace (SomePace a) (Positive a) where
  MkSomePace s x .% k = MkSomePace s (x .% k)

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  ConvertDistance (SomePace a)
  where
  type ConvertedDistance (SomePace a) e = Pace e a
  type ToConstraints (SomePace a) e = PaceDistF e

  convertDistance_ :: (PaceDistF e, SingI e) => SomePace a -> Pace e a
  convertDistance_ (MkSomePace s p) = withSingI s $ convertDistance_ p

instance (MMonoid a) => HasDistance (SomePace a) where
  type DistanceVal (SomePace a) = SomeDistance a
  type HideDistance (SomePace a) = SomePace a

  distanceUnitOf (MkSomePace s _) = fromSing s

  distanceOf (MkSomePace s p) = withSingI s $ hideDistance $ distanceOf p

  hideDistance = id

-------------------------------------------------------------------------------
--                                   Parsing                                 --
-------------------------------------------------------------------------------

-- NOTE: [SomePace Parsing]
--
-- Adds units to pace e.g. "1h 4'30\" /km".
--
-- See NOTE: [Pace Parsing]

instance
  (AMonoid a, Fromℤ a, Ord a, Show a) =>
  Parser (SomePace a)
  where
  parser = do
    MkDuration x <- parser @(Duration a)
    MPC.space
    MPC.char '/'

    -- We do not use DistanceUnit's built-in parsing because we want the
    -- long units here (kilometer and mile) to be __singular__, not plural.
    eDUnit <-
      asum
        [ P.string "meters" $> Left "meters",
          P.string "km" $> Right Kilometer,
          P.string "kilometers" $> Left "kilometers",
          P.string "kilometer" $> Right Kilometer,
          P.string "miles" $> Left "miles",
          P.string "mile" $> Right Mile,
          P.string "mi" $> Right Mile,
          P.char 'm' $> Right Meter
        ]

    case eDUnit of
      Left "meters" -> fail $ symbolVal (Proxy @Utils.PaceMetersErrMsg)
      Left d -> fail $ "Pace unit " ++ d ++ " should be singular"
      Right Meter -> fail $ symbolVal (Proxy @Utils.PaceMetersErrMsg)
      Right Kilometer -> pure $ MkSomePace SKilometer $ MkPace (MkDuration x)
      Right Mile -> pure $ MkSomePace SMile $ MkPace (MkDuration x)

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

-- | Exposes the underlying duration.
unSomePace :: SomePace a -> Duration a
unSomePace (MkSomePace _ (MkPace x)) = x

-- | Hides the distance.
mkSomePace :: forall d a. (PaceDistF d, SingI d) => Pace d a -> SomePace a
mkSomePace = MkSomePace (sing @d)

applySomePace2 ::
  (Fromℤ a, Ord a, Semifield a, Show a) =>
  (forall d. (SingI d) => Pace d a -> Pace d a -> r) ->
  SomePace a ->
  SomePace a ->
  r
applySomePace2 f p1 p2 =
  DistU.convertToKilometers p1 `f` DistU.convertToKilometers p2
