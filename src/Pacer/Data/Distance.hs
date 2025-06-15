{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provide distance types.
module Pacer.Data.Distance
  ( -- * Distance
    Distance (..),

    -- ** Aliases
    Meters,
    Kilometers,
    Miles,

    -- ** Functions
    liftDist,
    liftDist2,
    liftDistLeft2,
    forceUnit,

    -- ** Distances
    marathon,
    halfMarathon,
    k_10,
    k_5,

    -- * Some Distance
    SomeDistance (..),

    -- ** Functions
    liftSomeDist,
    liftSomeDist2,
    liftSomeDistLeft2,

    -- * Units
    DistanceUnit (..),

    -- * Hiding Distance
    HasDistance (..),
  )
where

import Pacer.Class.Parser (MParser, Parser (parser))
import Pacer.Class.Units (singFactor)
import Pacer.Data.Distance.Units
  ( ConvertDistance (ConvertedDistance, ToConstraints, convertDistance_),
    DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SKilometer, SMeter, SMile),
    convertDistance,
    convertToMeters,
  )
import Pacer.Prelude
import Text.Megaparsec ((<?>))
import Text.Megaparsec.Char qualified as MPC
import Text.Printf (printf)

-------------------------------------------------------------------------------
--                                  Distance                                 --
-------------------------------------------------------------------------------

-- | Represents a numeric distance with units.
type Distance :: DistanceUnit -> Type -> Type
newtype Distance d a = MkDistance {unDistance :: Positive a}
  deriving stock (Generic)
  deriving newtype (MetricSpace)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''Distance

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance (MetricSpace a, SingI d) => Eq (Distance d a) where
  x == y = ɛEq ɛ x y
    where
      ɛ = 1 / singFactor @_ @d

instance (MetricSpace a, Ord a, SingI d) => Ord (Distance d a) where
  dx@(MkDistance x) <= dy@(MkDistance y) = dx == dy || x < y

instance (Show a, SingI d) => Show (Distance d a) where
  showsPrec i (MkDistance x) =
    showParen
      (i >= 11)
      ( showString "MkDistance "
          . showsPrec 11 x
          . showSpace
          . showsPrec 11 d
      )
    where
      d = fromSingI @_ @d

instance (Display a, SingI d, Toℝ a) => Display (Distance d a) where
  displayBuilder (MkDistance x) =
    mconcat
      [ x',
        " ",
        displayBuilder d
      ]
    where
      d = fromSingI @_ @d

      xDouble = toℝ x
      x' = case d of
        Meter -> displayBuilder $ round @Double @Int xDouble
        Kilometer -> displayBuilder @String $ printf "%.2f" xDouble
        Mile -> displayBuilder @String $ printf "%.2f" xDouble

-------------------------------------------------------------------------------
--                                   Algebra                                 --
-------------------------------------------------------------------------------

instance (ASemigroup a) => ASemigroup (Distance d a) where
  (.+.) = liftDist2 (.+.)

instance (MSemigroup a) => MSemiSpace (Distance d a) (Positive a) where
  MkDistance x .* k = MkDistance (x .*. k)

instance (MGroup a) => MSpace (Distance d a) (Positive a) where
  MkDistance x .% k = MkDistance (x .%. k)

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a,
    SingI d
  ) =>
  ConvertDistance (Distance d a)
  where
  type ConvertedDistance (Distance d a) e = Distance e a
  type ToConstraints (Distance d a) _ = ()

  convertDistance_ :: forall e. (SingI e) => Distance d a -> Distance e a
  convertDistance_ = MkDistance . (.%. fromBase) . (.*. toBase) . view #unDistance
    where
      toBase = singFactor @_ @d
      fromBase = singFactor @_ @e

instance (SingI d) => HasDistance (Distance d a) where
  type DistanceVal (Distance d a) = Distance d a
  type HideDistance (Distance d a) = SomeDistance a

  distanceUnitOf _ = fromSingI @_ @d

  distanceOf = id

  hideDistance = MkSomeDistance (sing @d)

-------------------------------------------------------------------------------
--                                   Parsing                                 --
-------------------------------------------------------------------------------

-- NOTE: [Distance Parsing]
--
-- No units, simply reuses Double's instances in Parser.hs.
--
-- Normally this is just a wrapper over Double's instance in Parser.hs i.e.
-- a number with no units. However, we also parse some hardcoded strings
-- e.g. marathon.
--
-- See NOTE: [SomeDistance Parsing]

instance
  ( AMonoid a,
    Fromℚ a,
    Parser a,
    Ord a,
    Show a,
    SingI d
  ) =>
  Parser (Distance d a)
  where
  parser =
    distanceParser <&> \case
      DistParseMarathon -> marathon
      DistParseHalfMarathon -> halfMarathon
      DistParseNumeric x -> MkDistance x

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

liftDist ::
  (Positive a -> Positive a) ->
  Distance d a ->
  Distance d a
liftDist f (MkDistance x) = MkDistance (f x)

liftDist2 ::
  (Positive a -> Positive a -> Positive a) ->
  Distance d a ->
  Distance d a ->
  Distance d a
liftDist2 f (MkDistance x) (MkDistance y) = MkDistance (f x y)

liftDistLeft2 ::
  forall d1 d2 a.
  (Fromℤ a, Ord a, Semifield a, Show a, SingI d1, SingI d2) =>
  (forall d. Distance d a -> Distance d a -> Distance d a) ->
  Distance d1 a ->
  Distance d2 a ->
  Distance d1 a
liftDistLeft2 f x = f x . convertDistance_ @_ @d1

-------------------------------------------------------------------------------
--                                SomeDistance                               --
-------------------------------------------------------------------------------

-- | Distance, existentially quantifying the units.
type SomeDistance :: Type -> Type
data SomeDistance a where
  MkSomeDistance :: Sing d -> Distance d a -> SomeDistance a

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance (NFData a) => NFData (SomeDistance a) where
  rnf (MkSomeDistance s d) = s `deepseq` d `deepseq` ()

instance
  ( Fromℤ a,
    MetricSpace a,
    MGroup a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  Eq (SomeDistance a)
  where
  t1 == t2 = convertToMeters t1 == convertToMeters t2

instance
  ( Fromℤ a,
    MetricSpace a,
    MGroup a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  Ord (SomeDistance a)
  where
  t1 <= t2 = convertToMeters t1 <= convertToMeters t2

instance HasField "unSomeDistance" (SomeDistance a) (Positive a) where
  getField (MkSomeDistance _ (MkDistance x)) = x

instance (Show a) => Show (SomeDistance a) where
  showsPrec i (MkSomeDistance u x) =
    showParen
      (i >= 11)
      ( showString "MkSomeDistance "
          . showsPrec 11 u
          . showSpace
          . withSingI u showsPrec 11 x
      )

instance (Display a, Toℝ a) => Display (SomeDistance a) where
  displayBuilder (MkSomeDistance u x) = withSingI u displayBuilder x

-------------------------------------------------------------------------------
--                                   Algebra                                 --
-------------------------------------------------------------------------------

instance
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  ASemigroup (SomeDistance a)
  where
  (.+.) = liftSomeDist2 (.+.)

instance (MSemigroup a) => MSemiSpace (SomeDistance a) (Positive a) where
  MkSomeDistance u x .* k = MkSomeDistance u (x .* k)

instance (MGroup a) => MSpace (SomeDistance a) (Positive a) where
  MkSomeDistance u x .% k = MkSomeDistance u (x .% k)

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  ConvertDistance (SomeDistance a)
  where
  type ConvertedDistance (SomeDistance a) e = Distance e a
  type ToConstraints (SomeDistance a) _ = ()

  convertDistance_ :: (SingI e) => SomeDistance a -> Distance e a
  convertDistance_ (MkSomeDistance s x) = withSingI s convertDistance_ x

instance HasDistance (SomeDistance a) where
  type DistanceVal (SomeDistance a) = SomeDistance a
  type HideDistance (SomeDistance a) = SomeDistance a

  distanceUnitOf (MkSomeDistance sd _) = fromSing sd

  distanceOf = id

  hideDistance = id

-------------------------------------------------------------------------------
--                                   Parsing                                 --
-------------------------------------------------------------------------------

-- NOTE: [SomeDistance Parsing]
--
-- Uses Distance's instance and adds units e.g. '4 km'.
-- See NOTE: [Distance Parsing]
instance
  ( AMonoid a,
    Fromℚ a,
    Ord a,
    Parser a,
    Show a
  ) =>
  Parser (SomeDistance a)
  where
  parser =
    distanceParser >>= \case
      DistParseMarathon -> pure $ MkSomeDistance SKilometer marathon
      DistParseHalfMarathon -> pure $ MkSomeDistance SKilometer halfMarathon
      DistParseNumeric x -> do
        _ <- MPC.space
        unit <- parser

        pure $ case unit of
          Meter -> MkSomeDistance SMeter (MkDistance x)
          Kilometer -> MkSomeDistance SKilometer (MkDistance x)
          Mile -> MkSomeDistance SMile (MkDistance x)

data DistParse a
  = DistParseMarathon
  | DistParseHalfMarathon
  | DistParseNumeric a

-- | Normal parsing w/ special cases
distanceParser :: (Parser a) => MParser (DistParse a)
distanceParser = do
  asum
    [ MPC.string "marathon" $> DistParseMarathon,
      MPC.string "half-marathon" $> DistParseHalfMarathon,
      MPC.string "hmarathon" $> DistParseHalfMarathon,
      DistParseNumeric <$> parser <?> "digits_unit"
    ]

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

liftSomeDist ::
  (Fromℤ a, Ord a, Semifield a, Show a) =>
  (forall d. Distance d a -> Distance d a) ->
  SomeDistance a ->
  SomeDistance a
liftSomeDist f x = hideDistance (f $ convertToMeters x)

-- | Lifts a binary 'Distance' function to 'SomeDistance' by converting both
-- arguments to meters.
liftSomeDist2 ::
  (Fromℤ a, Ord a, Semifield a, Show a) =>
  (forall d. Distance d a -> Distance d a -> Distance d a) ->
  SomeDistance a ->
  SomeDistance a ->
  SomeDistance a
liftSomeDist2 f x y =
  hideDistance (convertToMeters x `f` convertToMeters y)

-- | Like 'liftSomeDist2', except we convert to the LHS units.
liftSomeDistLeft2 ::
  (Fromℤ a, Ord a, Semifield a, Show a) =>
  (forall d. Distance d a -> Distance d a -> Distance d a) ->
  SomeDistance a ->
  SomeDistance a ->
  SomeDistance a
liftSomeDistLeft2 f (MkSomeDistance sx x) (MkSomeDistance sy y) =
  withSingI sy $ case sx of
    SMeter -> MkSomeDistance sx (x `f` convertToMeters y)
    SKilometer -> MkSomeDistance sx (x `f` convertDistance Kilometer y)
    SMile -> MkSomeDistance sx (x `f` convertDistance Mile y)

-- NOTE: Values hardcoded rather than multiplied by factor to avoid some
-- (minor) float rounding.
--
-- Also, it might be nice to use TH here instead of the unsafe fromℚ.
-- Alas, typed TH does not work with class constraints, apparently:
--
-- https://stackoverflow.com/a/65406350
--
-- Fleeting attempts with untyped TH do not work either.

-- | Marathon.
marathon :: forall d a. (AMonoid a, Fromℚ a, Ord a, Show a, SingI d) => Distance d a
marathon = case sing @d of
  SMeter -> MkDistance $ fromℚ 42_195
  SKilometer -> MkDistance $ fromℚ 42.195
  SMile -> MkDistance $ fromℚ 26.2188

-- | Half marathon.
halfMarathon :: forall d a. (AMonoid a, Fromℚ a, Ord a, Show a, SingI d) => Distance d a
halfMarathon = case sing @d of
  SMeter -> MkDistance $ fromℚ 21_097.5
  SKilometer -> MkDistance $ fromℚ 21.0975
  SMile -> MkDistance $ fromℚ 13.1094

-- | 10 km.
k_10 :: forall d a. (AMonoid a, Fromℚ a, Ord a, Show a, SingI d) => Distance d a
k_10 = case sing @d of
  SMeter -> MkDistance $ fromℚ 10_000
  SKilometer -> MkDistance $ fromℚ 10
  SMile -> MkDistance $ fromℚ 6.21371

-- | 5 km.
k_5 :: forall d a. (AMonoid a, Fromℚ a, Ord a, Show a, SingI d) => Distance d a
k_5 = case sing @d of
  SMeter -> MkDistance $ fromℚ 5_000
  SKilometer -> MkDistance $ fromℚ 5
  SMile -> MkDistance $ fromℚ 3.10686

-- | Alias for 'Distance Meters'.
type Meters a = Distance Meter a

-- | Alias for 'Distance Kilometers'.
type Kilometers a = Distance Kilometer a

-- | Alias for 'Distance Miles'.
type Miles a = Distance Mile a

-- | Changes the distance __without__ doing any conversions. Internal tool
-- intended for forcing a universally quantified value to be a specific
-- distance.
forceUnit :: Distance d a -> Distance e a
forceUnit (MkDistance x) = MkDistance x

-------------------------------------------------------------------------------
--                                Units classes                              --
-------------------------------------------------------------------------------

-- | Class that provides a unified interface for handling distance units.
class HasDistance a where
  type DistanceVal a
  type HideDistance a

  -- | Retrieves the type of distance unit.
  distanceUnitOf :: a -> DistanceUnit

  distanceOf :: a -> DistanceVal a

  -- | Hides the distance unit.
  hideDistance :: a -> HideDistance a
