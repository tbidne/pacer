-- | Provide distance types.
module Pacer.Data.Distance
  ( -- * Distance
    Distance (..),

    -- * Some Distance
    SomeDistance (..),

    -- * Units
    DistanceUnit (..),

    -- * Aliases
    Meters,
    Kilometers,
    Miles,

    -- * Hiding Distance
    HasDistance (..),

    -- * Misc
    marathon,
    halfMarathon,
    k_10,
    k_5,
  )
where

import Pacer.Class.Parser (MParser, Parser (parser))
import Pacer.Class.Units (singFactor)
import Pacer.Data.Distance.Units
  ( ConvertDistance (ConvertedDistance, ToConstraints, convertDistance_),
    DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SKilometer, SMeter, SMile),
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
newtype Distance d a = MkDistance {unDistance :: a}
  deriving stock (Functor)
  deriving newtype (MetricSpace)

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

instance (Display a, SingI d, ToRational a) => Display (Distance d a) where
  displayBuilder (MkDistance x) =
    mconcat
      [ x',
        " ",
        displayBuilder d
      ]
    where
      d = fromSingI @_ @d

      xDouble = realToFrac @_ @Double $ toQ x
      x' = case d of
        Meter -> displayBuilder $ round @Double @Int xDouble
        Kilometer -> displayBuilder @String $ printf "%.2f" xDouble
        Mile -> displayBuilder @String $ printf "%.2f" xDouble

-------------------------------------------------------------------------------
--                                   Algebra                                 --
-------------------------------------------------------------------------------

instance (ASemigroup a) => ASemigroup (Distance d a) where
  (.+.) = liftDist2 (.+.)

instance (AMonoid a) => AMonoid (Distance d a) where
  zero = MkDistance zero

instance (AGroup a) => AGroup (Distance d a) where
  (.-.) = liftDist2 (.-.)

instance (MSemigroup a) => MSemiSpace (Distance d a) a where
  MkDistance x .* k = MkDistance (x .*. k)

instance (MGroup a) => MSpace (Distance d a) a where
  MkDistance x .% k = MkDistance (x .%. k)

instance (Semiring a) => Semimodule (Distance d a) a

instance (Semifield a) => SemivectorSpace (Distance d a) a

instance (Ring a) => Module (Distance d a) a

instance (Field a) => VectorSpace (Distance d a) a

-------------------------------------------------------------------------------
--                             Numeric Conversions                           --
-------------------------------------------------------------------------------

instance (FromRational a) => FromRational (Distance d a) where
  fromQ = MkDistance . fromQ

instance (ToRational a) => ToRational (Distance d a) where
  toQ (MkDistance x) = toQ x

instance (FromInteger a) => FromInteger (Distance d a) where
  fromZ = MkDistance . fromZ

instance (ToInteger a) => ToInteger (Distance d a) where
  toZ (MkDistance x) = toZ x

instance (FromReal a) => FromReal (Distance d a) where
  fromR = MkDistance . fromR

instance (ToReal a) => ToReal (Distance d a) where
  toR (MkDistance x) = toR x

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance (FromInteger a, MGroup a, SingI d) => ConvertDistance (Distance d a) where
  type ConvertedDistance (Distance d a) e = Distance e a
  type ToConstraints (Distance d a) _ = ()

  convertDistance_ :: forall e. (SingI e) => Distance d a -> Distance e a
  convertDistance_ = MkDistance . (.%. fromBase) . (.*. toBase) . (.unDistance)
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
  ( FromRational a,
    SingI d,
    Parser a
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

liftDist2 ::
  (a -> a -> a) ->
  Distance d a ->
  Distance d a ->
  Distance d a
liftDist2 f (MkDistance x) (MkDistance y) = MkDistance (f x y)

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

deriving stock instance Functor SomeDistance

instance (FromInteger a, MetricSpace a, MGroup a) => Eq (SomeDistance a) where
  t1 == t2 = convertToMeters t1 == convertToMeters t2

instance
  ( FromInteger a,
    MetricSpace a,
    MGroup a,
    Ord a
  ) =>
  Ord (SomeDistance a)
  where
  t1 <= t2 = convertToMeters t1 <= convertToMeters t2

instance HasField "unSomeDistance" (SomeDistance a) a where
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

instance (Display a, ToRational a) => Display (SomeDistance a) where
  displayBuilder (MkSomeDistance u x) = withSingI u displayBuilder x

-------------------------------------------------------------------------------
--                                   Algebra                                 --
-------------------------------------------------------------------------------

instance
  ( ASemigroup a,
    FromInteger a,
    MGroup a
  ) =>
  ASemigroup (SomeDistance a)
  where
  (.+.) = liftSomeDist2 (.+.)

instance (FromInteger a, Semifield a) => AMonoid (SomeDistance a) where
  zero = MkSomeDistance SMeter zero

instance (Field a, FromInteger a) => AGroup (SomeDistance a) where
  (.-.) = liftSomeDist2 (.-.)

instance (MSemigroup a) => MSemiSpace (SomeDistance a) a where
  MkSomeDistance u x .* k = MkSomeDistance u (x .* k)

instance (MGroup a) => MSpace (SomeDistance a) a where
  MkSomeDistance u x .% k = MkSomeDistance u (x .% k)

instance (FromInteger a, Semifield a) => Semimodule (SomeDistance a) a

instance (FromInteger a, Semifield a) => SemivectorSpace (SomeDistance a) a

instance (Field a, FromInteger a) => Module (SomeDistance a) a

instance (Field a, FromInteger a) => VectorSpace (SomeDistance a) a

-------------------------------------------------------------------------------
--                             Numeric Conversions                           --
-------------------------------------------------------------------------------

instance (FromRational a) => FromRational (SomeDistance a) where
  fromQ = MkSomeDistance SMeter . fromQ

instance (FromInteger a, MGroup a, ToRational a) => ToRational (SomeDistance a) where
  toQ = toQ . convertToMeters

instance (FromReal a) => FromReal (SomeDistance a) where
  fromR = MkSomeDistance SMeter . fromR

instance (FromInteger a, MGroup a, ToReal a) => ToReal (SomeDistance a) where
  toR = toR . convertToMeters

instance (FromInteger a) => FromInteger (SomeDistance a) where
  fromZ = MkSomeDistance SMeter . fromZ

instance (FromInteger a, MGroup a, ToInteger a) => ToInteger (SomeDistance a) where
  toZ = toZ . convertToMeters

-------------------------------------------------------------------------------
--                                    Units                                  --
-------------------------------------------------------------------------------

instance (FromInteger a, MGroup a) => ConvertDistance (SomeDistance a) where
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
instance (FromRational a, Parser a) => Parser (SomeDistance a) where
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

liftSomeDist2 ::
  (FromInteger a, MGroup a) =>
  (forall d. Distance d a -> Distance d a -> Distance d a) ->
  SomeDistance a ->
  SomeDistance a ->
  SomeDistance a
liftSomeDist2 f x y =
  hideDistance (convertToMeters x `f` convertToMeters y)

-- NOTE: Values hardcoded rather than multiplied by factor to avoid some
-- (minor) float rounding.

-- | Marathon.
marathon :: forall d a. (FromRational a, SingI d) => Distance d a
marathon = case sing @d of
  SMeter -> MkDistance $ fromℚ 42_195
  SKilometer -> MkDistance $ fromℚ 42.195
  SMile -> MkDistance $ fromℚ 26.2188

-- | Half marathon.
halfMarathon :: forall d a. (FromRational a, SingI d) => Distance d a
halfMarathon = case sing @d of
  SMeter -> MkDistance $ fromℚ 21_097.5
  SKilometer -> MkDistance $ fromℚ 21.0975
  SMile -> MkDistance $ fromℚ 13.1094

-- | 10 km.
k_10 :: forall d a. (FromRational a, SingI d) => Distance d a
k_10 = case sing @d of
  SMeter -> MkDistance $ fromℚ 10_000
  SKilometer -> MkDistance $ fromℚ 10
  SMile -> MkDistance $ fromℚ 6.21371

-- | 5 km.
k_5 :: forall d a. (FromRational a, SingI d) => Distance d a
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
