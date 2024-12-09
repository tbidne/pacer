-- | Provide distance types.
module Pacer.Data.Distance
  ( -- * Distance
    Distance (..),

    -- ** Functions
    toMeters,
    convertDistance,

    -- * Some Distance
    SomeDistance (..),
    someToMeters,
    convertSomeDistance,

    -- * Units
    DistanceUnit (..),

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
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Pacer.Prelude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Printf (printf)

-- | Represents a numeric distance with units.
type Distance :: DistanceUnit -> Type -> Type
newtype Distance d a = MkDistance {unDistance :: a}
  deriving newtype (MetricSpace)

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

liftDist2 ::
  (a -> a -> a) ->
  Distance d a ->
  Distance d a ->
  Distance d a
liftDist2 f (MkDistance x) (MkDistance y) = MkDistance (f x y)

-- | Converts any distance to meters.
toMeters ::
  forall d a.
  ( FromInteger a,
    MSemigroup a,
    SingI d
  ) =>
  Distance d a ->
  Distance Meter a
toMeters = MkDistance . (.*. toBase) . (.unDistance)
  where
    toBase = singFactor @_ @d

-- Technically toMeters is a special case of convertDistance, but by
-- re-implementing it we avoid the 'MGroup' requirement.

-- | Convert from on distance to another.
convertDistance ::
  forall d2 d1 a.
  ( FromInteger a,
    MGroup a,
    SingI d1,
    SingI d2
  ) =>
  Distance d1 a ->
  Distance d2 a
convertDistance = MkDistance . (.%. fromBase) . (.*. toBase) . (.unDistance)
  where
    toBase = singFactor @_ @d1
    fromBase = singFactor @_ @d2

-- | Converts some distance to meters.
someToMeters ::
  ( FromInteger a,
    MGroup a
  ) =>
  SomeDistance a ->
  Distance Meter a
someToMeters (MkSomeDistance u x) = withSingI u toMeters x

convertSomeDistance ::
  forall d a.
  ( FromInteger a,
    MGroup a,
    SingI d
  ) =>
  SomeDistance a ->
  Distance d a
convertSomeDistance (MkSomeDistance s x) = withSingI s convertDistance x

-- | Distance, existentially quantifying the units.
type SomeDistance :: Type -> Type
data SomeDistance a where
  MkSomeDistance :: Sing d -> Distance d a -> SomeDistance a

instance (FromInteger a, MetricSpace a, MGroup a) => Eq (SomeDistance a) where
  t1 == t2 = someToMeters t1 == someToMeters t2

instance
  ( FromInteger a,
    MetricSpace a,
    MGroup a,
    Ord a
  ) =>
  Ord (SomeDistance a)
  where
  t1 <= t2 = someToMeters t1 <= someToMeters t2

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

instance (FromRational a) => FromRational (SomeDistance a) where
  fromQ = MkSomeDistance SMeter . fromQ

instance (FromInteger a, MGroup a, ToRational a) => ToRational (SomeDistance a) where
  toQ = toQ . someToMeters

instance (FromReal a) => FromReal (SomeDistance a) where
  fromR = MkSomeDistance SMeter . fromR

instance (FromInteger a, MGroup a, ToReal a) => ToReal (SomeDistance a) where
  toR = toR . someToMeters

instance (FromInteger a) => FromInteger (SomeDistance a) where
  fromZ = MkSomeDistance SMeter . fromZ

instance (FromInteger a, MGroup a, ToInteger a) => ToInteger (SomeDistance a) where
  toZ = toZ . someToMeters

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

liftSomeDist2 ::
  (FromInteger a, MGroup a) =>
  (forall d. Distance d a -> Distance d a -> Distance d a) ->
  SomeDistance a ->
  SomeDistance a ->
  SomeDistance a
liftSomeDist2 f x y =
  MkSomeDistance SMeter (someToMeters x `f` someToMeters y)

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

data DistParse a
  = DistParseMarathon
  | DistParseHalfMarathon
  | DistParseNumeric a

-- | Normal parsing w/ special cases
distanceParser :: (Parser a) => MParser (DistParse a)
distanceParser = do
  MP.choice
    [ MP.try $ MPC.string "marathon" $> DistParseMarathon,
      MP.try $ MPC.string "half-marathon" $> DistParseHalfMarathon,
      MP.try $ MPC.string "hmarathon" $> DistParseHalfMarathon,
      DistParseNumeric <$> parser
    ]
