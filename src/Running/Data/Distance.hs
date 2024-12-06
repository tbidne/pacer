-- | Provide distance types.
module Running.Data.Distance
  ( -- * Distance
    Distance (..),

    -- ** Functions
    toMeters,
    convertDistance,

    -- * Some Distance
    SomeDistance (..),
    someToMeters,

    -- * Units
    DistanceUnit (..),

    -- * Misc
    marathon,
    halfMarathon,
    k_10,
    k_5,
  )
where

import Running.Class.Parser (Parser (parser))
import Running.Class.Units (singFactor)
import Running.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Running.Prelude
import Text.Megaparsec.Char qualified as MPC

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

instance (Display a, SingI d) => Display (Distance d a) where
  displayBuilder (MkDistance x) =
    mconcat
      [ displayBuilder x,
        " ",
        displayBuilder (fromSingI @_ @d)
      ]

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

-- relies on instances defined in Parser.hs
instance (Parser a) => Parser (Distance d a) where
  parser = MkDistance <$> parser

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

-- | Distance, existentially quantifying the units.
type SomeDistance :: Type -> Type
data SomeDistance a where
  MkSomeDistance :: Sing d -> Distance d a -> SomeDistance a

instance (FromInteger a, MetricSpace a, MGroup a) => Eq (SomeDistance a) where
  t1 == t2 = someToMeters t1 == someToMeters t2

instance (FromInteger a, MetricSpace a, MGroup a, Ord a) => Ord (SomeDistance a) where
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

instance (Display a) => Display (SomeDistance a) where
  displayBuilder (MkSomeDistance u x) = withSingI u displayBuilder x

instance (ASemigroup a, FromInteger a, MGroup a) => ASemigroup (SomeDistance a) where
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

instance (ToRational a) => ToRational (SomeDistance a) where
  toQ (MkSomeDistance _ x) = toQ x

instance (FromInteger a) => FromInteger (SomeDistance a) where
  fromZ = MkSomeDistance SMeter . fromZ

instance (ToInteger a) => ToInteger (SomeDistance a) where
  toZ (MkSomeDistance _ x) = toZ x

instance (Parser a) => Parser (SomeDistance a) where
  parser = do
    x <- parser

    MPC.char ' '

    u <- parser

    pure $ case u of
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
  SMeter -> MkDistance $ fromQ 42_195
  SKilometer -> MkDistance $ fromQ 42.195
  SMile -> MkDistance $ fromQ 26.2188

-- | Half marathon.
halfMarathon :: forall d a. (FromRational a, SingI d) => Distance d a
halfMarathon = case sing @d of
  SMeter -> MkDistance $ fromQ 21_097.5
  SKilometer -> MkDistance $ fromQ 21.0975
  SMile -> MkDistance $ fromQ 13.1094

-- | 10 km.
k_10 :: forall d a. (FromRational a, SingI d) => Distance d a
k_10 = case sing @d of
  SMeter -> MkDistance $ fromQ 10_000
  SKilometer -> MkDistance $ fromQ 10
  SMile -> MkDistance $ fromQ 6.21371

-- | 5 km.
k_5 :: forall d a. (FromRational a, SingI d) => Distance d a
k_5 = case sing @d of
  SMeter -> MkDistance $ fromQ 5_000
  SKilometer -> MkDistance $ fromQ 5
  SMile -> MkDistance $ fromQ 3.10686
