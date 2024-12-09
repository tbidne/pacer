{-# LANGUAGE UndecidableInstances #-}

module Pacer.Data.Pace
  ( -- * Pace
    Pace (..),
    PaceDistF,

    -- ** Creation
    mkPace,

    -- ** Elimination
    unPace,

    -- ** Functions
    toKilometers,
    convertPace,

    -- * SomePace
    SomePace (..),

    -- ** Creation
    mkSomePace,

    -- ** Elimination
    unSomePace,

    -- ** Functions
    someToKilometers,
  )
where

import GHC.TypeError (Unsatisfiable)
import GHC.TypeError qualified as TE
import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Units (singFactor)
import Pacer.Data.Distance (DistanceUnit (Kilometer, Meter, Mile))
import Pacer.Data.Distance.Units (SDistanceUnit (SKilometer, SMile))
import Pacer.Data.Duration
  ( Duration (MkDuration),
    TimeUnit
      ( Second
      ),
  )
import Pacer.Data.Duration qualified as Duration
import Pacer.Prelude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | Represents a duration per distance e.g. 4'30" per kilometer. We only
-- allow the distance to be kilometers or miles. Meters are disallowed as
-- they chance for mistakes are high (i.e. almost all paces per meter are
-- zero since they are so low).
type Pace :: DistanceUnit -> Type -> Type
data Pace d a where
  MkPace :: (PaceDistF d) => Duration Second a -> Pace d a

instance HasField "unPace" (Pace d a) (Duration Second a) where
  getField = unPace

-- NOTE:
--
-- - Duration is monomorphized to Second. There is no technical requirement
--   for this (indeed, a previous iteration had the type param exposed).
--   But this is both simpler internally and, more importantly, there does
--   not appear to be any reason why we would want to support time units.

type PaceDistF :: DistanceUnit -> Constraint
type family PaceDistF d where
  PaceDistF Meter = Unsatisfiable (TE.Text "Meters are disallowed in Pace; use km or mi.")
  PaceDistF Kilometer = ()
  PaceDistF Mile = ()

instance
  ( Eq a,
    FromInteger a,
    MetricSpace a,
    MGroup a,
    SingI d
  ) =>
  Eq (Pace d a)
  where
  x == y = x.unPace == y.unPace

instance
  ( FromInteger a,
    MetricSpace a,
    MGroup a,
    Ord a,
    SingI d
  ) =>
  Ord (Pace d a)
  where
  x <= y = x.unPace <= y.unPace

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
    FromInteger a,
    MSemigroup a,
    SingI d,
    ToRational a
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

instance (MSemigroup a) => MSemiSpace (Pace d a) a where
  MkPace x .* k = MkPace (x .* k)

instance (MGroup a) => MSpace (Pace d a) a where
  MkPace x .% k = MkPace (x .% k)

instance (FromRational a, PaceDistF d) => FromRational (Pace d a) where
  fromQ = MkPace . fromQ

instance (ToRational a) => ToRational (Pace d a) where
  toQ = toQ . (.unPace)

instance (FromInteger a, PaceDistF d) => FromInteger (Pace d a) where
  fromZ = MkPace . fromℤ

instance (ToInteger a) => ToInteger (Pace d a) where
  toZ = toZ . (.unPace)

instance (FromReal a, PaceDistF d) => FromReal (Pace d a) where
  fromR = MkPace . fromR

instance (ToReal a) => ToReal (Pace d a) where
  toR = toR . (.unPace)

-- NOTE: [Pace Parsing]
--
-- Parses a pace like "1h 4'30\"", where each h/m/s component is optional,
-- but we must have at least one.
--
-- Like Duration, we have separate instances for 'a' and 'Positive a'.
--
-- See NOTE: [SomePace Parsing]

instance
  {-# OVERLAPPABLE #-}
  ( Fractional a,
    FromInteger a,
    MGroup a,
    PaceDistF d
  ) =>
  Parser (Pace d a)
  where
  parser = MkPace <$> parser

instance
  {-# OVERLAPPING #-}
  ( Fractional a,
    FromInteger a,
    MGroup a,
    Ord a,
    PaceDistF d,
    Show a
  ) =>
  Parser (Pace d (Positive a))
  where
  parser = MkPace <$> parser

-- | Creates a pace from a duration.
mkPace ::
  ( PaceDistF d,
    FromInteger a,
    MSemigroup a,
    SingI t
  ) =>
  Duration t a ->
  Pace d a
mkPace = MkPace . Duration.toSeconds

-- | Eliminates a pace to the underlying duration.
unPace :: Pace d a -> Duration Second a
unPace (MkPace d) = d

-- | Converts pace to kilometers.
toKilometers ::
  forall d1 a.
  ( FromInteger a,
    MGroup a,
    SingI d1
  ) =>
  Pace d1 a ->
  Pace Kilometer a
toKilometers = MkPace . (.% fromBase) . (.* toBase) . (.unPace)
  where
    toBase = singFactor @_ @d1
    fromBase = singFactor @_ @Kilometer

-- | Converts pace to arbitrary units.
convertPace ::
  forall d1 d2 a.
  ( FromInteger a,
    MGroup a,
    PaceDistF d2,
    SingI d1,
    SingI d2
  ) =>
  Pace d1 a ->
  Pace d2 a
convertPace = MkPace . (.% fromBase) . (.* toBase) . (.unPace)
  where
    toBase = singFactor @_ @d1
    fromBase = singFactor @_ @d2

-- | Pace, existentially quantifying the distance unit.
type SomePace :: Type -> Type
data SomePace a where
  MkSomePace :: Sing d -> Pace d a -> SomePace a

instance
  ( Eq a,
    FromInteger a,
    MetricSpace a,
    MGroup a
  ) =>
  Eq (SomePace a)
  where
  MkSomePace sx x == MkSomePace sy y =
    withSingI sx toKilometers x == withSingI sy toKilometers y

instance
  ( FromInteger a,
    MetricSpace a,
    MGroup a,
    Ord a
  ) =>
  Ord (SomePace a)
  where
  MkSomePace sx x <= MkSomePace sy y =
    withSingI sx toKilometers x <= withSingI sy toKilometers y

instance HasField "unSomePace" (SomePace a) (Duration Second a) where
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
    FromInteger a,
    MSemigroup a,
    ToRational a
  ) =>
  Display (SomePace a)
  where
  displayBuilder (MkSomePace s x) = withSingI s displayBuilder x

instance (MSemigroup a) => MSemiSpace (SomePace a) a where
  MkSomePace s x .* k = MkSomePace s (x .* k)

instance (MGroup a) => MSpace (SomePace a) a where
  MkSomePace s x .% k = MkSomePace s (x .% k)

instance (FromRational a) => FromRational (SomePace a) where
  fromQ = MkSomePace SKilometer . fromQ

instance (FromInteger a, MGroup a, ToRational a) => ToRational (SomePace a) where
  toQ = toQ . someToKilometers

instance (FromInteger a) => FromInteger (SomePace a) where
  fromZ = MkSomePace SKilometer . fromℤ

instance (FromInteger a, MGroup a, ToInteger a) => ToInteger (SomePace a) where
  toZ = toZ . someToKilometers

instance (FromReal a) => FromReal (SomePace a) where
  fromR = MkSomePace SKilometer . fromR

instance (FromInteger a, MGroup a, ToReal a) => ToReal (SomePace a) where
  toR = toR . someToKilometers

-- NOTE: [SomePace Parsing]
--
-- Adds units to pace e.g. "1h 4'30\" /km".
--
-- See NOTE: [Pace Parsing]

instance
  {-# OVERLAPPABLE #-}
  ( Fractional a,
    FromInteger a,
    MGroup a
  ) =>
  Parser (SomePace a)
  where
  parser = do
    MkDuration x <- parser @(Duration Second a)
    MPC.space
    MPC.char '/'

    -- We do not use DistanceUnit's built-in parsing because we want the
    -- long units here (kilometer and mile) to be __singular__, not plural.
    eDUnit <-
      MP.choice
        [ MPC.string "meters" $> Left "meters",
          MPC.string "km" $> Right Kilometer,
          MPC.string "kilometers" $> Left "kilometers",
          MPC.string "kilometer" $> Right Kilometer,
          MPC.string "miles" $> Left "miles",
          MPC.string "mile" $> Right Mile,
          MPC.string "mi" $> Right Mile,
          MPC.char 'm' $> Right Meter
        ]

    case eDUnit of
      Left "meters" -> fail "Meters are disallowed in Pace; use km or mi."
      Left d -> fail $ "Pace unit " ++ d ++ " should be singular"
      Right Meter -> fail "Meters are disallowed in Pace; use km or mi."
      Right Kilometer -> pure $ MkSomePace SKilometer $ MkPace (MkDuration x)
      Right Mile -> pure $ MkSomePace SMile $ MkPace (MkDuration x)

instance
  {-# OVERLAPPING #-}
  ( Fractional a,
    FromInteger a,
    MGroup a,
    Ord a,
    Show a
  ) =>
  Parser (SomePace (Positive a))
  where
  parser = do
    -- reuse non-positive parser
    MkSomePace s (MkPace (MkDuration x)) <- parser @(SomePace a)
    y <- mkPositiveFailZ x
    pure $ MkSomePace s (MkPace (MkDuration y))

-- | Exposes the underlying duration.
unSomePace :: SomePace a -> Duration Second a
unSomePace (MkSomePace _ (MkPace x)) = x

-- | Hides the distance.
mkSomePace :: forall d a. (SingI d) => Pace d a -> SomePace a
mkSomePace = MkSomePace (sing @d)

-- | Converts some distance to meters.
someToKilometers ::
  (FromInteger a, MGroup a) =>
  SomePace a ->
  Pace Kilometer a
someToKilometers (MkSomePace s x) = withSingI s toKilometers x
