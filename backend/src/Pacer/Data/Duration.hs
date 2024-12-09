{-# LANGUAGE UndecidableInstances #-}

module Pacer.Data.Duration
  ( -- * Duration
    Duration (..),

    -- ** Elimination
    toHrMinSec,

    -- ** Functions
    toSeconds,
    convertDuration,

    -- * Some Duration
    SomeDuration (..),
    someToSeconds,

    -- * Units
    TimeUnit (..),
  )
where

import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Class.Units (Units (baseFactor), singFactor)
import Pacer.Data.Duration.Units
  ( STimeUnit (SSecond),
    TimeUnit (Hour, Minute, Second),
  )
import Pacer.Prelude

-- | Units for time.
type Duration :: TimeUnit -> Type -> Type
newtype Duration t a = MkDuration {unDuration :: a}

instance (MetricSpace a, SingI t) => Eq (Duration t a) where
  MkDuration x == MkDuration y = ɛEq ɛ x y
    where
      ɛ = 1 / singFactor @_ @t

instance (MetricSpace a, Ord a, SingI t) => Ord (Duration t a) where
  dx@(MkDuration x) <= dy@(MkDuration y) = dx == dy || x < y

instance (Show a, SingI t) => Show (Duration t a) where
  showsPrec i (MkDuration x) =
    showParen
      (i >= 11)
      ( showString "MkDuration "
          . showsPrec 11 x
          . showSpace
          . showsPrec 11 t
      )
    where
      t = fromSingI @_ @t

instance
  ( FromInteger a,
    MSemigroup a,
    SingI t,
    ToRational a
  ) =>
  Display (Duration t a)
  where
  displayBuilder x =
    mconcat
      [ hTxt,
        displayBuilder m,
        "'",
        pad2 s <> displayBuilder s,
        "\""
      ]
    where
      (h, m, s) = toHrMinSec x

      hTxt =
        if h == 0
          then ""
          else displayBuilder h <> "h "

      pad2 i
        | i < 9 = "0"
        | otherwise = ""

instance (ASemigroup a) => ASemigroup (Duration t a) where
  (.+.) = liftDuration2 (.+.)

instance (AMonoid a) => AMonoid (Duration t a) where
  zero = MkDuration zero

instance (AGroup a) => AGroup (Duration t a) where
  (.-.) = liftDuration2 (.-.)

instance (MSemigroup a) => MSemiSpace (Duration t a) a where
  MkDuration x .* k = MkDuration (x .*. k)

instance (MGroup a) => MSpace (Duration t a) a where
  MkDuration x .% k = MkDuration (x .%. k)

instance (Semiring a) => Semimodule (Duration t a) a

instance (Semifield a) => SemivectorSpace (Duration t a) a

instance (Ring a) => Module (Duration t a) a

instance (Field a) => VectorSpace (Duration t a) a

instance (FromRational a) => FromRational (Duration t a) where
  fromQ = MkDuration . fromQ

instance (ToRational a) => ToRational (Duration t a) where
  toQ (MkDuration x) = toQ x

instance (FromInteger a) => FromInteger (Duration t a) where
  fromZ = MkDuration . fromℤ

instance (ToInteger a) => ToInteger (Duration t a) where
  toZ (MkDuration x) = toZ x

instance (FromReal a) => FromReal (Duration t a) where
  fromR = MkDuration . fromR

instance (ToReal a) => ToReal (Duration t a) where
  toR (MkDuration x) = toR x

-- NOTE: [Duration Parsing]
--
-- Parses a time string and converts to the requested units. We have separate
-- (overlapping) instances for 'a' and 'Positive a', since the latter needs
-- to perform a positive check.
--
-- See NOTE: [SomeDuration Parsing]

instance
  {-# OVERLAPPABLE #-}
  ( Fractional a,
    FromInteger a,
    MGroup a,
    SingI t
  ) =>
  Parser (Duration t a)
  where
  parser = do
    -- read text like "1d2h3m4s", parse w/ relative time into seconds.
    seconds <- P.parseTimeString
    -- convert to requested units
    pure $ MkDuration seconds .% singFactor @_ @t

instance
  {-# OVERLAPPING #-}
  ( Fractional a,
    FromInteger a,
    MGroup a,
    Ord a,
    Show a,
    SingI t
  ) =>
  Parser (Duration t (Positive a))
  where
  parser = do
    -- reuse non-positive parser
    MkDuration x <- parser @(Duration t a)
    y <- mkPositiveFailZ x
    pure $ MkDuration y

liftDuration2 ::
  (a -> a -> a) ->
  Duration t a ->
  Duration t a ->
  Duration t a
liftDuration2 f (MkDuration x) (MkDuration y) = MkDuration (f x y)

-- | Convert to seconds.
toSeconds ::
  forall t a.
  ( FromInteger a,
    MSemigroup a,
    SingI t
  ) =>
  Duration t a ->
  Duration Second a
toSeconds = MkDuration . (.*. toBase) . (.unDuration)
  where
    toBase = singFactor @_ @t

-- | Convert from on duration to another.
convertDuration ::
  forall t2 t1 a.
  ( FromInteger a,
    MGroup a,
    SingI t1,
    SingI t2
  ) =>
  Duration t1 a ->
  Duration t2 a
convertDuration = MkDuration . (.%. fromBase) . (.*. toBase) . (.unDuration)
  where
    toBase = singFactor @_ @t1
    fromBase = singFactor @_ @t2

toHrMinSec ::
  forall t a.
  ( FromInteger a,
    MSemigroup a,
    SingI t,
    ToRational a
  ) =>
  Duration t a ->
  (Word32, Word32, Word32)
toHrMinSec d = normalizeTime (h_init, m_init, s_init)
  where
    -- total_seconds: duration in seconds, via a -> Rational -> Double
    total_seconds :: Double
    total_seconds = realToFrac $ toQ $ toSeconds d

    -- In general _init means the calculated value __before__ we handle
    -- normalizing (e.g. 60 seconds -> 1 minute).

    -- h_init := non-normalized hours
    -- frac_h := the decimal remainder, fraction of an hour
    (h_init :: Word32, frac_h :: Double) = properFraction $ total_seconds / k_hr

    -- rem_seconds := total number of seconds from the frac_h remainder,
    -- after multiplying by the hour/second factor.
    rem_seconds :: Double
    rem_seconds = fromIntegral @Integer (round (frac_h * k_hr))

    -- m is the integral number of minutes
    -- frac_m in [0, 1) is the fraction of a minute leftover i.e. multiply
    -- by k (60) to get num seconds.

    -- m_init: non-normalized minutes
    -- frac_m: the decimal remainder, fraction of a minute
    (m_init, frac_m) = properFraction $ rem_seconds / k_min

    -- s_init: non-normalized seconds
    s_init :: Word32
    s_init = round $ frac_m * k_min

    k_min :: Double
    k_min = baseFactor Minute

    k_hr :: Double
    k_hr = baseFactor Hour

-- Normalizes values. This is a bit clunky. Alternatively, we could
-- simply trade our 'round' usages for 'floor', and that would probably avoid
-- having non-normalized values. But round is more precise, hence we
-- prefer it.
normalizeTime :: (Word32, Word32, Word32) -> (Word32, Word32, Word32)
normalizeTime (h, m, s) = (h_final, m_final, s_final)
  where
    (m_temp, s_final) =
      if s == 60
        then (m + 1, 0)
        else (m, s)

    (h_final, m_final) =
      if m_temp == 60
        then (h + 1, 0)
        else (h, m_temp)

-- | Duration, existentially quantifying the units.
type SomeDuration :: Type -> Type
data SomeDuration a where
  MkSomeDuration :: Sing t -> Duration t a -> SomeDuration a

instance
  ( FromInteger a,
    MetricSpace a,
    MSemigroup a
  ) =>
  Eq (SomeDuration a)
  where
  t1 == t2 = someToSeconds t1 == someToSeconds t2

instance
  ( FromInteger a,
    MetricSpace a,
    MSemigroup a,
    Ord a
  ) =>
  Ord (SomeDuration a)
  where
  t1 <= t2 = someToSeconds t1 <= someToSeconds t2

instance HasField "unSomeDuration" (SomeDuration a) a where
  getField (MkSomeDuration _ (MkDuration t)) = t

instance (Show a) => Show (SomeDuration a) where
  showsPrec i (MkSomeDuration u x) =
    showParen
      (i >= 11)
      ( showString "MkSomeDuration "
          . showsPrec 11 u
          . showSpace
          . withSingI u showsPrec 11 x
      )

instance
  ( Display a,
    FromInteger a,
    MetricSpace a,
    MSemigroup a,
    ToRational a
  ) =>
  Display (SomeDuration a)
  where
  displayBuilder (MkSomeDuration u x) = withSingI u displayBuilder x

instance (ASemigroup a, FromInteger a, MSemigroup a) => ASemigroup (SomeDuration a) where
  (.+.) = liftSomeDuration2 (.+.)

instance (AMonoid a, FromInteger a, MSemigroup a) => AMonoid (SomeDuration a) where
  zero = MkSomeDuration SSecond zero

instance (FromInteger a, Ring a) => AGroup (SomeDuration a) where
  (.-.) = liftSomeDuration2 (.-.)

instance (MSemigroup a) => MSemiSpace (SomeDuration a) a where
  MkSomeDuration u x .* k = MkSomeDuration u (x .* k)

instance (MGroup a) => MSpace (SomeDuration a) a where
  MkSomeDuration u x .% k = MkSomeDuration u (x .% k)

instance (FromInteger a, Semiring a) => Semimodule (SomeDuration a) a

instance (FromInteger a, Semifield a) => SemivectorSpace (SomeDuration a) a

instance (FromInteger a, Ring a) => Module (SomeDuration a) a

instance (FromInteger a, Field a) => VectorSpace (SomeDuration a) a

instance (FromRational a) => FromRational (SomeDuration a) where
  fromQ = MkSomeDuration SSecond . fromQ

instance (FromInteger a, MSemigroup a, ToRational a) => ToRational (SomeDuration a) where
  toQ = toQ . someToSeconds

instance (FromReal a) => FromReal (SomeDuration a) where
  fromR = MkSomeDuration SSecond . fromR

instance (FromInteger a, MSemigroup a, ToReal a) => ToReal (SomeDuration a) where
  toR = toℝ . someToSeconds

instance (FromInteger a) => FromInteger (SomeDuration a) where
  fromZ = MkSomeDuration SSecond . fromZ

instance (FromInteger a, MSemigroup a, ToInteger a) => ToInteger (SomeDuration a) where
  toZ = toZ . someToSeconds

-- NOTE: [SomeDuration Parsing]
--
-- Reuses the Duration parser, and converts to Second. Interestingly, this is
-- one of the few cases where the SomeX parser is simpler than the X parser.
-- Why?
--
-- Most of the time, the SomeX parser adds some notion of units e.g.
-- Distance and SomeDistance are '"2.45" :: Distance t a' and
-- '"2.45 meters" :: SomeDistance a', respectively. Letting the user choose
-- the units via the text means the type needs to be existentially quantified,
-- hence the SomeX type.
--
-- In contrast, the time-strings parsed by Duration and SomeDuration allow
-- the user to include multiple units e.g. "1h2d3m4s". This is for
-- convenience, as supplying "2h4m15s" is easier than "2.071 hours" or
-- "7455 seconds". Notice this means there is no notion of a "requested unit",
-- like with SomeDistance.
--
-- Hence the SomeDuration has the same parsing as Duration. We merely choose
-- the final unit ourselves (Seconds), since it is existentially quantified.
--
-- See NOTE: [Duration Parsing]

instance
  {-# OVERLAPPABLE #-}
  ( Fractional a,
    FromInteger a,
    MGroup a
  ) =>
  Parser (SomeDuration a)
  where
  parser = MkSomeDuration SSecond <$> parser

instance
  {-# OVERLAPPING #-}
  ( Fractional a,
    FromInteger a,
    MGroup a,
    Ord a,
    Show a
  ) =>
  Parser (SomeDuration (Positive a))
  where
  parser = do
    -- reuse non-positive parser
    MkSomeDuration s (MkDuration x) <- parser @(SomeDuration a)
    y <- mkPositiveFailZ x
    pure $ MkSomeDuration s (MkDuration y)

liftSomeDuration2 ::
  (FromInteger a, MSemigroup a) =>
  (forall t. Duration t a -> Duration t a -> Duration t a) ->
  SomeDuration a ->
  SomeDuration a ->
  SomeDuration a
liftSomeDuration2 f x y =
  MkSomeDuration SSecond (someToSeconds x `f` someToSeconds y)

-- | Converts some distance to meters.
someToSeconds ::
  (FromInteger a, MSemigroup a) =>
  SomeDuration a ->
  Duration Second a
someToSeconds (MkSomeDuration u t) = withSingI u toSeconds t
