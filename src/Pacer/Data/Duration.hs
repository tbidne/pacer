{-# LANGUAGE UndecidableInstances #-}

module Pacer.Data.Duration
  ( -- * Duration
    Duration (..),

    -- ** Elimination
    toHrMinSec,

    -- ** Functions
    toTimeString,
    liftDuration,
    liftDuration2,
  )
where

import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Prelude

-------------------------------------------------------------------------------
--                                  Duration                                 --
-------------------------------------------------------------------------------

-- | Represents time as seconds.
type Duration :: Type -> Type
newtype Duration a = MkDuration {unDuration :: Positive a}
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
--                                Base Classes                               --
-------------------------------------------------------------------------------

instance (MetricSpace a) => Eq (Duration a) where
  MkDuration x == MkDuration y = ɛEq 1 x y

instance (MetricSpace a, Ord a) => Ord (Duration a) where
  dx@(MkDuration x) <= dy@(MkDuration y) = dx == dy || x < y

instance
  ( Fromℤ a,
    MSemigroup a,
    Toℚ a
  ) =>
  Display (Duration a)
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

-------------------------------------------------------------------------------
--                                   Algebra                                 --
-------------------------------------------------------------------------------

instance (ASemigroup a) => ASemigroup (Duration a) where
  (.+.) = liftDuration2 (.+.)

instance (MSemigroup a) => MSemiSpace (Duration a) (Positive a) where
  MkDuration x .* k = MkDuration (x .*. k)

instance (MGroup a) => MSpace (Duration a) (Positive a) where
  MkDuration x .% k = MkDuration (x .%. k)

-------------------------------------------------------------------------------
--                                   Parsing                                 --
-------------------------------------------------------------------------------

-- NOTE: [Duration Parsing]
--
-- Parses a time string and converts to the requested units. We have separate
-- (overlapping) instances for 'a' and 'Positive a', since the latter needs
-- to perform a positive check.
--
-- See NOTE: [SomeDuration Parsing]

instance
  (AMonoid a, Fromℤ a, Ord a, Show a) =>
  Parser (Duration a)
  where
  parser = do
    -- read text like "1d2h3m4s", parse w/ relative time into seconds.
    MkDuration <$> P.parsePosTimeString

-------------------------------------------------------------------------------
--                                    Misc                                   --
-------------------------------------------------------------------------------

liftDuration ::
  (Positive a -> Positive a) ->
  Duration a ->
  Duration a
liftDuration f (MkDuration x) = MkDuration (f x)

liftDuration2 ::
  (Positive a -> Positive a -> Positive a) ->
  Duration a ->
  Duration a ->
  Duration a
liftDuration2 f (MkDuration x) (MkDuration y) = MkDuration (f x y)

toHrMinSec :: (Toℚ a) => Duration a -> Tuple3 Word32 Word32 Word32
toHrMinSec (MkDuration d) = normalizeTime (h_init, m_init, s_init)
  where
    -- total_seconds: duration in seconds, via a -> Rational -> Double
    total_seconds :: Double
    total_seconds = toℝ d

    -- In general _init means the calculated value __before__ we handle
    -- normalizing (e.g. 60 seconds -> 1 minute).

    -- h_init := non-normalized hours
    -- frac_h := the decimal remainder, fraction of an hour
    (h_init, frac_h) = properFraction @Double $ total_seconds / k_hr

    -- rem_seconds := total number of seconds from the frac_h remainder,
    -- after multiplying by the hour/second factor.
    rem_seconds :: Double
    rem_seconds = fromℤ (round (frac_h * k_hr))

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
    k_min = 60

    k_hr :: Double
    k_hr = 3_600

toTimeString :: (Toℚ a) => Duration a -> Text
toTimeString d =
  mconcat
    [ showu h "h",
      showu m "m",
      showu s "s"
    ]
  where
    (h, m, s) = toHrMinSec d
    showu 0 _ = ""
    showu x u = showt x <> u

-- Normalizes values. This is a bit clunky. Alternatively, we could
-- simply trade our 'round' usages for 'floor', and that would probably avoid
-- having non-normalized values. But round is more precise, hence we
-- prefer it.
normalizeTime :: Tuple3 Word32 Word32 Word32 -> Tuple3 Word32 Word32 Word32
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
