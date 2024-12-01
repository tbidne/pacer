{-# LANGUAGE AllowAmbiguousTypes #-}

module Running.Data.Duration
  ( -- * Duration
    Duration (..),

    -- ** Creation
    fromMinSec,
    fromMinSecRaw,

    -- ** Elimination
    toSeconds,
    toMinSec,
    toMinSecRaw,

    -- * Units
    TimeUnit (..),
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Kind (Type)
import Data.Text.Display (Display (displayBuilder))
import GHC.Show (showSpace)
import Numeric.Algebra.Additive
  ( AGroup ((.-.)),
    AMonoid (zero),
    ASemigroup ((.+.)),
  )
import Numeric.Algebra.Space
  ( MSemiSpace ((.*)),
    MSpace ((.%)),
    Module,
    Semimodule,
    SemivectorSpace,
    VectorSpace,
  )
import Numeric.Literal.Integer (FromInteger (afromInteger))
import Numeric.Literal.Rational (FromRational (afromRational))
import Running.Data.Duration.Units
  ( STimeUnit (SHour, SMinute, SSecond),
    SingTimeUnit (singTimeUnit),
    TimeUnit (Hour, Minute, Second),
    fromSingleton',
  )
import Running.Data.Duration.Units qualified as Units

type Duration :: TimeUnit -> Type
newtype Duration t = MkDuration {unDuration :: Double}
  deriving stock (Eq)

instance (SingTimeUnit t) => Show (Duration t) where
  showsPrec i (MkDuration x) =
    showParen
      (i >= 11)
      ( showString "MkDuration "
          . showsPrec 11 x
          . showSpace
          . showsPrec 11 t
      )
    where
      t = fromSingleton' @t

instance (SingTimeUnit t) => Display (Duration t) where
  displayBuilder (MkDuration x) =
    mconcat
      [ displayBuilder x,
        " ",
        displayBuilder (fromSingleton' @t)
      ]

instance ASemigroup (Duration t) where
  (.+.) = liftBinOp (+)

instance AMonoid (Duration t) where
  zero = MkDuration 0

instance AGroup (Duration t) where
  (.-.) = liftBinOp (-)

instance MSemiSpace (Duration t) Double where
  MkDuration x .* k = MkDuration (x * k)

instance MSpace (Duration t) Double where
  MkDuration x .% k = MkDuration (x / k)

instance Semimodule (Duration t) Double

instance SemivectorSpace (Duration t) Double

instance Module (Duration t) Double

instance VectorSpace (Duration t) Double

instance FromRational (Duration t) where
  afromRational = MkDuration . fromRational

instance FromInteger (Duration t) where
  afromInteger = MkDuration . fromInteger

toSeconds :: forall t. (SingTimeUnit t) => Duration t -> Duration Second
toSeconds = MkDuration . (* factor) . (.unDuration)
  where
    factor :: Double
    factor = Units.baseFactor $ fromSingleton' @t

toMinSec ::
  forall t.
  (SingTimeUnit t) =>
  Duration t ->
  (Duration Minute, Duration Second)
toMinSec = bimap afromInteger afromInteger . toMinSecRaw

toMinSecRaw ::
  forall t a.
  (Integral a, SingTimeUnit t) =>
  Duration t ->
  (a, a)
toMinSecRaw (MkDuration 0) = (0, 0)
toMinSecRaw d = (m, s)
  where
    MkDuration totalSeconds = toSeconds d
    (m, s) = quotRem (floor totalSeconds) 60

fromMinSec ::
  forall t.
  (SingTimeUnit t) =>
  (Duration Minute, Duration Second) ->
  Duration t
fromMinSec (MkDuration m, MkDuration s) = fromMinSecRaw (m, s)

fromMinSecRaw ::
  forall t.
  (SingTimeUnit t) =>
  (Double, Double) ->
  Duration t
fromMinSecRaw (m, s) = case singTimeUnit @t of
  SSecond -> MkDuration (m * 60 + s)
  SMinute -> MkDuration (m + s / 60)
  SHour -> MkDuration (m / 60 + s / 3_600)

liftBinOp ::
  (Double -> Double -> Double) ->
  Duration t ->
  Duration t ->
  Duration t
liftBinOp f (MkDuration x) (MkDuration y) = MkDuration (f x y)
