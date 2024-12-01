module Running.Data.Pace
  ( Pace (..),

    -- ** Creation
    fromMinSec,
    fromMinSecRaw,

    -- ** Elimination
    toMinSec,
    toMinSecRaw,
  )
where

import Data.Kind (Type)
import Data.Text.Display (Display (displayBuilder))
import GHC.Show (showSpace)
import Numeric.Algebra.Space (MSemiSpace ((.*)), MSpace ((.%)))
import Numeric.Literal.Integer (FromInteger (afromInteger))
import Numeric.Literal.Rational (FromRational (afromRational))
import Running.Data.Distance (DistanceUnit)
import Running.Data.Distance.Units (SingDistanceUnit)
import Running.Data.Distance.Units qualified as DUnit
import Running.Data.Duration (Duration (MkDuration), TimeUnit (Minute, Second))
import Running.Data.Duration qualified as D
import Running.Data.Duration.Units (SingTimeUnit)

-- 1. Should we let underlying value be a? Probably
-- 2. Should pace hide time variable? Also probably

type Pace :: TimeUnit -> DistanceUnit -> Type
newtype Pace t d = MkPace {unPace :: Duration t}
  deriving stock (Eq)

instance (SingDistanceUnit d, SingTimeUnit t) => Show (Pace t d) where
  showsPrec i (MkPace p) =
    showParen
      (i >= 11)
      ( showString "MkPace "
          . showsPrec 11 p
          . showSpace
          . showsPrec 11 d
      )
    where
      d = DUnit.fromSingleton' @d

instance (SingDistanceUnit d, SingTimeUnit t) => Display (Pace t d) where
  displayBuilder (MkPace x) =
    mconcat
      [ displayBuilder (toInt m),
        "'",
        displayBuilder (toInt s),
        "\"",
        " /",
        displayBuilder d
      ]
    where
      (MkDuration m, MkDuration s) = D.toMinSec x
      d = DUnit.fromSingleton' @d

      toInt = floor @Double @Int

instance MSemiSpace (Pace t d) Double where
  MkPace x .* k = MkPace (x .* k)

instance MSpace (Pace t d) Double where
  MkPace x .% k = MkPace (x .% k)

instance FromRational (Pace t d) where
  afromRational = MkPace . afromRational

instance FromInteger (Pace t d) where
  afromInteger = MkPace . afromInteger

fromMinSec ::
  forall t d.
  (SingTimeUnit t) =>
  (Duration Minute, Duration Second) ->
  Pace t d
fromMinSec = MkPace . D.fromMinSec

fromMinSecRaw ::
  forall t d.
  (SingTimeUnit t) =>
  (Double, Double) ->
  Pace t d
fromMinSecRaw = MkPace . D.fromMinSecRaw

toMinSec ::
  forall t d.
  (SingTimeUnit t) =>
  Pace t d ->
  (Duration Minute, Duration Second)
toMinSec = D.toMinSec . (.unPace)

toMinSecRaw ::
  forall t d a.
  (Integral a, SingTimeUnit t) =>
  Pace t d ->
  (a, a)
toMinSecRaw = D.toMinSecRaw . (.unPace)
