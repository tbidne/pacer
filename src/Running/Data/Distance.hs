{-# LANGUAGE AllowAmbiguousTypes #-}

module Running.Data.Distance
  ( -- * Distance
    Distance (..),
    toMeters,

    -- * Units
    DistanceUnit (..),
    SI (..),
  )
where

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
import Running.Data.Distance.Units
  ( DistanceUnit (Meter, Mile),
    SingDistanceUnit (singDistanceUnit),
    fromSingleton,
    fromSingleton',
    toFactor,
  )
import Running.Data.SI
  ( SI
      ( Base,
        Centi,
        Deca,
        Deci,
        Hecto,
        Kilo,
        Mega,
        Micro,
        Milli
      ),
  )

type Distance :: DistanceUnit -> Type
newtype Distance d = MkDistance {unDistance :: Double}
  deriving stock (Eq)

instance (SingDistanceUnit d) => Show (Distance d) where
  showsPrec i (MkDistance x) =
    showParen
      (i >= 11)
      ( showString "MkDistance "
          . showsPrec 11 x
          . showSpace
          . showsPrec 11 d
      )
    where
      d = fromSingleton' @d

instance (SingDistanceUnit d) => Display (Distance d) where
  displayBuilder (MkDistance x) =
    mconcat
      [ displayBuilder x,
        " ",
        displayBuilder (fromSingleton' @d)
      ]

instance ASemigroup (Distance d) where
  (.+.) = liftBinOp (+)

instance AMonoid (Distance d) where
  zero = MkDistance 0

instance AGroup (Distance d) where
  (.-.) = liftBinOp (-)

instance MSemiSpace (Distance d) Double where
  MkDistance x .* k = MkDistance (x * k)

instance MSpace (Distance d) Double where
  MkDistance x .% k = MkDistance (x / k)

instance Semimodule (Distance d) Double

instance SemivectorSpace (Distance d) Double

instance Module (Distance d) Double

instance VectorSpace (Distance d) Double

instance FromRational (Distance d) where
  afromRational = MkDistance . fromRational

instance FromInteger (Distance d) where
  afromInteger = MkDistance . fromInteger

liftBinOp ::
  (Double -> Double -> Double) ->
  Distance d ->
  Distance d ->
  Distance d
liftBinOp f (MkDistance x) (MkDistance y) = MkDistance (f x y)

toMeters :: forall d. (SingDistanceUnit d) => Distance d -> Distance (Meter Base)
toMeters = MkDistance . (* factor) . (.unDistance)
  where
    factor :: Double
    factor = toFactor $ fromSingleton $ singDistanceUnit @d
