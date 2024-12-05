{-# LANGUAGE UndecidableInstances #-}

module Running.Data.Pace
  ( Pace,
    PaceDistF,

    -- ** Creation
    mkPace,

    -- ** Elimination
    unPace,
  )
where

import GHC.TypeError (Unsatisfiable)
import GHC.TypeError qualified as TE
import Running.Class.Singleton (SingI, fromSingI)
import Running.Data.Distance (DistanceUnit (Kilometer, Meter, Mile))
import Running.Data.Duration (Duration, TimeUnit (Second))
import Running.Data.Duration qualified as D
import Running.Prelude

-- | Represents a duration per distance e.g. 4'30" per kilometer. We only
-- allow the distance to be kilometers or miles. Meters are disallowed as
-- they chance for mistakes are high (i.e. almost all paces per meter are
-- zero since they are so low).
type Pace :: DistanceUnit -> Type -> Type
data Pace d a where
  MkPace :: (PaceDistF d) => Duration Second a -> Pace d a

-- | Creates a pace from a duration.
mkPace ::
  ( PaceDistF d,
    FromInteger a,
    MSemigroup a,
    SingI t
  ) =>
  Duration t a ->
  Pace d a
mkPace = MkPace . D.toSeconds

-- | Eliminates a pace to the underlying duration.
unPace :: Pace d a -> Duration Second a
unPace (MkPace d) = d

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
  fromQ = MkPace . (fromQ @(Duration Second a))

instance (FromInteger a, PaceDistF d) => FromInteger (Pace d a) where
  fromZ = MkPace . (fromZ @(Duration Second a))
