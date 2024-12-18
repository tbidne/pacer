-- | Exports functions for deriving quantities.
module Pacer.Derive
  ( -- * Distance
    deriveDistance,
    deriveSomeDistance,

    -- * Duration
    deriveDuration,
    deriveSomeDuration,

    -- * Pace
    derivePace,
    deriveSomePace,
  )
where

import Pacer.Data.Distance
  ( Distance (MkDistance, unDistance),
    HasDistance (hideDistance),
    SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance qualified as Dist
import Pacer.Data.Distance.Units (SDistanceUnit (SKilometer, SMeter, SMile))
import Pacer.Data.Duration (Duration (MkDuration), Seconds)
import Pacer.Data.Duration qualified as Duration
import Pacer.Data.Pace
  ( Pace (MkPace),
    PaceDistF,
    SomePace (MkSomePace),
    mkPace,
  )
import Pacer.Prelude

-- | Given a distance and a duration, derives the pace.
derivePace ::
  forall t d a.
  ( FromInteger a,
    MGroup a,
    PaceDistF d,
    SingI t
  ) =>
  -- | Distance.
  Distance d (Positive a) ->
  -- | Duration.
  Duration t a ->
  -- | Pace.
  Pace d a
derivePace distance duration =
  mkPace $ duration .% distance.unDistance.unPositive

-- | Given an existentially-quantified distance and a duration, derives
-- the pace.
deriveSomePace ::
  forall t a.
  (AMonoid a, FromInteger a, MGroup a, Ord a, Show a, SingI t) =>
  -- | Existentially-quantified distance.
  SomeDistance (Positive a) ->
  -- | Duration.
  Duration t a ->
  -- | Pace.
  SomePace a
deriveSomePace (MkSomeDistance sdist distance) duration =
  case sdist of
    SMeter ->
      let distanceKm = Dist.convertToKilometers_ distance
       in hideDistance $ derivePace distanceKm duration
    SKilometer ->
      hideDistance $ derivePace distance duration
    SMile ->
      hideDistance $ derivePace distance duration

-- | Given a distance and pace, derives the duration.
deriveDuration ::
  ( MSemigroup a
  ) =>
  -- | Distance.
  Distance d a ->
  -- | Pace.
  Pace d a ->
  -- | Duration.
  Seconds a
deriveDuration distance pace = pace.unPace .* distance.unDistance

-- | Given existentially-quantified distance and pace, derives the duration.
-- Different distance units are converted.
deriveSomeDuration ::
  ( FromInteger a,
    MGroup a
  ) =>
  -- | Existentially-quantified distance.
  SomeDistance a ->
  -- | Existentially-quantified pace.
  SomePace a ->
  -- | Duration.
  Seconds a
deriveSomeDuration
  (MkSomeDistance sdistance distance)
  (MkSomePace space pace) =
    deriveDuration distance' pace
    where
      distance' =
        withSingI sdistance
          $ withSingI space
          $ Dist.convertDistance_ distance

-- | Given a duration and pace, derives the distance.
deriveDistance ::
  forall t d a.
  ( FromInteger a,
    MGroup a,
    SingI t
  ) =>
  -- | Distance.
  Duration t a ->
  -- | Pace.
  Pace d (Positive a) ->
  -- | Distance.
  Distance d a
deriveDistance duration (MkPace (MkDuration paceDuration)) =
  MkDistance $ scaleDuration (Duration.toSeconds duration)
  where
    -- monomorphic on Second so that we have to use toSeconds
    scaleDuration :: Seconds a -> a
    scaleDuration = (.unDuration) . (.% paceDuration.unPositive)

-- | Given a duration and existentially-quantified pace, derives the
-- distance.
deriveSomeDistance ::
  forall t a.
  (FromInteger a, MGroup a, SingI t) =>
  -- | Duration.
  Duration t a ->
  -- | Existentially-quantified Pace.
  SomePace (Positive a) ->
  -- | Existentially-quantified Distance.
  SomeDistance a
deriveSomeDistance duration (MkSomePace space pace) =
  MkSomeDistance space $ deriveDistance duration pace
