module Running
  ( -- * Conversions

    -- ** Distance
    calculateDistance,
    calculateSomeDistance,

    -- ** Duration
    calculateDuration,
    calculateSomeDuration,

    -- ** Pace
    calculatePace,
    calculateSomePace,
  )
where

import Running.Data.Distance
  ( Distance (MkDistance, unDistance),
    SomeDistance (MkSomeDistance),
  )
import Running.Data.Distance qualified as Dist
import Running.Data.Distance.Units (SDistanceUnit (SKilometer, SMeter, SMile))
import Running.Data.Duration (Duration (MkDuration), TimeUnit (Second))
import Running.Data.Duration qualified as Duration
import Running.Data.Pace (Pace (MkPace), PaceDistF, SomePace (MkSomePace), mkPace)
import Running.Prelude

-- | Given a distance and a duration, calculates the pace.
calculatePace ::
  forall t d.
  ( PaceDistF d,
    SingI t
  ) =>
  -- | Distance.
  Distance d PDouble ->
  -- | Duration.
  Duration t PDouble ->
  -- | Pace.
  Pace d PDouble
calculatePace distance duration = mkPace $ duration .% distance.unDistance

-- | Given an existentially-quantified distance and a duration, calculates
-- the pace.
calculateSomePace ::
  (SingI t) =>
  -- | Existentially-quantified distance.
  SomeDistance PDouble ->
  -- | Duration.
  Duration t PDouble ->
  -- | Pace.
  SomePace PDouble
calculateSomePace (MkSomeDistance sdist distance) duration =
  case sdist of
    SMeter ->
      let distance' = Dist.convertDistance distance
       in MkSomePace SKilometer $ calculatePace distance' duration
    SKilometer ->
      MkSomePace SKilometer $ calculatePace distance duration
    SMile ->
      MkSomePace SMile $ calculatePace distance duration

-- | Given a distance and pace, calculates the duration.
calculateDuration ::
  -- | Distance.
  Distance d PDouble ->
  -- | Pace.
  Pace d PDouble ->
  -- | Duration.
  Duration Second PDouble
calculateDuration distance pace = pace.unPace .* distance.unDistance

-- | Given existentially-quantified distance and pace, calculates the duration.
-- Different distance units are converted.
calculateSomeDuration ::
  -- | Existentially-quantified distance.
  SomeDistance PDouble ->
  -- | Existentially-quantified pace.
  SomePace PDouble ->
  -- | Duration.
  Duration Second PDouble
calculateSomeDuration
  (MkSomeDistance sdistance distance)
  (MkSomePace space pace) =
    calculateDuration distance' pace
    where
      distance' =
        withSingI sdistance
          $ withSingI space
          $ Dist.convertDistance distance

-- | Given a duration and pace, calculates the distance.
calculateDistance ::
  (SingI t) =>
  -- | Distance.
  Duration t PDouble ->
  -- | Pace.
  Pace d PDouble ->
  -- | Distance.
  Distance d PDouble
calculateDistance duration (MkPace (MkDuration paceDuration)) =
  MkDistance $ scaleDuration (Duration.toSeconds duration)
  where
    -- monomorphic on Second so that we have to use toSeconds
    scaleDuration :: Duration Second PDouble -> PDouble
    scaleDuration = (.unDuration) . (.% paceDuration)

-- | Given a duration and existentially-quantified pace, calculates the
-- distance.
calculateSomeDistance ::
  (SingI t) =>
  -- | Duration.
  Duration t PDouble ->
  -- | Existentially-quantified Pace.
  SomePace PDouble ->
  -- | Existentially-quantified Distance.
  SomeDistance PDouble
calculateSomeDistance duration (MkSomePace space pace) =
  MkSomeDistance space $ calculateDistance duration pace
