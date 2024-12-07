module Running
  ( -- * Conversions

    -- ** To Pace
    calculatePace,
    calculateSomePace,

    -- ** From Pace
    calculateDuration,
    calculateSomeDuration,
  )
where

import Running.Data.Distance
  ( Distance (unDistance),
    SomeDistance (MkSomeDistance),
  )
import Running.Data.Distance qualified as Dist
import Running.Data.Distance.Units (SDistanceUnit (SKilometer, SMeter, SMile))
import Running.Data.Duration (Duration, TimeUnit (Second))
import Running.Data.Pace (Pace, PaceDistF, SomePace (MkSomePace), mkPace)
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
calculateSomePace (MkSomeDistance @d sdist distance) duration =
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

-- TODO: sanity check: ensure that converting the pace instead gives
-- the same result
