module Running
  ( -- * To Pace
    displaySomePace,
    displayPace,
    calculatePace,

    -- * From Pace
    calculateDuration,
  )
where

import Running.Data.Distance
  ( Distance (unDistance),
    DistanceUnit (Kilometer),
    SomeDistance (MkSomeDistance),
    convertDistance,
  )
import Running.Data.Distance.Units (SDistanceUnit (SKilometer, SMeter, SMile))
import Running.Data.Duration (Duration, TimeUnit (Second))
import Running.Data.Pace (Pace, PaceDistF, mkPace)
import Running.Prelude

-- | Given some distance and duration, displays the calculated pace.
displaySomePace ::
  (SingI t) =>
  SomeDistance PDouble ->
  Duration t PDouble ->
  Text
displaySomePace someDistance duration = case someDistance of
  MkSomeDistance s d -> case s of
    SMeter -> displayPace (convertDistance @Kilometer d) duration
    SKilometer -> displayPace d duration
    SMile -> displayPace d duration

-- | Given a distance and duration, displays the calculated pace.
displayPace ::
  forall t d.
  (PaceDistF d, SingI d, SingI t) =>
  Distance d PDouble ->
  Duration t PDouble ->
  Text
displayPace distance = display . calculatePace distance

-- | Given a distance and duration, calculate the pace.
calculatePace ::
  forall t d.
  ( PaceDistF d,
    SingI t
  ) =>
  Distance d PDouble ->
  Duration t PDouble ->
  Pace d PDouble
calculatePace distance duration = mkPace $ duration .% distance.unDistance

-- | Given a pace and distance, calculates the duration.
calculateDuration ::
  Distance d PDouble ->
  Pace d PDouble ->
  Duration Second PDouble
calculateDuration distance pace = pace.unPace .* distance.unDistance
