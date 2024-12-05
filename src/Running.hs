{-# OPTIONS_GHC -Wwarn #-}

module Running
  ( -- * To Pace
    displaySomePace,
    displayPace,
    calculatePace,
  )
where

import Running.Class.Singleton (SingI)
import Running.Data.Distance
import Running.Data.Distance.Units (SDistanceUnit (..))
import Running.Data.Duration
import Running.Data.Pace
import Running.Prelude

-- | Given some distance and duration, displays the calculated pace.
displaySomePace :: (SingI t) => SomeDistance PDouble -> Duration t PDouble -> Text
displaySomePace someDistance duration = case someDistance of
  MkSomeDistance s d -> case s of
    SMeter -> displayPace (convertDistance @Kilometer d) duration
    SKilometer -> displayPace d duration
    SMile -> displayPace d duration

-- | Given a distance and duration, displays the calculated pace.
displayPace ::
  forall t d.
  (AllowedDist d, SingI d, SingI t) =>
  Distance d PDouble ->
  Duration t PDouble ->
  Text
displayPace distance = display . calculatePace distance

-- | Given a distance and duration, calculate the pace.
calculatePace ::
  forall t d.
  ( AllowedDist d,
    SingI t
  ) =>
  Distance d PDouble ->
  Duration t PDouble ->
  Pace d PDouble
calculatePace distance duration = mkPace $ duration .% distance.unDistance

-- 3. --pace "5m30s km" --distance marathon
--    Pace Second KM Natural -> Distance Meter Natural -> Duration Second Natural
paceToTime :: Pace d1 a -> Distance d2 a -> Duration Second a
paceToTime = todo
