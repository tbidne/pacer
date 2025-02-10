-- | Derive functionality.
module Pacer.Command.Derive
  ( -- * Top level handler
    handle,

    -- * Low level functions

    -- ** Distance
    deriveDistance,
    deriveSomeDistance,

    -- ** Duration
    deriveDuration,
    deriveSomeDuration,

    -- ** Pace
    derivePace,
    deriveSomePace,
  )
where

import Pacer.Command.Derive.Params
  ( DeriveParams (quantity, unit),
    DeriveParamsFinal,
    DeriveQuantity (DeriveDistance, DeriveDuration, DerivePace),
  )
import Pacer.Data.Distance
  ( Distance (MkDistance, unDistance),
    HasDistance (hideDistance),
    SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration (Duration (MkDuration))
import Pacer.Data.Duration qualified as Duration
import Pacer.Data.Pace
  ( Pace (MkPace),
    PaceDistF,
    SomePace (MkSomePace),
    mkPace,
  )
import Pacer.Exception qualified as PEx
import Pacer.Prelude

-- | Handles derive command.
handle ::
  forall es a.
  ( Display a,
    Fromℤ a,
    HasCallStack,
    Ord a,
    Semifield a,
    Show a,
    Terminal :> es,
    Toℚ a
  ) =>
  DeriveParamsFinal a ->
  Eff es ()
handle params = case params.quantity of
  DeriveDistance duration pace -> do
    let dist = deriveSomeDistance ((.unPositive) <$> duration) pace
    case params.unit of
      Nothing -> putTextLn $ display dist
      Just unit -> case toSing unit of
        SomeSing (s :: SDistanceUnit e) -> withSingI s $ do
          let dist' = DistU.convertDistance e dist
          putTextLn $ display dist'
  DeriveDuration paceOptUnits dist -> do
    when (isJust params.unit)
      $ throwM PEx.CommandDeriveDurationUnit

    let duration = case paceOptUnits of
          Left pace -> deriveSomeDuration dist pace
          Right paceDuration -> case dist of
            MkSomeDistance sdist distx ->
              case sdist of
                SMeter ->
                  let disty = DistU.convertDistance Kilometer distx
                   in deriveDuration disty (MkPace @Kilometer paceDuration)
                SKilometer -> deriveDuration distx (MkPace paceDuration)
                SMile -> deriveDuration distx (MkPace paceDuration)
    putTextLn $ display duration
  DerivePace duration dist -> do
    let pace = deriveSomePace dist ((.unPositive) <$> duration)
    case params.unit of
      Nothing -> putTextLn $ display pace
      Just unit -> case unit of
        Meter -> throwM PEx.CommandDerivePaceMeters
        Kilometer -> putTextLn $ display $ DistU.convertDistance Kilometer pace
        Mile -> putTextLn $ display $ DistU.convertDistance Mile pace

-- | Given a distance and a duration, derives the pace.
derivePace ::
  forall d a.
  ( MGroup a,
    PaceDistF d
  ) =>
  -- | Distance.
  Distance d (Positive a) ->
  -- | Duration.
  Duration a ->
  -- | Pace.
  Pace d a
derivePace distance duration =
  mkPace $ duration .% distance.unDistance.unPositive

-- | Given an existentially-quantified distance and a duration, derives
-- the pace.
deriveSomePace ::
  forall a.
  (Fromℤ a, Ord a, Semifield a, Show a) =>
  -- | Existentially-quantified distance.
  SomeDistance (Positive a) ->
  -- | Duration.
  Duration a ->
  -- | Pace.
  SomePace a
deriveSomePace (MkSomeDistance sdist distance) duration =
  case sdist of
    SMeter ->
      let distanceKm = DistU.convertToKilometers distance
       in hideDistance $ derivePace distanceKm duration
    SKilometer ->
      hideDistance $ derivePace distance duration
    SMile ->
      hideDistance $ derivePace distance duration

-- | Given a distance and pace, derives the duration.
deriveDuration ::
  (MSemigroup a) =>
  -- | Distance.
  Distance d a ->
  -- | Pace.
  Pace d a ->
  -- | Duration.
  Duration a
deriveDuration distance pace = pace.unPace .* distance.unDistance

-- | Given existentially-quantified distance and pace, derives the duration.
-- Different distance units are converted.
deriveSomeDuration ::
  ( Fromℤ a,
    MGroup a
  ) =>
  -- | Existentially-quantified distance.
  SomeDistance a ->
  -- | Existentially-quantified pace.
  SomePace a ->
  -- | Duration.
  Duration a
deriveSomeDuration
  (MkSomeDistance sdistance distance)
  (MkSomePace @distUnit space pace) =
    deriveDuration distance' pace
    where
      distance' =
        withSingI sdistance
          $ withSingI space
          $ DistU.convertDistance distUnit distance

-- | Given a duration and pace, derives the distance.
deriveDistance ::
  forall d a.
  (MGroup a) =>
  -- | Distance.
  Duration a ->
  -- | Pace.
  Pace d (Positive a) ->
  -- | Distance.
  Distance d a
deriveDistance duration (MkPace (MkDuration paceDuration)) =
  MkDistance $ scaleDuration duration
  where
    -- monomorphic on Second so that we have to use toSeconds
    scaleDuration :: Duration a -> a
    scaleDuration = (.unDuration) . (.% paceDuration.unPositive)

-- | Given a duration and existentially-quantified pace, derives the
-- distance.
deriveSomeDistance ::
  forall a.
  (MGroup a) =>
  -- | Duration.
  Duration a ->
  -- | Existentially-quantified Pace.
  SomePace (Positive a) ->
  -- | Existentially-quantified Distance.
  SomeDistance a
deriveSomeDistance duration (MkSomePace space pace) =
  MkSomeDistance space $ deriveDistance duration pace
