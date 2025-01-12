-- | Scale functionality.
module Pacer.Command.Scale
  ( handle,
  )
where

import Pacer.Command.Scale.Params
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit,
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration qualified as Dur
import Pacer.Exception qualified as PEx
import Pacer.Prelude

-- | Handles scale command.
handle ::
  forall m a.
  ( FromInteger a,
    HasCallStack,
    MonadTerminal m,
    MonadThrow m,
    Ord a,
    Semifield a,
    Show a,
    ToRational a
  ) =>
  ScaleParamsFinal a ->
  m ()
handle params = case params.quantity of
  ScaleDistance dist -> do
    let distScaled = dist .* params.factor
    case params.unit of
      Nothing -> putTextLn $ display distScaled
      Just unit -> case toSing unit of
        SomeSing (s :: SDistanceUnit e) -> withSingI s $ do
          let distScaled' = DistU.convertDistance e distScaled
          putTextLn $ display distScaled'
  ScaleDuration duration -> do
    when (isJust params.unit)
      $ throwM PEx.CommandScaleDurationUnit

    handleDisplay $ duration .* params.factor
  ScalePace paceOptUnits ->
    case paceOptUnits of
      Left pace -> do
        let paceScaled = pace .* params.factor
        case params.unit of
          Nothing -> handleDisplay paceScaled
          Just unit -> case unit of
            Meter -> throwM PEx.CommandScalePaceMeters
            Kilometer ->
              putTextLn
                $ display
                $ DistU.convertDistance Kilometer paceScaled
            Mile ->
              putTextLn
                $ display
                $ DistU.convertDistance Mile paceScaled
      Right duration -> do
        when (isJust params.unit) $ do
          let example = Dur.toTimeString duration <> " /km"
          throwM $ PEx.CommandScalePaceUnitNoUnit example

        handleDisplay $ duration .* params.factor
  where
    handleDisplay :: forall x. (Display x) => x -> m ()
    handleDisplay = putTextLn . display
