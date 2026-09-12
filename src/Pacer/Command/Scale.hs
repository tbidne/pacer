-- | Scale functionality.
module Pacer.Command.Scale
  ( handle,
  )
where

import Data.Text qualified as T
import Pacer.Command.Scale.Params
import Pacer.Data.Distance qualified as Dist
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration qualified as Dur
import Pacer.Data.Pace (SomePace)
import Pacer.Exception qualified as PEx
import Pacer.Prelude

-- | Handles scale command.
handle ::
  forall es a.
  ( Display a,
    Field a,
    Fromℚ a,
    HasCallStack,
    Ord a,
    Show a,
    Terminal :> es,
    Toℚ a
  ) =>
  ScaleParamsFinal a ->
  Eff es Unit
handle @es @a params = case params ^. #quantity of
  ScaleDistance dist -> do
    factor <- requireFactor $ params ^. #factor
    let distScaled = dist .* factor
    case params ^. #unit of
      Nothing -> handleDisplay distScaled
      Just unit -> case toSing unit of
        SomeSing @_ @e s -> withSingI s $ do
          let distScaled' = DistU.convertDistance e distScaled
          handleDisplay distScaled'
  ScaleDuration duration -> do
    when (is (#unit % _Just) params)
      $ throwM PEx.CommandScaleDurationUnit

    factor <- requireFactor $ params ^. #factor

    handleDisplay $ duration .* factor
  ScalePace paceOptUnits ->
    case paceOptUnits of
      Left pace -> do
        convFunction <- case params ^. #unit of
          Nothing -> pure $ id @(SomePace a)
          Just unit -> case unit of
            Meter -> throwM PEx.CommandScalePaceMeters
            Kilometer -> pure $ Dist.hideDistance . DistU.convertDistance Kilometer
            Mile -> pure $ Dist.hideDistance . DistU.convertDistance Mile

        case scaleFactor pace of
          Left p -> handleDisplay $ convFunction p
          Right ps -> handleDisplayPaces $ fmap (second convFunction) ps
      Right duration -> do
        when (is (#unit % _Just) params) $ do
          let example = Dur.toTimeString duration <> " /km"
          throwM $ PEx.CommandScalePaceUnitNoUnit example

        case scaleFactor duration of
          Left p -> handleDisplay p
          Right ps -> handleDisplayPaces ps
  where
    handleDisplay :: forall x. (Display x) => x -> Eff es Unit
    handleDisplay = putTextLn . display

    handleDisplayPaces :: forall x. (Display x) => List (Tuple2 Text x) -> Eff es Unit
    handleDisplayPaces paces = do
      let displayPaces (s, p) = s <> display p
          paceStr = T.intercalate "\n" $ displayPaces <$> paces
      putTextLn paceStr

    -- Handle the scale factor, either by multiplying it or using our
    -- built-ins.
    scaleFactor :: forall x. (MSemiSpace x (Positive a)) => x -> Either x (List (Tuple2 Text x))
    scaleFactor val = case params ^. #factor of
      Just k -> Left (val .* k)
      Nothing -> Right (second (val .*) <$> paceFactors)

    -- Would be nice to do this with TH, but sadly typeclasses strike again.
    paceFactors :: List (Tuple2 Text (Positive a))
    paceFactors =
      second (unsafePositive . fromℚ)
        <$> [ ("0.85: ", 0.85),
              ("0.90: ", 0.90),
              ("0.95: ", 0.95),
              ("1.00: ", 1.00),
              ("1.05: ", 1.05),
              ("1.10: ", 1.10),
              ("1.15: ", 1.15),
              ("1.20: ", 1.20)
            ]

requireFactor :: Maybe (Positive a) -> Eff es (Positive a)
requireFactor = \case
  Just k -> pure k
  Nothing -> throwM PEx.CommandScaleFactorRequired
