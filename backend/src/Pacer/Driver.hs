module Pacer.Driver
  ( -- * Main
    runApp,
    runAppWith,
  )
where

import Options.Applicative qualified as OA
import Pacer.Config.Args (Args (command), parserInfo)
import Pacer.Config.Args.Command
  ( Command (Derive, Scale),
    DeriveFinal (DeriveDistance, DeriveDuration, DerivePace),
    DistanceDurationPaceArgs,
    ScaleFinal (ScaleDistance, ScaleDuration, ScalePace),
    argsToDerive,
    argsToScale,
  )
import Pacer.Data.Distance (SomeDistance (MkSomeDistance))
import Pacer.Data.Distance qualified as Dist
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Pacer.Data.Pace (Pace (MkPace))
import Pacer.Derive qualified as Derive
import Pacer.Prelude

runApp :: IO ()
runApp = runAppWith (putStrLn . unpackText)

runAppWith :: (Text -> IO a) -> IO a
runAppWith handler = do
  args <- OA.execParser parserInfo
  case args.command of
    Derive ddpArgs -> handleDerive handler ddpArgs
    Scale ddpArgs scaleFactor -> handleScale handler ddpArgs scaleFactor

handleDerive :: (Text -> IO a) -> DistanceDurationPaceArgs -> IO a
handleDerive handler ddpArgs =
  argsToDerive ddpArgs >>= \case
    DeriveDistance duration pace -> do
      let dist = Derive.deriveSomeDistance ((.unPositive) <$> duration) pace
      handler $ display dist
    DeriveDuration paceOptUnits dist -> do
      let duration = case paceOptUnits of
            Left pace -> Derive.deriveSomeDuration dist pace
            Right paceDuration -> case dist of
              MkSomeDistance sdist distx ->
                case sdist of
                  SMeter ->
                    let disty = Dist.convertDistance distx
                     in Derive.deriveDuration disty (MkPace @Kilometer paceDuration)
                  SKilometer -> Derive.deriveDuration distx (MkPace paceDuration)
                  SMile -> Derive.deriveDuration distx (MkPace paceDuration)
      handler $ display duration
    DerivePace duration dist -> do
      let pace = Derive.deriveSomePace dist ((.unPositive) <$> duration)
      handler $ display pace

handleScale :: forall a. (Text -> IO a) -> DistanceDurationPaceArgs -> PDouble -> IO a
handleScale handler ddpArgs scaleFactor =
  argsToScale ddpArgs >>= \case
    ScaleDistance dist -> scaleDisplay dist
    ScaleDuration duration -> scaleDisplay duration
    ScalePace paceOptUnits -> case paceOptUnits of
      Left pace -> scaleDisplay pace
      Right duration -> scaleDisplay duration
  where
    scaleDisplay :: forall b. (Display b, MSemiSpace b PDouble) => b -> IO a
    scaleDisplay = handler . display . (.* scaleFactor)