module Pacer.Driver
  ( -- * Main
    runApp,
    runAppWith,
  )
where

import Control.Exception.Annotation.Utils
  ( setUncaughtExceptionDisplayInnerMatch,
  )
import FileSystem.OsPath qualified as OsPath
import Options.Applicative qualified as OA
import Pacer.Chart (ChartParamsArgs)
import Pacer.Chart qualified as Chart
import Pacer.Config.Args (Args (command), parserInfo)
import Pacer.Config.Args.Command
  ( Command (Chart, Derive, Scale),
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
  -- TODO: We should make this more precise at some point.
  setUncaughtExceptionDisplayInnerMatch
    knownExceptions
    putStrLn

  args <- OA.execParser parserInfo
  case args.command of
    Chart chartArgs -> handleChart handler chartArgs
    Derive ddpArgs -> handleDerive handler ddpArgs
    Scale ddpArgs scaleFactor -> handleScale handler ddpArgs scaleFactor

handleChart :: (Text -> IO a) -> ChartParamsArgs -> IO a
handleChart handler chartParamsArgs = do
  Chart.createChartsJsonFile chartParamsFinal

  handler msg
  where
    chartParamsFinal = Chart.advancePhase chartParamsArgs

    msg =
      mconcat
        [ "Successfully created charts.\n",
          "  - runs: '",
          packText $ OsPath.decodeLenient chartParamsFinal.runsPath,
          "'\n  - chart-requests: '",
          packText $ OsPath.decodeLenient chartParamsFinal.chartRequestsPath,
          "'\n  - output: '",
          packText $ OsPath.decodeLenient chartParamsFinal.outJsonPath,
          "'"
        ]

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
                    let disty = Dist.convertDistance_ distx
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
