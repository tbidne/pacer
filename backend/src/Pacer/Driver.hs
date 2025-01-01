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
  ( Command (Chart, Convert, Derive, Scale),
    ConvertFinal (ConvertDistance, ConvertPace),
    DeriveFinal (DeriveDistance, DeriveDuration, DerivePace),
    DistanceDurationPaceArgs (mUnit),
    DistancePaceArgs (unit),
    ScaleFinal (ScaleDistance, ScaleDuration, ScalePace),
    argsToConvert,
    argsToDerive,
    argsToScale,
  )
import Pacer.Data.Distance (SomeDistance (MkSomeDistance))
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration qualified as Dur
import Pacer.Data.Pace (Pace (MkPace))
import Pacer.Derive qualified as Derive
import Pacer.Exception qualified as PEx
import Pacer.Prelude

runApp :: IO ()
runApp = runAppWith (putStrLn . unpackText)

runAppWith :: (Text -> IO a) -> IO a
runAppWith handler = do
  setUncaughtExceptionDisplayInnerMatch
    PEx.knownExceptions
    putStrLn

  args <- OA.execParser (parserInfo @Double)
  case args.command of
    Chart chartArgs -> handleChart handler chartArgs
    Convert convertArgs -> handleConvert handler convertArgs
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

handleConvert ::
  forall a b.
  ( FromInteger a,
    Ord a,
    Semifield a,
    Show a,
    ToRational a
  ) =>
  (Text -> IO b) ->
  DistancePaceArgs a ->
  IO b
handleConvert handler dpArgs =
  argsToConvert dpArgs >>= \case
    ConvertDistance dist ->
      case toSing unit of
        SomeSing (s :: SDistanceUnit e) -> withSingI s $ do
          let dist' = DistU.convertDistance_ @_ @e dist
          handler $ display dist'
    ConvertPace pace -> case unit of
      Meter -> throwIO PEx.CommandConvertPaceMeters
      Kilometer -> handler $ display $ DistU.convertDistance_ @_ @Kilometer pace
      Mile -> handler $ display $ DistU.convertDistance_ @_ @Mile pace
  where
    unit = dpArgs.unit

handleDerive ::
  forall a b.
  ( Display a,
    FromInteger a,
    Ord a,
    Semifield a,
    Show a,
    ToRational a
  ) =>
  (Text -> IO b) ->
  DistanceDurationPaceArgs a ->
  IO b
handleDerive handler ddpArgs =
  argsToDerive ddpArgs >>= \case
    DeriveDistance duration pace -> do
      let dist = Derive.deriveSomeDistance ((.unPositive) <$> duration) pace
      case ddpArgs.mUnit of
        Nothing -> handler $ display dist
        Just unit -> case toSing unit of
          SomeSing (s :: SDistanceUnit e) -> withSingI s $ do
            let dist' = DistU.convertDistance_ @_ @e dist
            handler $ display dist'
    DeriveDuration paceOptUnits dist -> do
      when (isJust ddpArgs.mUnit)
        $ throwIO PEx.CommandDeriveDurationUnit

      let duration = case paceOptUnits of
            Left pace -> Derive.deriveSomeDuration dist pace
            Right paceDuration -> case dist of
              MkSomeDistance sdist distx ->
                case sdist of
                  SMeter ->
                    let disty = DistU.convertDistance_ distx
                     in Derive.deriveDuration disty (MkPace @Kilometer paceDuration)
                  SKilometer -> Derive.deriveDuration distx (MkPace paceDuration)
                  SMile -> Derive.deriveDuration distx (MkPace paceDuration)
      handler $ display duration
    DerivePace duration dist -> do
      let pace = Derive.deriveSomePace dist ((.unPositive) <$> duration)
      case ddpArgs.mUnit of
        Nothing -> handler $ display pace
        Just unit -> case unit of
          Meter -> throwIO PEx.CommandDerivePaceMeters
          Kilometer -> handler $ display $ DistU.convertDistance_ @_ @Kilometer pace
          Mile -> handler $ display $ DistU.convertDistance_ @_ @Mile pace

handleScale ::
  forall a b.
  ( FromInteger a,
    Ord a,
    Semifield a,
    Show a,
    ToRational a
  ) =>
  (Text -> IO b) ->
  DistanceDurationPaceArgs a ->
  Positive a ->
  IO b
handleScale handler ddpArgs scaleFactor =
  argsToScale ddpArgs >>= \case
    ScaleDistance dist -> do
      let distScaled = dist .* scaleFactor
      case ddpArgs.mUnit of
        Nothing -> handler $ display distScaled
        Just unit -> case toSing unit of
          SomeSing (s :: SDistanceUnit e) -> withSingI s $ do
            let distScaled' = DistU.convertDistance_ @_ @e distScaled
            handler $ display distScaled'
    ScaleDuration duration -> do
      when (isJust ddpArgs.mUnit)
        $ throwIO PEx.CommandScaleDurationUnit

      handleDisplay $ duration .* scaleFactor
    ScalePace paceOptUnits ->
      case paceOptUnits of
        Left pace -> do
          let paceScaled = pace .* scaleFactor
          case ddpArgs.mUnit of
            Nothing -> handleDisplay paceScaled
            Just unit -> case unit of
              Meter -> throwIO PEx.CommandScalePaceMeters
              Kilometer ->
                handler
                  $ display
                  $ DistU.convertDistance_ @_ @Kilometer paceScaled
              Mile ->
                handler
                  $ display
                  $ DistU.convertDistance_ @_ @Mile paceScaled
        Right duration -> do
          when (isJust ddpArgs.mUnit) $ do
            let example = Dur.toTimeString duration <> " /km"
            throwIO $ PEx.CommandScalePaceUnitNoUnit example

          handleDisplay $ duration .* scaleFactor
  where
    handleDisplay :: forall x. (Display x) => x -> IO b
    handleDisplay = handler . display
