module Pacer.Driver
  ( -- * Main
    runApp,
  )
where

import FileSystem.OsPath qualified as OsPath
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

runApp ::
  ( HasCallStack,
    MonadFileReader m,
    MonadFileWriter m,
    MonadPathWriter m,
    MonadOptparse m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  m ()
runApp = do
  args <- execParser (parserInfo @Double)
  case args.command of
    Chart chartArgs -> handleChart chartArgs
    Convert convertArgs -> handleConvert convertArgs
    Derive ddpArgs -> handleDerive ddpArgs
    Scale ddpArgs scaleFactor -> handleScale ddpArgs scaleFactor

handleChart ::
  ( HasCallStack,
    MonadFileReader m,
    MonadFileWriter m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  ChartParamsArgs ->
  m ()
handleChart chartParamsArgs = do
  Chart.createChartsJsonFile chartParamsFinal
  putTextLn msg
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
  DistancePaceArgs a ->
  m ()
handleConvert dpArgs =
  argsToConvert dpArgs >>= \case
    ConvertDistance dist ->
      case toSing unit of
        SomeSing (s :: SDistanceUnit e) -> withSingI s $ do
          let dist' = DistU.convertDistance_ @_ @e dist
          putTextLn $ display dist'
    ConvertPace pace -> case unit of
      Meter -> throwM PEx.CommandConvertPaceMeters
      Kilometer -> putTextLn $ display $ DistU.convertDistance_ @_ @Kilometer pace
      Mile -> putTextLn $ display $ DistU.convertDistance_ @_ @Mile pace
  where
    unit = dpArgs.unit

handleDerive ::
  forall m a.
  ( Display a,
    FromInteger a,
    HasCallStack,
    MonadTerminal m,
    MonadThrow m,
    Ord a,
    Semifield a,
    Show a,
    ToRational a
  ) =>
  DistanceDurationPaceArgs a ->
  m ()
handleDerive ddpArgs =
  argsToDerive ddpArgs >>= \case
    DeriveDistance duration pace -> do
      let dist = Derive.deriveSomeDistance ((.unPositive) <$> duration) pace
      case ddpArgs.mUnit of
        Nothing -> putTextLn $ display dist
        Just unit -> case toSing unit of
          SomeSing (s :: SDistanceUnit e) -> withSingI s $ do
            let dist' = DistU.convertDistance_ @_ @e dist
            putTextLn $ display dist'
    DeriveDuration paceOptUnits dist -> do
      when (isJust ddpArgs.mUnit)
        $ throwM PEx.CommandDeriveDurationUnit

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
      putTextLn $ display duration
    DerivePace duration dist -> do
      let pace = Derive.deriveSomePace dist ((.unPositive) <$> duration)
      case ddpArgs.mUnit of
        Nothing -> putTextLn $ display pace
        Just unit -> case unit of
          Meter -> throwM PEx.CommandDerivePaceMeters
          Kilometer -> putTextLn $ display $ DistU.convertDistance_ @_ @Kilometer pace
          Mile -> putTextLn $ display $ DistU.convertDistance_ @_ @Mile pace

handleScale ::
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
  DistanceDurationPaceArgs a ->
  Positive a ->
  m ()
handleScale ddpArgs scaleFactor =
  argsToScale ddpArgs >>= \case
    ScaleDistance dist -> do
      let distScaled = dist .* scaleFactor
      case ddpArgs.mUnit of
        Nothing -> putTextLn $ display distScaled
        Just unit -> case toSing unit of
          SomeSing (s :: SDistanceUnit e) -> withSingI s $ do
            let distScaled' = DistU.convertDistance_ @_ @e distScaled
            putTextLn $ display distScaled'
    ScaleDuration duration -> do
      when (isJust ddpArgs.mUnit)
        $ throwM PEx.CommandScaleDurationUnit

      handleDisplay $ duration .* scaleFactor
    ScalePace paceOptUnits ->
      case paceOptUnits of
        Left pace -> do
          let paceScaled = pace .* scaleFactor
          case ddpArgs.mUnit of
            Nothing -> handleDisplay paceScaled
            Just unit -> case unit of
              Meter -> throwM PEx.CommandScalePaceMeters
              Kilometer ->
                putTextLn
                  $ display
                  $ DistU.convertDistance_ @_ @Kilometer paceScaled
              Mile ->
                putTextLn
                  $ display
                  $ DistU.convertDistance_ @_ @Mile paceScaled
        Right duration -> do
          when (isJust ddpArgs.mUnit) $ do
            let example = Dur.toTimeString duration <> " /km"
            throwM $ PEx.CommandScalePaceUnitNoUnit example

          handleDisplay $ duration .* scaleFactor
  where
    handleDisplay :: forall x. (Display x) => x -> m ()
    handleDisplay = putTextLn . display
