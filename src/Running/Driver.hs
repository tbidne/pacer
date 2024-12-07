module Running.Driver
  ( -- * Main
    runApp,
    runAppWith,
  )
where

import Options.Applicative qualified as OA
import Running qualified
import Running.Config.Args (Args (command), parserInfo)
import Running.Config.Args.Command
  ( Command (Convert, Scale),
    ConvertFinal (ConvertDistance, ConvertDuration, ConvertPace),
    fromConvertArgs,
  )
import Running.Data.Distance (SomeDistance (MkSomeDistance))
import Running.Data.Distance qualified as Dist
import Running.Data.Distance.Units
  ( DistanceUnit (Kilometer),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Running.Data.Pace (Pace (MkPace))
import Running.Prelude
import System.Exit (die)

runApp :: IO ()
runApp = runAppWith (putStrLn . unpackText)

runAppWith :: (Text -> IO a) -> IO a
runAppWith handler = do
  args <- OA.execParser parserInfo
  case args.command of
    Convert convertArgs -> do
      finalConvertArgs <- fromConvertArgs convertArgs
      case finalConvertArgs of
        ConvertDistance duration pace -> do
          let dist = Running.calculateSomeDistance duration pace
          handler $ display dist
        ConvertDuration paceOptUnits dist -> do
          let duration = case paceOptUnits of
                Left pace -> Running.calculateSomeDuration dist pace
                Right paceDuration -> case dist of
                  MkSomeDistance sdist distx ->
                    case sdist of
                      SMeter ->
                        let disty = Dist.convertDistance distx
                         in Running.calculateDuration disty (MkPace @Kilometer paceDuration)
                      SKilometer -> Running.calculateDuration distx (MkPace paceDuration)
                      SMile -> Running.calculateDuration distx (MkPace paceDuration)
          handler $ display duration
        ConvertPace duration dist -> do
          let pace = Running.calculateSomePace dist duration
          handler $ display pace
    Scale -> die "Not yet implemented"
