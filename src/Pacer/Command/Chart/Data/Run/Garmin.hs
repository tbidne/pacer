module Pacer.Command.Chart.Data.Run.Garmin
  ( GarminAct (..),
    GarminRun (..),
  )
where

import Data.Csv (FromNamedRecord (parseNamedRecord), NamedRecord, Parser, (.:))
import Data.Time.Format qualified as Format
import Pacer.Class.Parser qualified as P
import Pacer.Data.Distance (Distance)
import Pacer.Data.Duration (Seconds)
import Pacer.Data.Result (failErr)
import Pacer.Prelude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

data GarminAct d a
  = GarminActRun (GarminRun d a)
  | GarminActOther
  deriving stock (Eq, Show)

data GarminRun d a = MkGarminRun
  { datetime :: LocalTime,
    distance :: Distance d a,
    duration :: Seconds a,
    title :: Text
  }
  deriving stock (Eq, Show)

instance
  ( Fromℚ a,
    MGroup a,
    P.Parser a,
    SingI d
  ) =>
  FromNamedRecord (GarminAct d a)
  where
  parseNamedRecord = parseGarminRow

-- Example format:
--
-- Activity Type,Date,Favorite,Title,Distance,Calories,Time,Avg HR,Max HR,Aerobic TE,Avg Run Cadence,Max Run Cadence,Avg Pace,Best Pace,Total Ascent,Total Descent,Avg Stride Length,Avg Vertical Ratio,Avg Vertical Oscillation,Avg Ground Contact Time,Avg GAP,Normalized Power® (NP®),Training Stress Score®,Avg Power,Max Power,Steps,Total Reps,Total Sets,Decompression,Best Lap Time,Number of Laps,Moving Time,Elapsed Time,Min Elevation,Max Elevation
-- Running,2025-02-06 17:06:03,false,"Wellington Running","5.48","447","00:31:24","153","176","3.6","153","173","5:44","4:06","215","200","1.15","8.6","9.5","281","5:01","371","0.0","343","641","4,808","--","--","No","00:02:15.3","6","00:31:08","00:32:57","-4","190"

parseGarminRow ::
  forall a d.
  ( Fromℚ a,
    P.Parser a,
    P.Parser (Seconds a),
    SingI d
  ) =>
  NamedRecord ->
  Parser (GarminAct d a)
parseGarminRow r = do
  actType :: Text <- r .: "Activity Type"

  if actType == "Running"
    then do
      title <- r .: "Title"
      datetime <- parseDatetime =<< r .: "Date"
      distance <- parseX =<< r .: "Distance"
      duration <- parseDuration =<< r .: "Moving Time"
      pure
        $ GarminActRun
        $ MkGarminRun
          { datetime,
            distance,
            duration,
            title
          }
    else pure GarminActOther
  where
    parseDatetime :: String -> Parser LocalTime
    parseDatetime = Format.parseTimeM False Format.defaultTimeLocale timeFmt

    parseDuration :: Text -> Parser (Seconds a)
    parseDuration = failErr . P.parseAllWith movingTimeParser

    movingTimeParser :: P.MParser (Seconds a)
    movingTimeParser = do
      h <- MP.takeP (Just "digits") 2
      MPC.char ':'
      m <- MP.takeP (Just "digits") 2
      MPC.char ':'
      s <- MP.takeP (Just "digits") 2
      failErr $ P.parseAll (h <> "h" <> m <> "m" <> s <> "s")

    parseX :: forall x. (P.Parser x) => Text -> Parser x
    parseX = P.parseAll >>> failErr

    timeFmt = "%Y-%m-%d %H:%M:%S"
