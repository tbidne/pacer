{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pacer.Command.Chart.Data.Garmin
  ( -- * Types
    GarminAct (..),
    GarminRun (..),

    -- * Functions
    readRunsCsv,

    -- * Misc
    getRunsType,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Csv
  ( FromNamedRecord (parseNamedRecord),
    NamedRecord,
    Parser,
    (.:),
  )
import Data.Csv.Streaming (Records (Cons, Nil))
import Data.Csv.Streaming qualified as Csv
import Data.Text qualified as T
import Data.Time.Format qualified as Format
import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.OsPath (decodeLenient)
import FileSystem.UTF8 (decodeUtf8Lenient)
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Run
  ( Run (MkRun),
    SomeRun (MkSomeRun),
    SomeRuns,
  )
import Pacer.Command.Chart.Data.Run qualified as Run
import Pacer.Command.Chart.Data.Time (Timestamp (TimestampTime))
import Pacer.Command.Chart.Params
  ( RunsType (RunsDefault, RunsGarmin),
  )
import Pacer.Data.Distance (Distance)
import Pacer.Data.Distance qualified as Distance
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
  )
import Pacer.Data.Duration (Seconds)
import Pacer.Data.Result (Result (Err, Ok), failErr)
import Pacer.Exception
  ( GarminE (GarminDecode, GarminMeters, GarminOther),
  )
import Pacer.Prelude
import System.OsPath qualified as OsPath
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

readRunsCsv ::
  ( HasCallStack,
    FileReader :> es,
    Logger :> es
  ) =>
  DistanceUnit ->
  OsPath ->
  Eff es (SomeRuns Double)
readRunsCsv @es inputDistUnit csvPath = do
  -- 1. Read csv into bytestring.
  bs <- readBinaryFile csvPath

  someRunsList <- case inputDistUnit of
    Meter -> throwM GarminMeters
    Kilometer -> bsToRuns Kilometer bs
    Mile -> bsToRuns Mile bs

  case Run.mkSomeRuns someRunsList of
    Err err -> throwM $ GarminOther err
    Ok someRuns -> pure someRuns
  where
    bsToTxt = decodeUtf8Lenient . toStrictBS

    bsToRuns ::
      forall (d :: DistanceUnit) ->
      (SingI d) =>
      ByteString ->
      Eff es (List (SomeRun Double))
    bsToRuns d bs =
      case Csv.decodeByName @(GarminAct d PDouble) (fromStrictBS bs) of
        Left err -> throwM $ GarminDecode err
        Right (_, rs) -> foldGarmin [] rs

    toSomeRun :: (SingI d) => GarminRun d PDouble -> SomeRun Double
    toSomeRun @d gr =
      MkSomeRun (sing @d)
        $ MkRun
          { datetime = TimestampTime gr.datetime,
            distance = Distance.forceUnit gr.distance,
            duration = gr.duration,
            labels = [],
            title = Just gr.title
          }

    foldGarmin ::
      forall d.
      (SingI d) =>
      List (SomeRun Double) ->
      Records (GarminAct d PDouble) ->
      Eff es (List (SomeRun Double))
    foldGarmin acc = \case
      (Nil Nothing leftover) -> do
        unless (BSL.null leftover)
          $ $(Logger.logWarn)
          $ "Csv bytes leftover: "
          <> (bsToTxt $ leftover)

        pure acc
      (Nil (Just err) leftover) -> do
        $(Logger.logWarn) $ "Csv parse error: " <> packText err

        unless (BSL.null leftover)
          $ $(Logger.logWarn)
          $ "Csv bytes leftover: "
          <> (bsToTxt $ leftover)

        pure acc
      Cons (Left err) rs -> do
        $(Logger.logWarn) $ "Csv parse error: " <> packText err

        foldGarmin acc rs
      Cons (Right r) rs -> case r of
        GarminActOther -> foldGarmin acc rs
        GarminActRun gr -> foldGarmin (toSomeRun @d gr : acc) rs

-- heuristics to decide if we should decode csv or toml.
getRunsType ::
  (Logger :> es) =>
  -- | Specified runs type.
  Maybe RunsType ->
  -- | Runs path.
  OsPath ->
  Eff es RunsType
getRunsType mRunsType runsOsPath = do
  let guess =
        if
          -- 1. Ends w/ .csv -> read garmin csv
          | runsExt == [osp|.csv|] -> RunsGarmin
          -- 2. Ends w/ .toml -> read custom toml
          | runsExt == [osp|.toml|] -> RunsDefault
          -- 3. Name contains the strings 'garmin' or 'activities', assume csv
          | "garmin" `T.isInfixOf` runsBaseNameTxt -> RunsGarmin
          | "activities" `T.isInfixOf` runsBaseNameTxt -> RunsGarmin
          -- 4. Otherwise default to toml
          | otherwise -> RunsDefault

  case mRunsType of
    Nothing -> pure guess
    Just runsType ->
      if runsType == guess
        then pure runsType
        else do
          let msg =
                mconcat
                  [ "Specified runs type '",
                    display runsType,
                    "', but guessed '",
                    display guess,
                    "'; assuming the former."
                  ]
          $(Logger.logWarn) msg
          pure runsType
  where
    runsBaseName = OsPath.takeBaseName runsOsPath
    runsBaseNameTxt = T.toCaseFold $ packText $ decodeLenient runsBaseName
    runsExt = OsPath.takeExtension runsOsPath
