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
import Data.Set qualified as Set
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
import Pacer.Data.Duration (Duration)
import Pacer.Exception
  ( GarminE (GarminDecode, GarminMeters, GarminOther),
  )
import Pacer.Prelude hiding ((.:))
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
    duration :: Duration a,
    title :: Text
  }
  deriving stock (Eq, Show)

instance
  ( AMonoid a,
    Fromℚ a,
    Ord a,
    P.Parser a,
    Show a,
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
  ( AMonoid a,
    Fromℚ a,
    Ord a,
    P.Parser a,
    Show a,
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

    parseDuration :: Text -> Parser (Duration a)
    parseDuration = failErr . P.parseAllWith movingTimeParser

    movingTimeParser :: P.MParser (Duration a)
    movingTimeParser = do
      h <- MP.takeP (Just "digits") 2
      MPC.char ':'
      m <- MP.takeP (Just "digits") 2
      MPC.char ':'

      -- The last bit might not be an integer e.g. 00:23:40.5 (40.5), so we
      -- take all, round it, then convert back to text for final parsing.
      --
      -- Kind of clumsy, but the alternative would be to give up rounding,
      -- so we do it this way.
      s <- P.parseDigits @Double
      let sTxt = showt $ round @_ @Int s

      failErr $ P.parseAll (h <> "h" <> m <> "m" <> sTxt <> "s")

    parseX :: forall x. (P.Parser x) => Text -> Parser x
    parseX = P.parseAll >>> failErr

    timeFmt = "%Y-%m-%d %H:%M:%S"

readRunsCsv ::
  ( HasCallStack,
    FileReader :> es,
    Logger :> es
  ) =>
  DistanceUnit ->
  Path Abs File ->
  Eff es (SomeRuns Double)
readRunsCsv @es inputDistUnit csvPath = do
  -- 1. Read csv into bytestring.
  bs <- readBinaryFile csvOsPath

  someRunsList <- case inputDistUnit of
    Meter -> throwM GarminMeters
    Kilometer -> bsToRuns Kilometer bs
    Mile -> bsToRuns Mile bs

  case Run.mkSomeRunsFail someRunsList of
    Err err -> throwM $ GarminOther err
    Ok someRuns -> pure someRuns
  where
    csvOsPath = pathToOsPath csvPath
    bsToTxt = decodeUtf8Lenient . toStrictBS

    bsToRuns ::
      forall (d :: DistanceUnit) ->
      (SingI d) =>
      ByteString ->
      Eff es (List (SomeRun Double))
    bsToRuns d bs =
      case Csv.decodeByName @(GarminAct d Double) (fromStrictBS bs) of
        Left err -> throwM $ GarminDecode err
        Right (_, rs) -> foldGarmin [] rs

    toSomeRun :: (SingI d) => GarminRun d Double -> SomeRun Double
    toSomeRun @d gr =
      MkSomeRun (sing @d)
        $ MkRun
          { datetime = TimestampTime gr.datetime,
            distance = Distance.forceUnit gr.distance,
            duration = gr.duration,
            labels = Set.fromList [],
            title = Just gr.title
          }

    foldGarmin ::
      forall d.
      (SingI d) =>
      List (SomeRun Double) ->
      Records (GarminAct d Double) ->
      Eff es (List (SomeRun Double))
    foldGarmin acc = \case
      (Nil Nothing leftover) -> do
        unless (BSL.null leftover)
          $ $(Logger.logWarn)
          $ "Csv bytes leftover: "
          <> (bsToTxt $ leftover)

        pure acc
      (Nil (Just err) leftover) -> do
        $(Logger.logError) $ "Csv parse error: " <> packText err

        unless (BSL.null leftover)
          $ $(Logger.logWarn)
          $ "Csv bytes leftover: "
          <> (bsToTxt $ leftover)

        pure acc
      Cons (Left err) rs -> do
        $(Logger.logError) $ "Csv parse error: " <> packText err

        foldGarmin acc rs
      Cons (Right r) rs -> case r of
        GarminActOther -> foldGarmin acc rs
        GarminActRun gr -> foldGarmin (toSomeRun @d gr : acc) rs

-- heuristics to decide if we should decode csv or json.
getRunsType ::
  (Logger :> es) =>
  -- | Runs path.
  Path Abs File ->
  Eff es RunsType
getRunsType runsPath = do
  if
    -- 1. Ends w/ .csv -> read garmin csv
    | runsExt == [osp|.csv|] -> pure RunsGarmin
    -- 2. Ends w/ .json -> read custom json
    | runsExt == [osp|.json|] -> pure RunsDefault
    | runsExt == [osp|.jsonc|] -> pure RunsDefault
    -- 3. Name contains the strings 'garmin' or 'activities', assume csv
    | "garmin" `T.isInfixOf` runsBaseNameTxt -> pure RunsGarmin
    | "activities" `T.isInfixOf` runsBaseNameTxt -> pure RunsGarmin
    -- 4. Otherwise default to json
    | otherwise -> do
        let msg =
              mconcat
                [ "Unknown file type: '",
                  packText $ decodeLenient runsOsPath,
                  "'. Guessing custom json format."
                ]
        $(Logger.logWarn) msg
        pure RunsDefault
  where
    runsOsPath = pathToOsPath runsPath
    runsBaseName = OsPath.takeBaseName runsOsPath
    runsBaseNameTxt = T.toCaseFold $ packText $ decodeLenient runsBaseName
    runsExt = OsPath.takeExtension runsOsPath
