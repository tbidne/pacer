-- | Chart CLI parsing.
module Pacer.Command.Chart.Args
  ( parser,
  )
where

import Data.Sequence qualified as Seq
import Effectful.Optparse.Completer qualified as EOC
import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Pacer.Command.Chart.Params
  ( ChartParams
      ( MkChartParams,
        activityLabelsPath,
        activityPaths,
        buildDir,
        chartRequestsPath,
        cleanInstall,
        dataDir,
        json,
        port
      ),
    ChartParamsArgs,
  )
import Pacer.Configuration.Utils qualified as Utils
import Pacer.Prelude

-- | Parse chart args.
parser :: Parser ChartParamsArgs
parser = do
  activityLabelsPath <- mActivityLabelsParser
  activityPaths <- mActivitiesParser
  buildDir <- mBuildDirPath
  chartRequestsPath <- mChartRequestsParser
  cleanInstall <- cleanInstallParser
  dataDir <- dataDirParser
  json <- jsonParser
  port <- portParser

  pure
    $ MkChartParams
      { buildDir,
        chartRequestsPath,
        cleanInstall,
        dataDir,
        json,
        activityLabelsPath,
        activityPaths,
        port
      }

mBuildDirPath :: Parser (Maybe OsPath)
mBuildDirPath =
  mOsPathParser Nothing "build-dir" "PATH" helpTxt
  where
    helpTxt =
      mconcat
        [ "Optional path to build directory, used with --json. Defaults ",
          "to ./build."
        ]

mChartRequestsParser :: Parser (Maybe OsPath)
mChartRequestsParser =
  mOsPathParser Nothing "chart-requests" "PATH" helpTxt
  where
    helpTxt = "Optional path to chart-requests file. Overrides --data."

mActivityLabelsParser :: Parser (Maybe OsPath)
mActivityLabelsParser = mOsPathParser Nothing "activity-labels" "PATH" helpTxt
  where
    helpTxt = "Optional path to activity-labels file. Overrides --data."

mActivitiesParser :: Parser (Seq OsPath)
mActivitiesParser =
  Seq.fromList <$> OA.many (osPathParser Nothing "activities" "PATHs..." helpTxt)
  where
    helpTxt =
      mconcat
        [ "Optional path(s) to activities file(s). Overrides --data. We expect ",
          "either default (.json) or garmin (.csv) files."
        ]

dataDirParser :: Parser (Maybe OsPath)
dataDirParser = mOsPathParser (Just 'd') "data" "PATH" helpTxt
  where
    helpTxt =
      mconcat
        [ "Path to data directory i.e. where we search for chart-requests ",
          "file and activities file. If not given, defaults to the ",
          "XDG config e.g. ~/.config/pacer/."
        ]

mOsPathParser ::
  Maybe Char ->
  String ->
  String ->
  String ->
  Parser (Maybe OsPath)
mOsPathParser mShort long metavar =
  OA.optional . osPathParser mShort long metavar

osPathParser ::
  Maybe Char ->
  String ->
  String ->
  String ->
  Parser OsPath
osPathParser mShort long metavar helpTxt = do
  OA.option
    Utils.readOsPath
    ( mconcat
        $ [ OA.long long,
            OA.metavar metavar,
            OA.completer EOC.compgenCwdPathsCompleter,
            Utils.mkHelp helpTxt
          ]
        ++ shortXs
    )
  where
    shortXs = maybe [] ((: []) . OA.short) mShort

jsonParser :: Parser Bool
jsonParser =
  OA.switch
    ( mconcat
        [ OA.short 'j',
          OA.long "json",
          Utils.mkHelp
            $ mconcat
              [ "If active, stops after generating the intermediate json ",
                "file. Primarily used for testing."
              ]
        ]
    )

cleanInstallParser :: Parser Bool
cleanInstallParser =
  OA.switch
    ( mconcat
        [ OA.short 'c',
          OA.long "clean",
          Utils.mkHelp "If active, cleans prior build files."
        ]
    )

portParser :: Parser (Maybe Word16)
portParser =
  OA.optional
    $ OA.option
      OA.auto
      ( mconcat
          [ OA.short 'p',
            OA.long "port",
            Utils.mkHelp "Port upon which the server runs. Defaults to 3000."
          ]
      )
