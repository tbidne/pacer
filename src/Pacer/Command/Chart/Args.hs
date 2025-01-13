-- | Chart CLI parsing.
module Pacer.Command.Chart.Args
  ( parser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Pacer.Command.Chart.Params
  ( ChartParams
      ( MkChartParams,
        chartRequestsPath,
        cleanInstall,
        dataDir,
        json,
        runsPath
      ),
    ChartParamsArgs,
  )
import Pacer.Config.Utils qualified as Utils
import Pacer.Prelude

-- | Parse chart args.
parser :: Parser ChartParamsArgs
parser = do
  chartRequestsPath <- mChartRequestsParser
  cleanInstall <- cleanInstallParser
  dataDir <- dataDirParser
  json <- jsonParser
  runsPath <- mRunsParser

  pure
    $ MkChartParams
      { chartRequestsPath,
        cleanInstall,
        dataDir,
        json,
        runsPath
      }

mChartRequestsParser :: Parser (Maybe OsPath)
mChartRequestsParser =
  mOsPathParser Nothing "chart-requests" "PATH" helpTxt
  where
    helpTxt = "Optional path to chart-requests file. Overrides --data."

mRunsParser :: Parser (Maybe OsPath)
mRunsParser = mOsPathParser Nothing "runs" "PATH" helpTxt
  where
    helpTxt = "Optional path to runs file. Overrides --data."

dataDirParser :: Parser (Maybe OsPath)
dataDirParser = mOsPathParser (Just 'd') "data" "PATH" helpTxt
  where
    helpTxt =
      mconcat
        [ "Path to data directory i.e. where we search for chart-requests ",
          "file and runs file. If not given, defaults to the ",
          "XDG config e.g. ~/.config/pacer/."
        ]

mOsPathParser ::
  Maybe Char ->
  String ->
  String ->
  String ->
  Parser (Maybe OsPath)
mOsPathParser mShort long metavar helpTxt = do
  OA.optional
    $ OA.option
      Utils.readOsPath
      ( mconcat
          $ [ OA.long long,
              OA.metavar metavar,
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
