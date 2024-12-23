module Pacer.Config.Args.Command
  ( -- * Command
    Command (..),
    cmdParser,

    -- ** Derive
    DeriveFinal (..),
    argsToDerive,

    -- ** Scale
    ScaleFinal (..),
    argsToScale,

    -- * Misc
    DistanceDurationPaceArgs (..),
    PaceOptUnits,
  )
where

import FileSystem.OsPath qualified as OsPath
import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Options.Applicative.Types (ReadM)
import Pacer.Chart (ChartParams (MkChartParams), ChartParamsArgs)
import Pacer.Chart qualified as Chart
import Pacer.Class.Parser qualified as P
import Pacer.Config.Args.Utils qualified as Utils
import Pacer.Data.Distance (SomeDistance)
import Pacer.Data.Duration (Seconds)
import Pacer.Data.Pace (SomePace)
import Pacer.Exception
  ( CommandDeriveE
      ( CommandDeriveArgs0,
        CommandDeriveArgs1,
        CommandDeriveArgs3,
        CommandDeriveNoPaceUnit
      ),
    CommandScaleE
      ( CommandScaleArgs0,
        CommandScaleArgs2,
        CommandScaleArgs3
      ),
  )
import Pacer.Prelude

-- | Possible commands
data Command
  = -- | Generate charts
    Chart ChartParamsArgs
  | -- | Given 2 of distance, duration, and pace, derives the 3rd.
    Derive DistanceDurationPaceArgs
  | -- | Scales a value.
    Scale DistanceDurationPaceArgs PDouble
  deriving stock (Eq, Show)

-- | Args for conversions. All args are optional, though we require exactly
-- 2 to derive the 3rd.
data DistanceDurationPaceArgs = MkDistanceDurationPaceArgs
  { -- | Possible duration.
    mDuration :: Maybe (Seconds PDouble),
    -- | Possible pace.
    mPaceOptUnits :: Maybe PaceOptUnits,
    -- | Possible distance.
    mSomeDistance :: Maybe (SomeDistance PDouble)
  }
  deriving stock (Eq, Show)

-- | Final args for convert command, after parsing.
data DeriveFinal
  = -- | Derives distance from duration and pace.
    DeriveDistance (Seconds PDouble) (SomePace PDouble)
  | -- | Derives duration from pace and distance.
    DeriveDuration PaceOptUnits (SomeDistance PDouble)
  | -- | Derives pace from duration and distance.
    DerivePace (Seconds PDouble) (SomeDistance PDouble)

-- | Converts CLI args into their final type, suitable for running the
-- convert.
argsToDerive :: DistanceDurationPaceArgs -> IO DeriveFinal
argsToDerive deriveArgs = do
  case ( deriveArgs.mDuration,
         deriveArgs.mPaceOptUnits,
         deriveArgs.mSomeDistance
       ) of
    (Just _, Just _, Just _) -> throwIO CommandDeriveArgs3
    (Nothing, Nothing, Nothing) -> throwIO CommandDeriveArgs0
    -- Duration x Pace -> Distance
    (Just a, Just b, Nothing) -> case b of
      Left pace -> pure $ DeriveDistance a pace
      Right _ -> throwIO CommandDeriveNoPaceUnit
    -- PaceOptUnits x Distance -> Duration
    (Nothing, Just b, Just c) -> pure $ DeriveDuration b c
    -- Duration x Distance -> Pace
    (Just a, Nothing, Just c) -> pure $ DerivePace a c
    _ -> throwIO CommandDeriveArgs1

-- TODO: Convert units command

-- | Final args for scale command, after parsing.
data ScaleFinal
  = -- | Scales distance.
    ScaleDistance (SomeDistance PDouble)
  | -- | Scales duation.
    ScaleDuration (Seconds PDouble)
  | -- | Scales pace.
    ScalePace PaceOptUnits

-- NOTE: [ScaleFinal scale factor]
--
-- Notice that the scaling factor is __not__ currently part of ScaleFinal.
-- The reason is that at the actual usage in Driver, we have access to the
-- original ScaleFactor, which is unchanged, so it's redundant.
--
-- If we ever do something facier e.g. "evolve" the args in a phased manner
-- a la Trees That Grow, we may need to add it.

argsToScale :: DistanceDurationPaceArgs -> IO ScaleFinal
argsToScale deriveArgs = do
  case ( deriveArgs.mSomeDistance,
         deriveArgs.mDuration,
         deriveArgs.mPaceOptUnits
       ) of
    (Just _, Just _, Just _) -> throwIO CommandScaleArgs3
    (Nothing, Nothing, Nothing) -> throwIO CommandScaleArgs0
    (Just a, Nothing, Nothing) -> pure $ ScaleDistance a
    (Nothing, Just b, Nothing) -> pure $ ScaleDuration b
    (Nothing, Nothing, Just c) -> pure $ ScalePace c
    _ -> throwIO CommandScaleArgs2

cmdParser :: Parser Command
cmdParser =
  OA.hsubparser
    ( mconcat
        [ Utils.mkCommand "chart" chartParser chartTxt,
          Utils.mkCommand "derive" deriveParser deriveTxt,
          Utils.mkCommand "scale" scaleParser scaleTxt
        ]
    )
  where
    chartTxt = Utils.mkCommandDesc "Generates charts json file"
    deriveTxt =
      Utils.mkCommandDesc
        $ mconcat
          [ "Given two quantities, derives the third. For instance, given a ",
            "distance and a duration, derives the pace."
          ]

    scaleTxt =
      Utils.mkCommandDesc
        "Scales a quantity. Requires exactly one quantity and the scale factor."

    chartParser = Chart <$> chartParamsArgsParser
    deriveParser = Derive <$> convertDistanceDurationPaceArgsParser
    scaleParser =
      Scale
        <$> scaleDistanceDurationPaceArgsParser
        <*> scaleFactorParser

convertDistanceDurationPaceArgsParser :: Parser DistanceDurationPaceArgs
convertDistanceDurationPaceArgsParser =
  distanceDurationPaceArgsParser convertPaceOptUnitsParser

scaleDistanceDurationPaceArgsParser :: Parser DistanceDurationPaceArgs
scaleDistanceDurationPaceArgsParser =
  distanceDurationPaceArgsParser scalePaceOptUnitsParser

distanceDurationPaceArgsParser :: Parser PaceOptUnits -> Parser DistanceDurationPaceArgs
distanceDurationPaceArgsParser paceOptParser = do
  mDuration <- OA.optional durationParser
  mPaceOptUnits <- OA.optional paceOptParser
  mSomeDistance <- OA.optional someDistanceParser

  pure
    $ MkDistanceDurationPaceArgs
      { mDuration,
        mPaceOptUnits,
        mSomeDistance
      }

someDistanceParser :: Parser (SomeDistance PDouble)
someDistanceParser =
  OA.option
    read
    ( mconcat
        [ OA.long "distance",
          OA.metavar "DIST_STR",
          Utils.mkHelp "A distance with units e.g. '4 km'."
        ]
    )
  where
    read :: OA.ReadM (SomeDistance PDouble)
    read =
      OA.str
        >>= ( P.parse >>> \case
                Right y -> pure y
                Left err -> fail $ unpackText err
            )

durationParser :: Parser (Seconds PDouble)
durationParser =
  OA.option
    read
    ( mconcat
        [ OA.long "duration",
          OA.metavar "TIME_STR",
          Utils.mkHelp "A length of time e.g. '1h2m3s'."
        ]
    )
  where
    read :: OA.ReadM (Seconds PDouble)
    read = do
      s <- OA.str
      case P.parse s of
        Right y -> pure y
        Left err -> fail $ unpackText err

-- | Represents the pace. For some conversions, the units are optional,
-- hence we only require the underlying Duration (a Pace is just a duration
-- with a unit, after all).
type PaceOptUnits = Either (SomePace PDouble) (Seconds PDouble)

convertPaceOptUnitsParser :: Parser PaceOptUnits
convertPaceOptUnitsParser = paceOptUnitsParser helpTxt
  where
    helpTxt =
      mconcat
        [ "A pace e.g. '4m30s /km', '1h5m /mi', '4m30'. If the units are not ",
          "given, we use the distance's units. Only kilometers and miles are ",
          "allowed."
        ]

chartParamsArgsParser :: Parser ChartParamsArgs
chartParamsArgsParser = do
  chartRequestsPath <- chartRequestsPathParser
  runsPath <- runsPathParser
  outJsonPath <- outJsonPathParser

  pure
    $ MkChartParams
      { chartRequestsPath,
        runsPath,
        outJsonPath
      }

chartRequestsPathParser :: Parser (Maybe OsPath)
chartRequestsPathParser =
  OA.optional
    $ OA.option
      read
      ( mconcat
          [ OA.short 'c',
            OA.long "chart-requests",
            OA.metavar "PATH",
            Utils.mkHelp
              $ mconcat
                [ "Path to chart-requests toml file. If not given, defaults to '",
                  OsPath.decodeLenient Chart.defChartRequestsPath,
                  "'"
                ]
          ]
      )
  where
    read :: ReadM OsPath
    read = OA.str >>= OsPath.encodeFail

runsPathParser :: Parser (Maybe OsPath)
runsPathParser =
  OA.optional
    $ OA.option
      read
      ( mconcat
          [ OA.short 'r',
            OA.long "runs",
            OA.metavar "PATH",
            Utils.mkHelp
              $ mconcat
                [ "Path to runs toml file. If not given, defaults to '",
                  OsPath.decodeLenient Chart.defRunsPath,
                  "'"
                ]
          ]
      )
  where
    read :: ReadM OsPath
    read = OA.str >>= OsPath.encodeFail

outJsonPathParser :: Parser (Maybe OsPath)
outJsonPathParser =
  OA.optional
    $ OA.option
      read
      ( mconcat
          [ OA.short 'j',
            OA.long "json",
            OA.metavar "PATH",
            Utils.mkHelp
              $ mconcat
                [ "Path to generated json file. If not given, defaults to '",
                  OsPath.decodeLenient Chart.defOutJsonPath,
                  "'"
                ]
          ]
      )
  where
    read :: ReadM OsPath
    read = OA.str >>= OsPath.encodeFail

scalePaceOptUnitsParser :: Parser PaceOptUnits
scalePaceOptUnitsParser = paceOptUnitsParser helpTxt
  where
    helpTxt = "A pace e.g. '4m30s /km', '1h5m /mi', '4m30'."

paceOptUnitsParser :: String -> Parser PaceOptUnits
paceOptUnitsParser helpTxt =
  OA.option
    read
    ( mconcat
        [ OA.long "pace",
          OA.metavar "TIME_STR [/UNIT]",
          Utils.mkHelp helpTxt
        ]
    )
  where
    read :: ReadM PaceOptUnits
    read = do
      s <- OA.str
      case P.parse s of
        Right duration -> pure $ Right duration
        Left err1 -> do
          case P.parse s of
            Right pace -> pure $ Left pace
            Left err2 ->
              fail
                $ mconcat
                  [ "Could not parse pace.\n* Error 1 (without unit):\n",
                    unpackText err1,
                    "\n* Error 2 (with unit):\n",
                    unpackText err2
                  ]

scaleFactorParser :: Parser PDouble
scaleFactorParser =
  OA.option
    read
    ( mconcat
        [ OA.short 'k',
          OA.long "factor",
          OA.metavar "POS_INT",
          Utils.mkHelp helpTxt
        ]
    )
  where
    helpTxt = "The scaling factor."
    read = OA.str >>= (readFail "Double" >=> mkPositiveFail)
