module Pacer.Config.Args.Command
  ( -- * Command
    Command (..),
    cmdParser,

    -- ** Convert
    ConvertFinal (..),
    argsToConvert,

    -- ** Derive
    DeriveFinal (..),
    argsToDerive,

    -- ** Scale
    ScaleFinal (..),
    argsToScale,

    -- * Misc
    DistancePaceArgs (..),
    DistanceDurationPaceArgs (..),
    PaceOptUnits,
  )
where

import FileSystem.OsPath qualified as OsPath
import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Options.Applicative.Types (ReadM)
import Pacer.Chart (ChartParams (MkChartParams))
import Pacer.Chart qualified as Chart
import Pacer.Class.Parser qualified as P
import Pacer.Config.Args.Utils qualified as Utils
import Pacer.Data.Distance (DistanceUnit, SomeDistance)
import Pacer.Data.Duration (Seconds)
import Pacer.Data.Pace (SomePace)
import Pacer.Exception
  ( CommandConvertE (CommandConvertArgs0, CommandConvertArgs2),
    CommandDeriveE
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
data Command a
  = -- | Generate charts
    Chart ChartParams
  | -- | Converts a quantity.
    Convert (DistancePaceArgs a)
  | -- | Given 2 of distance, duration, and pace, derives the 3rd.
    Derive (DistanceDurationPaceArgs a)
  | -- | Scales a value.
    Scale (DistanceDurationPaceArgs a) (Positive a)
  deriving stock (Eq, Show)

data DistancePaceArgs a = MkDistancePaceArgs
  { -- | Possible distance.
    mSomeDistance :: Maybe (SomeDistance (Positive a)),
    -- | Possible pace.
    mSomePace :: Maybe (SomePace (Positive a)),
    -- | Output unit.
    unit :: DistanceUnit
  }
  deriving stock (Eq, Show)

-- | Final args for scale command, after parsing.
data ConvertFinal a
  = -- | Converts distance.
    ConvertDistance (SomeDistance (Positive a))
  | -- | Converts pace.
    ConvertPace (SomePace (Positive a))

argsToConvert ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  DistancePaceArgs a ->
  m (ConvertFinal a)
argsToConvert deriveArgs = do
  case ( deriveArgs.mSomeDistance,
         deriveArgs.mSomePace
       ) of
    (Just _, Just _) -> throwM CommandConvertArgs2
    (Nothing, Nothing) -> throwM CommandConvertArgs0
    (Just a, Nothing) -> pure $ ConvertDistance a
    (Nothing, Just b) -> pure $ ConvertPace b

-- | Args for derive and scale. All args are optional, though we require
-- exactly 2 and 1, respectively.
data DistanceDurationPaceArgs a = MkDistanceDurationPaceArgs
  { -- | Possible duration.
    mDuration :: Maybe (Seconds (Positive a)),
    -- | Possible pace.
    mPaceOptUnits :: Maybe (PaceOptUnits a),
    -- | Possible distance.
    mSomeDistance :: Maybe (SomeDistance (Positive a)),
    -- | Possible output unit.
    mUnit :: Maybe DistanceUnit
  }
  deriving stock (Eq, Show)

-- | Final args for convert command, after parsing.
data DeriveFinal a
  = -- | Derives distance from duration and pace.
    DeriveDistance (Seconds (Positive a)) (SomePace (Positive a))
  | -- | Derives duration from pace and distance.
    DeriveDuration (PaceOptUnits a) (SomeDistance (Positive a))
  | -- | Derives pace from duration and distance.
    DerivePace (Seconds (Positive a)) (SomeDistance (Positive a))

-- | Converts CLI args into their final type, suitable for running the
-- convert.
argsToDerive ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  DistanceDurationPaceArgs a ->
  m (DeriveFinal a)
argsToDerive deriveArgs = do
  case ( deriveArgs.mDuration,
         deriveArgs.mPaceOptUnits,
         deriveArgs.mSomeDistance
       ) of
    (Just _, Just _, Just _) -> throwM CommandDeriveArgs3
    (Nothing, Nothing, Nothing) -> throwM CommandDeriveArgs0
    -- Duration x Pace -> Distance
    (Just a, Just b, Nothing) -> case b of
      Left pace -> pure $ DeriveDistance a pace
      Right _ -> throwM CommandDeriveNoPaceUnit
    -- PaceOptUnits x Distance -> Duration
    (Nothing, Just b, Just c) -> pure $ DeriveDuration b c
    -- Duration x Distance -> Pace
    (Just a, Nothing, Just c) -> pure $ DerivePace a c
    _ -> throwM CommandDeriveArgs1

-- | Final args for scale command, after parsing.
data ScaleFinal a
  = -- | Scales distance.
    ScaleDistance (SomeDistance (Positive a))
  | -- | Scales duation.
    ScaleDuration (Seconds (Positive a))
  | -- | Scales pace.
    ScalePace (PaceOptUnits a)

-- NOTE: [ScaleFinal scale factor]
--
-- Notice that the scaling factor is __not__ currently part of ScaleFinal.
-- The reason is that at the actual usage in Driver, we have access to the
-- original ScaleFactor, which is unchanged, so it's redundant.
--
-- If we ever do something facier e.g. "evolve" the args in a phased manner
-- a la Trees That Grow, we may need to add it.

argsToScale ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  DistanceDurationPaceArgs a ->
  m (ScaleFinal a)
argsToScale deriveArgs = do
  case ( deriveArgs.mSomeDistance,
         deriveArgs.mDuration,
         deriveArgs.mPaceOptUnits
       ) of
    (Just _, Just _, Just _) -> throwM CommandScaleArgs3
    (Nothing, Nothing, Nothing) -> throwM CommandScaleArgs0
    (Just a, Nothing, Nothing) -> pure $ ScaleDistance a
    (Nothing, Just b, Nothing) -> pure $ ScaleDuration b
    (Nothing, Nothing, Just c) -> pure $ ScalePace c
    _ -> throwM CommandScaleArgs2

cmdParser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (Command a)
cmdParser =
  OA.hsubparser
    ( mconcat
        [ Utils.mkCommand "chart" chartParser chartTxt,
          Utils.mkCommand "convert" convertParser convertTxt,
          Utils.mkCommand "derive" deriveParser deriveTxt,
          Utils.mkCommand "scale" scaleParser scaleTxt
        ]
    )
  where
    chartTxt =
      Utils.mkCommandDesc "Generates charts. Requires npm to be installed."
    convertTxt =
      Utils.mkCommandDesc
        "Converts a quantity. Requires exactly one quantity and the unit."
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
    convertParser = Convert <$> convertDpArgsParser
    deriveParser = Derive <$> deriveDdpArgsParser
    scaleParser =
      Scale
        <$> scaleDdpArgsParser
        <*> scaleFactorParser

convertDpArgsParser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (DistancePaceArgs a)
convertDpArgsParser = do
  mSomeDistance <- OA.optional someDistanceParser
  mSomePace <- OA.optional somePaceParser
  unit <- distanceUnitParser

  pure
    $ MkDistancePaceArgs
      { mSomeDistance,
        mSomePace,
        unit
      }

deriveDdpArgsParser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (DistanceDurationPaceArgs a)
deriveDdpArgsParser =
  ddpArgsParser derivePaceOptUnitsParser

scaleDdpArgsParser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (DistanceDurationPaceArgs a)
scaleDdpArgsParser =
  ddpArgsParser paceOptUnitsParser

ddpArgsParser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (PaceOptUnits a) ->
  Parser (DistanceDurationPaceArgs a)
ddpArgsParser paceOptParser = do
  mDuration <- OA.optional durationParser
  mPaceOptUnits <- OA.optional paceOptParser
  mSomeDistance <- OA.optional someDistanceParser
  mUnit <- OA.optional distanceUnitParser

  pure
    $ MkDistanceDurationPaceArgs
      { mDuration,
        mPaceOptUnits,
        mSomeDistance,
        mUnit
      }

someDistanceParser ::
  forall a.
  ( AMonoid a,
    FromRational a,
    Ord a,
    P.Parser a,
    Show a
  ) =>
  Parser (SomeDistance (Positive a))
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
    read :: OA.ReadM (SomeDistance (Positive a))
    read = readParseable

somePaceParser ::
  forall a.
  ( FromRational a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  Parser (SomePace (Positive a))
somePaceParser =
  OA.option
    read
    ( mconcat
        [ OA.long "pace",
          OA.metavar "TIME_STR (/UNIT)",
          Utils.mkHelp helpTxt
        ]
    )
  where
    read :: ReadM (SomePace (Positive a))
    read = readParseable

    helpTxt = "A pace e.g. '4m30s /km', '1h5m /mi'."

durationParser ::
  forall a.
  ( FromRational a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  Parser (Seconds (Positive a))
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
    read :: OA.ReadM (Seconds (Positive a))
    read = readParseable

-- | Represents the pace. For some conversions, the units are optional,
-- hence we only require the underlying Duration (a Pace is just a duration
-- with a unit, after all).
type PaceOptUnits a = Either (SomePace (Positive a)) (Seconds (Positive a))

derivePaceOptUnitsParser ::
  forall a.
  ( FromRational a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  Parser (PaceOptUnits a)
derivePaceOptUnitsParser = paceOptUnitsParserHelp helpTxt
  where
    helpTxt =
      mconcat
        [ "A pace e.g. '4m30s /km', '1h5m /mi', '4m30'. If the units are not ",
          "given, we use the distance's units. Only kilometers and miles are ",
          "allowed."
        ]

chartParamsArgsParser :: Parser ChartParams
chartParamsArgsParser = do
  cleanInstall <- cleanInstallParser
  dataDir <- dataDirParser
  json <- jsonParser

  pure
    $ MkChartParams
      { cleanInstall,
        dataDir,
        json
      }

dataDirParser :: Parser (Maybe OsPath)
dataDirParser =
  OA.optional
    $ OA.option
      read
      ( mconcat
          [ OA.short 'd',
            OA.long "data",
            OA.metavar "PATH",
            Utils.mkHelp
              $ mconcat
                [ "Path to data directory i.e. where we search for runs.toml ",
                  "and chart-requests.toml. If not given, defaults to the ",
                  "XDG config e.g. ~/.config/pacer/."
                ]
          ]
      )
  where
    read :: ReadM OsPath
    read = OA.str >>= OsPath.encodeFail

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

paceOptUnitsParser ::
  forall a.
  ( FromRational a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  Parser (PaceOptUnits a)
paceOptUnitsParser = paceOptUnitsParserHelp helpTxt
  where
    helpTxt = "A pace e.g. '4m30s /km', '1h5m /mi', '4m30'."

paceOptUnitsParserHelp ::
  forall a.
  ( FromRational a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  String ->
  Parser (PaceOptUnits a)
paceOptUnitsParserHelp helpTxt =
  OA.option
    read
    ( mconcat
        [ OA.long "pace",
          OA.metavar "TIME_STR [/UNIT]",
          Utils.mkHelp helpTxt
        ]
    )
  where
    read :: ReadM (PaceOptUnits a)
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

scaleFactorParser ::
  forall a.
  ( AMonoid a,
    Ord a,
    P.Parser a,
    Show a
  ) =>
  Parser (Positive a)
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
    read = readParseable

distanceUnitParser :: Parser DistanceUnit
distanceUnitParser =
  OA.option
    readParseable
    ( mconcat
        [ OA.short 'u',
          OA.long "unit",
          OA.metavar "UNIT",
          Utils.mkHelp helpTxt
        ]
    )
  where
    helpTxt = "Output unit e.g. 'km', 'miles'."

readParseable :: (P.Parser a) => ReadM a
readParseable =
  OA.str
    >>= ( P.parse >>> \case
            Right y -> pure y
            Left err -> fail $ unpackText err
        )
