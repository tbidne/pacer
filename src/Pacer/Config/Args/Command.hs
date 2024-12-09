module Pacer.Config.Args.Command
  ( -- * Command
    Command (..),
    cmdParser,

    -- ** Convert
    ConvertFinal (..),
    argsToConvert,

    -- ** Scale
    ScaleFinal (..),
    argsToScale,

    -- * Misc
    DistanceDurationPaceArgs (..),
    PaceOptUnits,
  )
where

import Options.Applicative
  ( Parser,
  )
import Options.Applicative qualified as OA
import Options.Applicative.Types (ReadM)
import Pacer.Class.Parser qualified as P
import Pacer.Config.Args.Utils qualified as Utils
import Pacer.Data.Distance (SomeDistance)
import Pacer.Data.Duration (Duration)
import Pacer.Data.Duration.Units (TimeUnit (Second))
import Pacer.Data.Pace (SomePace)
import Pacer.Prelude

-- | Possible commands
data Command
  = -- | Converts between distance, duration, and pace.
    Convert DistanceDurationPaceArgs
  | -- | Scales a value.
    Scale DistanceDurationPaceArgs PDouble
  deriving stock (Eq, Show)

-- | Args for conversions. All args are optional, though we require exactly
-- 2 to derive the 3rd.
data DistanceDurationPaceArgs = MkDistanceDurationPaceArgs
  { -- | Possible duration.
    mDuration :: Maybe (Duration Second PDouble),
    -- | Possible pace.
    mPaceOptUnits :: Maybe PaceOptUnits,
    -- | Possible distance.
    mSomeDistance :: Maybe (SomeDistance PDouble)
  }
  deriving stock (Eq, Show)

-- | Final args for convert command, after parsing.
data ConvertFinal
  = -- | Converts duration and pace to distance.
    ConvertDistance (Duration Second PDouble) (SomePace PDouble)
  | -- | Converts pace and distance to duration.
    ConvertDuration PaceOptUnits (SomeDistance PDouble)
  | -- | Converts duration and distance to pace.
    ConvertPace (Duration Second PDouble) (SomeDistance PDouble)

-- | Converts CLI args into their final type, suitable for running the
-- convert.
argsToConvert :: DistanceDurationPaceArgs -> IO ConvertFinal
argsToConvert convertArgs = do
  case ( convertArgs.mDuration,
         convertArgs.mPaceOptUnits,
         convertArgs.mSomeDistance
       ) of
    (Just _, Just _, Just _) ->
      throwText "Convert requires exactly 2 options, received 3."
    (Nothing, Nothing, Nothing) ->
      throwText "Convert requires exactly 2 options, received 0."
    -- Duration x Pace -> Distance
    (Just a, Just b, Nothing) -> case b of
      Left pace -> pure $ ConvertDistance a pace
      Right _ ->
        throwText
          $ mconcat
            [ "Converting duration and pace to distance requires that pace ",
              "has units."
            ]
    -- PaceOptUnits x Distance -> Duration
    (Nothing, Just b, Just c) -> pure $ ConvertDuration b c
    -- Duration x Distance -> Pace
    (Just a, Nothing, Just c) -> pure $ ConvertPace a c
    _ -> throwText "Convert requires exactly 2 options, received 1."

-- | Final args for scale command, after parsing.
data ScaleFinal
  = -- | Scales distance.
    ScaleDistance (SomeDistance PDouble)
  | -- | Scales duation.
    ScaleDuration (Duration Second PDouble)
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
argsToScale convertArgs = do
  case ( convertArgs.mSomeDistance,
         convertArgs.mDuration,
         convertArgs.mPaceOptUnits
       ) of
    (Just _, Just _, Just _) ->
      throwText "Scale requires exactly 1 quantity, received 3."
    (Nothing, Nothing, Nothing) ->
      throwText "Scale requires exactly 1 quantity, received 0."
    (Just a, Nothing, Nothing) -> pure $ ScaleDistance a
    (Nothing, Just b, Nothing) -> pure $ ScaleDuration b
    (Nothing, Nothing, Just c) -> pure $ ScalePace c
    _ -> throwText "Scale requires exactly 1 quantity, received 2."

cmdParser :: Parser Command
cmdParser =
  OA.hsubparser
    ( mconcat
        [ Utils.mkCommand "convert" convertParser convertTxt,
          Utils.mkCommand "scale" scaleParser scaleTxt
        ]
    )
  where
    convertTxt =
      Utils.mkCommandDesc "Converts between quantities. Requires exactly 2 options."

    scaleTxt =
      Utils.mkCommandDesc
        "Scales a quantity. Requires exactly one quantity and the scale factor."

    convertParser = Convert <$> convertDistanceDurationPaceArgsParser
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

durationParser :: Parser (Duration Second PDouble)
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
    read :: OA.ReadM (Duration Second PDouble)
    read = do
      s <- OA.str
      case P.parse s of
        Right y -> pure y
        Left err -> fail $ unpackText err

-- | Represents the pace. For some conversions, the units are optional,
-- hence we only require the underlying Duration (a Pace is just a duration
-- with a unit, after all).
type PaceOptUnits = Either (SomePace PDouble) (Duration Second PDouble)

convertPaceOptUnitsParser :: Parser PaceOptUnits
convertPaceOptUnitsParser = paceOptUnitsParser helpTxt
  where
    helpTxt =
      mconcat
        [ "A pace e.g. '4m30s /km', '1h5m /mi', '4m30'. If the units are not ",
          "given, we use the distance's units. Only kilometers and miles are ",
          "allowed."
        ]

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
    read = OA.str >>= (readFail "Double" >=> mkPositiveFailZ)
