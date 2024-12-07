module Running.Config.Args.Command
  ( -- * Command
    Command (..),
    cmdParser,

    -- ** Convert
    ConvertArgs (..),
    ConvertFinal (..),
    fromConvertArgs,

    -- * Misc
    PaceOptUnits,
  )
where

import Options.Applicative
  ( Parser,
  )
import Options.Applicative qualified as OA
import Options.Applicative.Types (ReadM)
import Running.Class.Parser qualified as P
import Running.Config.Args.Utils qualified as Utils
import Running.Data.Distance (SomeDistance)
import Running.Data.Duration (Duration)
import Running.Data.Duration.Units (TimeUnit (Second))
import Running.Data.Pace (SomePace)
import Running.Prelude

-- | Possible commands
data Command
  = -- | Converts between distance, duration, and pace.
    Convert ConvertArgs
  | -- | Scales a value.
    Scale
  deriving stock (Eq, Show)

-- | Args for conversions. All args are optional, though we require exactly
-- 2 to derive the 3rd.
data ConvertArgs = MkConvertArgs
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
fromConvertArgs :: ConvertArgs -> IO ConvertFinal
fromConvertArgs convertArgs = do
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

cmdParser :: Parser Command
cmdParser =
  OA.hsubparser
    ( mconcat
        [ Utils.mkCommand "convert" convertParser convertTxt
        ]
    )
  where
    convertTxt =
      Utils.mkCommandDesc "Converts between quantities. Requires exactly 2 options."

    convertParser = Convert <$> convertArgsParser

convertArgsParser :: Parser ConvertArgs
convertArgsParser = do
  mDuration <- OA.optional durationParser
  mPaceOptUnits <- OA.optional somePaceOptUnitsParser
  mSomeDistance <- OA.optional someDistanceParser

  pure
    $ MkConvertArgs
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

somePaceOptUnitsParser :: Parser PaceOptUnits
somePaceOptUnitsParser =
  OA.option
    read
    ( mconcat
        [ OA.long "pace",
          OA.metavar "TIME_STR [/UNIT]",
          Utils.mkHelp helpTxt
        ]
    )
  where
    helpTxt =
      mconcat
        [ "A pace e.g. '4m30s /km', '1h5m /mi', '4m30'. If the units are not ",
          "given, we use the distance's units. Only kilometers and miles are ",
          "allowed."
        ]
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
