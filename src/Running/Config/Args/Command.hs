module Running.Config.Args.Command
  ( -- * Command
    Command (..),
    cmdParser,

    -- ** Convert
    ConvertArgs (..),
    fromConvertArgs,

    -- * Misc
    PaceOrDuration,
  )
where

import Options.Applicative
  ( Parser,
  )
import Options.Applicative qualified as OA
import Options.Applicative.Types (ReadM)
import Running.Class.Parser qualified as P
import Running.Config.Args.Utils qualified as Utils
import Running.Config.Data
import Running.Data.Distance (SomeDistance)
import Running.Data.Duration (Duration)
import Running.Data.Duration.Units (TimeUnit (Second))
import Running.Data.Pace (SomePace)
import Running.Prelude

data Command
  = Convert ConvertArgs
  | Scale
  deriving stock (Eq, Show)

data ConvertArgs = MkConvertArgs
  { mDuration :: Maybe (Duration Second PDouble),
    mPaceOrDuration :: Maybe PaceOrDuration,
    mSomeDistance :: Maybe (SomeDistance PDouble)
  }
  deriving stock (Eq, Show)

type FinalConvertArgs =
  Either3
    (Tuple2 (Duration Second PDouble) (SomePace PDouble))
    (Tuple2 PaceOrDuration (SomeDistance PDouble))
    (Tuple2 (Duration Second PDouble) (SomeDistance PDouble))

fromConvertArgs :: ConvertArgs -> IO FinalConvertArgs
fromConvertArgs convertArgs = do
  case (convertArgs.mDuration, convertArgs.mPaceOrDuration, convertArgs.mSomeDistance) of
    (Just _, Just _, Just _) ->
      fail "Convert requires exactly 2 options, received 3."
    (Nothing, Nothing, Nothing) ->
      fail "Convert requires exactly 2 options, received 0."
    -- Duration x Pace -> Distance
    (Just a, Just b, Nothing) -> case b of
      Left pace -> pure $ Either1 (a, pace)
      Right _ ->
        fail
          $ mconcat
            [ "Converting duration and pace to distance requires that pace ",
              "has units"
            ]
    -- PaceOrDuration x Distance -> Duration
    (Nothing, Just b, Just c) -> pure $ Either2 (b, c)
    -- Duration x Distance -> Pace
    (Just a, Nothing, Just c) -> pure $ Either3 (a, c)
    _ -> fail "Convert requires exactly 2 options, received 1."

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
  mPaceOrDuration <- OA.optional somePaceOrDurationParser
  mSomeDistance <- OA.optional someDistanceParser

  pure
    $ MkConvertArgs
      { mDuration,
        mPaceOrDuration,
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

type PaceOrDuration = Either (SomePace PDouble) (Duration Second PDouble)

somePaceOrDurationParser :: Parser PaceOrDuration
somePaceOrDurationParser =
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
    read :: ReadM PaceOrDuration
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
