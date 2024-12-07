module Running.Config.Args
  ( -- * Main
    runApp,
    runAppWith,

    -- * Args
    Args (..),
    Command (..),
    ConvertArgs (..),
    fromConvertArgs,
    PaceOrDuration,
    getArgs,
  )
where

import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Version (Version (versionBranch))
import Options.Applicative
  ( CommandFields,
    InfoMod,
    Mod,
    Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse), ReadM)
import Paths_running qualified as Paths
import Running qualified
import Running.Class.Parser qualified as P
import Running.Data.Distance (SomeDistance (MkSomeDistance))
import Running.Data.Distance qualified as Dist
import Running.Data.Distance.Units
  ( DistanceUnit (Kilometer),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Running.Data.Duration (Duration)
import Running.Data.Duration.Units (TimeUnit (Second))
import Running.Data.Pace (Pace (MkPace), SomePace)
import Running.Prelude
import System.Exit (die)

runApp :: IO ()
runApp = runAppWith (putStrLn . unpackText)

runAppWith :: (Text -> IO a) -> IO a
runAppWith handler = do
  args <- getArgs
  case args.command of
    Convert convertArgs -> do
      finalConvertArgs <- fromConvertArgs convertArgs
      case finalConvertArgs of
        Either1 (duration, pace) -> do
          let dist = Running.calculateSomeDistance duration pace
          handler $ display dist
        Either2 (paceOrDuration, dist) -> do
          let duration = case paceOrDuration of
                Left pace -> Running.calculateSomeDuration dist pace
                Right paceDuration -> case dist of
                  MkSomeDistance sdist distx ->
                    case sdist of
                      SMeter ->
                        let disty = Dist.convertDistance distx
                         in Running.calculateDuration disty (MkPace @Kilometer paceDuration)
                      SKilometer -> Running.calculateDuration distx (MkPace paceDuration)
                      SMile -> Running.calculateDuration distx (MkPace paceDuration)
          handler $ display duration
        Either3 (duration, dist) -> do
          let pace = Running.calculateSomePace dist duration
          handler $ display pace
    Scale -> die "Not yet implemented"

newtype Args = MkArgs
  { command :: Command
  }
  deriving stock (Eq, Show)

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

data Either3 a b c
  = Either1 a
  | Either2 b
  | Either3 c

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

getArgs :: IO Args
getArgs = OA.execParser parserInfo

-- | Optparse-Applicative info.
parserInfo :: ParserInfo Args
parserInfo =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footer,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header = Just "Running: A tool for running."
    footer = Just $ fromString ""
    desc =
      Chunk.paragraph
        $ mconcat
          [ "Running includes several commands useful for runners."
          ]

argsParser :: Parser Args
argsParser = MkArgs <$> cmdParser

cmdParser :: Parser Command
cmdParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "convert" convertParser convertTxt
        ]
    )
    <**> OA.helper
    <**> version
  where
    convertTxt = mkCmdDesc "Converts values. Requires exactly 2 options."

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
          mkHelp "A distance with units e.g. '4 km'."
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
          mkHelp "A length of time e.g. '1h2m3s'."
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
          mkHelp helpTxt
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

mkCommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

mkCmdDesc :: String -> InfoMod a
mkCmdDesc =
  OA.progDescDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v')
  where
    versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)
