module Running.Config.Args
  ( -- * Main
    runApp,

    -- * Args
    Args (..),
    Command (..),
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
import Running.Data.Distance (SomeDistance)
import Running.Data.Duration (Duration)
import Running.Data.Duration.Units (TimeUnit (Second))
import Running.Data.Pace (SomePace)
import Running.Prelude
import System.Exit (die)

-- TODO: e2e test for this

runApp :: IO ()
runApp = do
  args <- getArgs
  case args.command of
    ConvertToDuration dist pace -> do
      let duration = Running.calculateSomeDuration dist pace
      putStrLn $ unpackText $ display duration
    ConvertToPace dist duration -> do
      let pace = Running.calculateSomePace dist duration
      putStrLn $ unpackText $ display pace
    Scale -> die "Not yet implemented"

newtype Args = MkArgs
  { command :: Command
  }
  deriving stock (Eq, Show)

data Command
  = -- NOTE: We include units on the distance __and__ the Pace. This seems
    -- mildly weird, since it means the units could be different e.g.
    -- --distance "42 mi" --pace "4'50\" /km". There are two reasons for this.
    --
    -- 1. The technical reason is that it's easier. SomePace hides the type
    --    variable, and it requires units. The easiest alternative is to
    --    simply parse a Duration w/ a manual parser (parsePaceDuration) and
    --    note why. This works, but it's arguably clunky.
    --
    -- 2. Arguably mixing units _does_ make sense in some situations e.g.
    --    --distance "10000 m" --pace "4'50\" /km". Thus we leave it.
    ConvertToDuration (SomeDistance PDouble) (SomePace PDouble)
  | ConvertToPace (SomeDistance PDouble) (Duration Second PDouble)
  | Scale
  deriving stock (Eq, Show)

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
        [ mkCommand "to-duration" toDurationParser toDurationTxt,
          mkCommand "to-pace" toPaceParser toPaceTxt
        ]
    )
    <**> OA.helper
    <**> version
  where
    toDurationTxt = mkCmdDesc "Converts a distance and pace to total duration."
    toPaceTxt = mkCmdDesc "Converts a distance and duration to pace."

    toDurationParser = liftA2 ConvertToDuration distanceParser paceParser
    toPaceParser = liftA2 ConvertToPace distanceParser durationParser

distanceParser :: Parser (SomeDistance PDouble)
distanceParser =
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

paceParser :: Parser (SomePace PDouble)
paceParser =
  OA.option
    read
    ( mconcat
        [ OA.long "pace",
          OA.metavar "PACE_STR",
          mkHelp "A pace e.g. 4'30\" /km or 1h 5' /mi."
        ]
    )
  where
    read :: ReadM (SomePace PDouble)
    read = do
      s <- OA.str
      case P.parse s of
        Right y -> pure y
        Left err -> fail $ unpackText err

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
