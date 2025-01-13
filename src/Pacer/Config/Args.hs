module Pacer.Config.Args
  ( Args (..),
    parserInfo,
  )
where

import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Version (Version (versionBranch))
import Options.Applicative
  ( Parser,
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
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Pacer.Class.Parser qualified as P
import Pacer.Command qualified as Command
import Pacer.Config.Logging (LogLevelParam)
import Pacer.Config.Logging qualified as Logging
import Pacer.Config.Phase (ConfigPhase (ConfigPhaseArgs))
import Pacer.Config.Utils qualified as Utils
import Pacer.Prelude
import Paths_pacer qualified as Paths

-- | CLI args.
data Args a = MkArgs
  { -- | Command to run.
    command :: Command.Command ConfigPhaseArgs a,
    -- | Optional toml config.
    configPath :: Maybe OsPath,
    -- | Optional logging.
    logLevel :: Maybe LogLevelParam
  }
  deriving stock (Eq, Show)

-- | Optparse-Applicative info.
parserInfo ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  ParserInfo (Args a)
parserInfo =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header = Just "Pacer: A tool for runners."
    footerTxt = Just $ fromString versNum
    desc =
      Chunk.paragraph
        $ mconcat
          [ "Pacer includes several commands useful for runners, including ",
            "the ability to generate graphic charts based on running data."
          ]

argsParser ::
  forall a.
  ( FromRational a,
    Ord a,
    P.Parser a,
    Semifield a,
    Show a
  ) =>
  Parser (Args a)
argsParser =
  MkArgs
    <$> Command.parser
    <*> configParser
    <*> OA.optional Logging.parser
    <**> OA.helper
    <**> version

configParser :: Parser (Maybe OsPath)
configParser =
  OA.optional
    $ OA.option
      Utils.readOsPath
    $ mconcat
      [ OA.short 'c',
        OA.long "config",
        OA.metavar "PATH",
        Utils.mkHelp "Path to optional toml config."
      ]

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v' <> OA.hidden)

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)
