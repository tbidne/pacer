{-# LANGUAGE TemplateHaskell #-}

module Pacer.Configuration.Args
  ( Args (..),
    parserInfo,
  )
where

import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Version (showVersion)
import FileSystem.OsString qualified as OsString
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
import Pacer.Configuration.Args.TH qualified as TH
import Pacer.Configuration.Logging (LogLevelParam, LogVerbosity)
import Pacer.Configuration.Logging qualified as Logging
import Pacer.Configuration.Phase (ConfigPhase (ConfigPhaseArgs))
import Pacer.Configuration.Utils qualified as Utils
import Pacer.Prelude
import Paths_pacer qualified as Paths
import System.Info qualified as Info

-- | CLI args.
data Args a = MkArgs
  { -- | Command to run.
    command :: Command.Command ConfigPhaseArgs a,
    -- | Optional config.
    configPath :: Maybe OsPath,
    -- | Optional logging.
    logLevel :: Maybe LogLevelParam,
    -- | Optional logging verbosity
    logVerbosity :: Maybe LogVerbosity
  }
  deriving stock (Eq, Show)

-- | Optparse-Applicative info.
parserInfo ::
  forall a.
  ( Fromℚ a,
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
    footerTxt = Just $ fromString versShort
    desc =
      Chunk.paragraph
        $ mconcat
          [ "Pacer includes several commands useful for runners, including ",
            "the ability to generate graphic charts based on running data."
          ]

argsParser ::
  forall a.
  ( Fromℚ a,
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
    <*> OA.optional Logging.verbosityParser
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
        Utils.mkHelp "Path to optional json config. Only affects 'chart' command."
      ]

version :: Parser (a -> a)
version = OA.infoOption versLong (OA.long "version" <> OA.short 'v' <> OA.hidden)

versShort :: String
versShort =
  mconcat
    [ "Version: ",
      showVersion Paths.version,
      " (",
      OsString.decodeLenient versionInfo.gitShortHash,
      ")"
    ]

versLong :: String
versLong =
  L.intercalate
    "\n"
    [ "Pacer: " <> showVersion Paths.version,
      " - Git revision: " <> OsString.decodeLenient versionInfo.gitHash,
      " - Commit date:  " <> OsString.decodeLenient versionInfo.gitCommitDate,
      " - GHC version:  " <> versionInfo.ghc
    ]

data VersionInfo = MkVersionInfo
  { gitCommitDate :: OsString,
    ghc :: String,
    gitHash :: OsString,
    gitShortHash :: OsString
  }

versionInfo :: VersionInfo
versionInfo =
  MkVersionInfo
    { gitCommitDate = d,
      ghc = showVersion Info.compilerVersion,
      gitHash = h,
      gitShortHash = sh
    }
  where
    (d, h, sh) = $$TH.gitData
