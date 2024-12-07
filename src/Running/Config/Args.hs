module Running.Config.Args
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
import Paths_running qualified as Paths
import Running.Config.Args.Command (Command, cmdParser)
import Running.Prelude

-- | CLI args.
newtype Args = MkArgs
  { -- | Command to run.
    command :: Command
  }
  deriving stock (Eq, Show)

-- | Optparse-Applicative info.
parserInfo :: ParserInfo Args
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
    header = Just "Running: A tool for running."
    footerTxt = Just $ fromString versNum
    desc =
      Chunk.paragraph
        $ mconcat
          [ "Running includes several commands useful for runners."
          ]

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> cmdParser
    <**> OA.helper
    <**> version

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v' <> OA.hidden)

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)
