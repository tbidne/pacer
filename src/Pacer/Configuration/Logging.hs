module Pacer.Configuration.Logging
  ( -- * Levels
    LogLevelParam (..),
    parser,
    parseLogLevel,

    -- * Verbosity
    LogVerbosity (..),
    verbosityParser,
    parseLogVerbosity,
  )
where

import Effectful.Logger.Dynamic
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn),
  )
import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Pacer.Configuration.Utils qualified as Utils
import Pacer.Prelude
import Pacer.Utils.Json (FromJSON (parseJSON))
import Pacer.Utils.Json qualified as Json

data LogLevelParam
  = LogNone
  | LogSome LogLevel
  deriving stock (Eq, Show)

instance FromJSON LogLevelParam where
  parseJSON = Json.withText "LogLevelParam" parseLogLevel

parser :: Parser LogLevelParam
parser =
  OA.option
    (OA.str >>= parseLogLevel)
    $ mconcat
      [ OA.long "log-level",
        OA.metavar logLvlStr,
        OA.completeWith ["debug", "info", "warn", "error", "off"],
        Utils.mkHelp "Optional log level. Defaults to 'info'."
      ]

parseLogLevel :: (MonadFail m) => Text -> m LogLevelParam
parseLogLevel = \case
  "off" -> pure LogNone
  "debug" -> pure $ LogSome LevelDebug
  "info" -> pure $ LogSome LevelInfo
  "warn" -> pure $ LogSome LevelWarn
  "error" -> pure $ LogSome LevelError
  other ->
    fail
      $ unpackText
      $ mconcat
        [ "Expected one of ",
          logLvlStr,
          ", received: ",
          other
        ]

logLvlStr :: (IsString a) => a
logLvlStr = "(debug | info | warn | error | off)"

data LogVerbosity
  = LogV0
  | LogV1
  deriving stock (Eq, Show)

instance Semigroup LogVerbosity where
  LogV1 <> _ = LogV1
  _ <> r = r

instance Monoid LogVerbosity where
  mempty = LogV0

instance FromJSON LogVerbosity where
  parseJSON = Json.withText "LogVerbosity" parseLogVerbosity

verbosityParser :: Parser LogVerbosity
verbosityParser =
  OA.option
    (OA.str >>= parseLogVerbosity)
    $ mconcat
      [ OA.long "log-verbosity",
        OA.metavar logVerbosityStr,
        Utils.mkHelp "Optional log verbosity. Defaults to 0."
      ]

parseLogVerbosity :: (MonadFail m) => Text -> m LogVerbosity
parseLogVerbosity = \case
  "0" -> pure LogV0
  "1" -> pure LogV1
  other ->
    fail
      $ unpackText
      $ mconcat
        [ "Expected one of ",
          logLvlStr,
          ", received: ",
          other
        ]

logVerbosityStr :: (IsString a) => a
logVerbosityStr = "(0 | 1)"
