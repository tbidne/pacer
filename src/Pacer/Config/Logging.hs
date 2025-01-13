module Pacer.Config.Logging
  ( LogLevelParam (..),
    parser,

    -- * Functions
    parseLogLevel,
  )
where

import Effectful.Logger.Dynamic
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn),
  )
import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Pacer.Config.Utils qualified as Utils
import Pacer.Prelude
import TOML (DecodeTOML (tomlDecoder))

data LogLevelParam
  = LogNone
  | LogSome LogLevel
  deriving stock (Eq, Show)

instance DecodeTOML LogLevelParam where
  tomlDecoder = tomlDecoder >>= parseLogLevel

parser :: Parser LogLevelParam
parser =
  OA.option
    (OA.str >>= parseLogLevel)
    $ mconcat
      [ OA.long "log-level",
        OA.metavar logLvlStr,
        Utils.mkHelp "Optional log level. Defaults to 'info'."
      ]
  where

parseLogLevel :: (MonadFail m) => Text -> m LogLevelParam
parseLogLevel = \case
  "none" -> pure LogNone
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
logLvlStr = "(none | debug | info | warn | error)"
