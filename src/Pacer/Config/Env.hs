module Pacer.Config.Env
  ( mkLogEnv,
  )
where

import Effectful.Logger.Dynamic (LogLevel (LevelInfo))
import Pacer.Config.Args (Args (logLevel))
import Pacer.Config.Env.Types (LogEnv (MkLogEnv, logLevel, logNamespace))
import Pacer.Config.Logging (LogLevelParam (LogNone, LogSome))
import Pacer.Config.Toml (Toml (logLevel))
import Pacer.Prelude

mkLogEnv :: Args a -> Maybe Toml -> LogEnv
mkLogEnv args mToml =
  MkLogEnv
    { logLevel,
      logNamespace = mempty
    }
  where
    logLevel = case args.logLevel <|> (mToml >>= (.logLevel)) of
      Nothing -> Just LevelInfo
      Just LogNone -> Nothing
      Just (LogSome l) -> Just l
