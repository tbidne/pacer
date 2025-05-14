module Pacer.Configuration.Env
  ( mkLogEnv,
  )
where

import Effectful.Logger.Dynamic (LogLevel (LevelInfo))
import Pacer.Configuration.Args (Args (logLevel, logVerbosity))
import Pacer.Configuration.Config (Config (logLevel, logVerbosity))
import Pacer.Configuration.Env.Types (LogEnv (MkLogEnv, logLevel, logNamespace, logVerbosity))
import Pacer.Configuration.Logging (LogLevelParam (LogNone, LogSome))
import Pacer.Prelude

mkLogEnv :: Args a -> Maybe Config -> LogEnv
mkLogEnv args mConfig =
  MkLogEnv
    { logLevel,
      logNamespace = mempty,
      logVerbosity
    }
  where
    logLevel = case args.logLevel <|> (mConfig >>= (.logLevel)) of
      Nothing -> Just LevelInfo
      Just LogNone -> Nothing
      Just (LogSome l) -> Just l

    logVerbosity =
      fromMaybe mempty
        $ args.logVerbosity
        <|> (mConfig >>= (.logVerbosity))
