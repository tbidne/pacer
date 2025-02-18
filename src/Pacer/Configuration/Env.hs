module Pacer.Configuration.Env
  ( mkLogEnv,
  )
where

import Effectful.Logger.Dynamic (LogLevel (LevelInfo))
import Pacer.Configuration.Args (Args (logLevel))
import Pacer.Configuration.Config (Config (logLevel))
import Pacer.Configuration.Env.Types (LogEnv (MkLogEnv, logLevel, logNamespace))
import Pacer.Configuration.Logging (LogLevelParam (LogNone, LogSome))
import Pacer.Prelude

mkLogEnv :: Args a -> Maybe Config -> LogEnv
mkLogEnv args mConfig =
  MkLogEnv
    { logLevel,
      logNamespace = mempty
    }
  where
    logLevel = case args.logLevel <|> (mConfig >>= (.logLevel)) of
      Nothing -> Just LevelInfo
      Just LogNone -> Nothing
      Just (LogSome l) -> Just l
