module Pacer.Configuration.Env
  ( mkLogEnv,
  )
where

import Effectful.Logger.Dynamic (LogLevel (LevelInfo))
import Pacer.Configuration.Args (Args)
import Pacer.Configuration.Config (Config)
import Pacer.Configuration.Env.Types
  ( LogEnv
      ( MkLogEnv,
        level,
        namespace,
        verbosity
      ),
  )
import Pacer.Configuration.Logging (LogLevelParam (LogNone, LogSome), LogVerbosity)
import Pacer.Prelude

mkLogEnv :: Args a -> Maybe Config -> LogEnv
mkLogEnv args mConfig =
  MkLogEnv
    { level,
      namespace = mempty,
      verbosity
    }
  where
    level = case args ^. #logLevel <|> preview logLevelAT mConfig of
      Nothing -> Just LevelInfo
      Just LogNone -> Nothing
      Just (LogSome l) -> Just l

    logLevelAT :: AffineTraversal' (Maybe Config) LogLevelParam
    logLevelAT = _Just % #logConfig %? #level % _Just

    verbosity =
      fromMaybe mempty
        $ args
        ^. #logVerbosity
        <|> preview logVerbosityAT mConfig

    logVerbosityAT :: AffineTraversal' (Maybe Config) LogVerbosity
    logVerbosityAT = _Just % #logConfig %? #verbosity % _Just
