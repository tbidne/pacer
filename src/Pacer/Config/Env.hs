module Pacer.Config.Env
  ( Env (..),
    mkEnv,
  )
where

import Effectful.Logger.Dynamic (LogLevel (LevelInfo))
import Pacer.Config.Args (Args (logLevel))
import Pacer.Config.Logging (LogLevelParam (LogNone, LogSome))
import Pacer.Config.Toml (Toml (logLevel))
import Pacer.Prelude

data Env = MkEnv
  { logLevel :: Maybe LogLevel,
    logNamespace :: Namespace
  }
  deriving stock (Eq, Show)

mkEnv :: Args a -> Maybe Toml -> Env
mkEnv args mToml = MkEnv logLevel mempty
  where
    logLevel = case args.logLevel <|> (mToml >>= (.logLevel)) of
      Nothing -> Just LevelInfo
      Just LogNone -> Nothing
      Just (LogSome l) -> Just l
