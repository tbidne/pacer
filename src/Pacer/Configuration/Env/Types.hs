{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Configuration.Env.Types
  ( -- * Cached paths
    CachedPaths (..),

    -- ** Functions
    getCachedXdgConfigPath,
    getCachedCurrentDirectory,

    -- * Logging Env
    LogEnv (..),

    -- ** Handlers
    runLoggerMock,
    runReaderLogEnvMock,
  )
where

import Effectful.Logger.Dynamic (LogLevel, Logger (LoggerLog))
import Pacer.Configuration.Logging (LogVerbosity (LogV0))
import Pacer.Prelude

-- | Holds cached paths, for ensuring we do not make unnecessary calls
-- (e.g. lookup the xdg config path twice).
data CachedPaths = MkCachedPaths
  { currentDirectory :: Maybe (Path Abs Dir),
    xdgConfigPath :: Maybe (Path Abs Dir)
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''CachedPaths

instance Semigroup CachedPaths where
  MkCachedPaths x1 x2 <> MkCachedPaths y1 y2 =
    MkCachedPaths (x1 <|> y1) (x2 <|> y2)

instance Monoid CachedPaths where
  mempty = MkCachedPaths empty empty

-- | Retrieves the xdg config path, updating the cache if necessary.
getCachedXdgConfigPath ::
  forall es.
  ( HasCallStack,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  Eff es (Path Abs Dir)
getCachedXdgConfigPath =
  getCachedPath
    (view #xdgConfigPath)
    (set' #xdgConfigPath . Just)
    getXdgConfigPath

-- | Retrieves the current directory, updating the cache if necessary.
-- As the current directory may change, this is intended to be the _initial_
-- current directory i.e. wherever the program is invoked.
getCachedCurrentDirectory ::
  forall es.
  ( HasCallStack,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  Eff es (Path Abs Dir)
getCachedCurrentDirectory =
  getCachedPath
    (view #currentDirectory)
    (set' #currentDirectory . Just)
    getCurrentDirectory

-- | Retrieves the cached path, updating the cache if necessary.
getCachedPath ::
  forall es b t.
  ( HasCallStack,
    State CachedPaths :> es
  ) =>
  -- | CachedPaths getter.
  (CachedPaths -> Maybe (Path b t)) ->
  -- | CachedPaths setter.
  (Path b t -> CachedPaths -> CachedPaths) ->
  -- | Fallback effectul get function.
  Eff es (Path b t) ->
  Eff es (Path b t)
getCachedPath retrieveCache updateCache getPath = do
  cache <- get @CachedPaths
  case retrieveCache cache of
    Just path -> pure path
    Nothing -> do
      path <- getPath
      put @CachedPaths (updateCache path cache)
      pure path

-- | Logging environment.
data LogEnv = MkLogEnv
  { level :: Maybe LogLevel,
    namespace :: Namespace,
    verbosity :: LogVerbosity
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''LogEnv

runLoggerMock :: Eff (Logger : es) a -> Eff es a
runLoggerMock = interpret_ $ \case
  LoggerLog {} -> pure ()

runReaderLogEnvMock :: Eff (Reader LogEnv : es) a -> Eff es a
runReaderLogEnvMock =
  runReader
    $ MkLogEnv
      { level = Nothing,
        namespace = mempty,
        verbosity = LogV0
      }
