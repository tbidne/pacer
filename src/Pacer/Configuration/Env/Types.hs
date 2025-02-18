module Pacer.Configuration.Env.Types
  ( -- * Cached paths
    CachedPaths (..),

    -- ** Functions
    getCachedXdgConfigPath,
    getCachedCurrentDirectory,

    -- * Logging Env
    LogEnv (..),
  )
where

import Effectful.Logger.Dynamic (LogLevel)
import Pacer.Prelude

-- | Holds cached paths, for ensuring we do not make unnecessary calls
-- (e.g. lookup the xdg config path twice).
data CachedPaths = MkCachedPaths
  { currentDirectory :: Maybe (Path Abs Dir),
    xdgConfigPath :: Maybe (Path Abs Dir)
  }
  deriving stock (Eq, Show)

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
    (.xdgConfigPath)
    (\p c -> c {xdgConfigPath = Just p})
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
    (.currentDirectory)
    (\p c -> c {currentDirectory = Just p})
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
  { logLevel :: Maybe LogLevel,
    logNamespace :: Namespace
  }
  deriving stock (Eq, Show)
