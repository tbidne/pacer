module Pacer.Config.Env.Types
  ( -- * Cached paths
    CachedPaths (..),

    -- ** Functions
    getCachedXdgConfigPath,

    -- * Logging Env
    LogEnv (..),
  )
where

import Effectful.Logger.Dynamic (LogLevel)
import Pacer.Prelude

-- | Holds cached paths, for ensuring we do not make unnecessary calls
-- (e.g. lookup the xdg config path twice).
newtype CachedPaths = MkCachedPaths
  { xdgConfigPath :: Maybe (Path Abs Dir)
  }
  deriving stock (Eq, Show)

instance Semigroup CachedPaths where
  MkCachedPaths x1 <> MkCachedPaths y1 = MkCachedPaths (x1 <|> y1)

instance Monoid CachedPaths where
  mempty = MkCachedPaths empty

-- | Retrieves the xdg config path, updating the cache if necessary.
getCachedXdgConfigPath ::
  forall es.
  ( HasCallStack,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  Eff es (Path Abs Dir)
getCachedXdgConfigPath = do
  cachedPaths <- get @CachedPaths
  let mXdgConfig = cachedPaths.xdgConfigPath
  case mXdgConfig of
    Just xdgConfig -> pure xdgConfig
    Nothing -> do
      xdgConfig <- getXdgConfigPath
      put @CachedPaths
        $ MkCachedPaths
          { xdgConfigPath = Just xdgConfig
          }
      pure xdgConfig

-- | Logging environment.
data LogEnv = MkLogEnv
  { logLevel :: Maybe LogLevel,
    logNamespace :: Namespace
  }
  deriving stock (Eq, Show)
