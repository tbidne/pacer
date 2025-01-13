module Pacer.Config.Toml
  ( Toml (..),
  )
where

import FileSystem.OsPath qualified as OsPath
import Pacer.Config.Logging (LogLevelParam)
import Pacer.Prelude
import TOML (DecodeTOML (tomlDecoder), getFieldOptWith)

-- NOTE: [User Path]
--
-- Paths that come from users must be OsPath and not Path since the
-- latter requires us to enforce that the user typed either an absolute or
-- relative path, but not both, but that is overly restrictive.

-- | Toml configuration.
data Toml = MkToml
  { -- | Optional path to directory with runs.toml and chart-requests.toml.
    dataDir :: Maybe OsPath,
    -- | Optional path to chart-requests.toml.
    chartRequestsPath :: Maybe OsPath,
    -- | Optional logging.
    logLevel :: Maybe LogLevelParam,
    -- | Optional path to runs.toml.
    runsPath :: Maybe OsPath
  }
  deriving stock (Eq, Show)

instance DecodeTOML Toml where
  tomlDecoder = do
    dataDir <- TOML.getFieldOptWith decodeOsPath "data"
    chartRequestsPath <- TOML.getFieldOptWith decodeOsPath "chart-requests"
    logLevel <- TOML.getFieldOptWith tomlDecoder "log-level"
    runsPath <- TOML.getFieldOptWith decodeOsPath "runs"

    pure
      $ MkToml
        { dataDir,
          chartRequestsPath,
          logLevel,
          runsPath
        }
    where
      decodeOsPath = tomlDecoder >>= OsPath.encodeValidFail
