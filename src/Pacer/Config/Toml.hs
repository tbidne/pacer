module Pacer.Config.Toml
  ( Toml (..),
  )
where

import FileSystem.OsPath qualified as OsPath
import Pacer.Prelude
import TOML (DecodeTOML (tomlDecoder), getFieldOptWith)

-- | Toml configuration.
data Toml = MkToml
  { -- | Optional path to directory with runs.toml and chart-requests.toml.
    dataDir :: Maybe OsPath,
    -- | Optional path to chart-requests.toml.
    chartRequestsPath :: Maybe OsPath,
    -- | Optional path to runs.toml.
    runsPath :: Maybe OsPath
  }
  deriving stock (Eq, Show)

instance DecodeTOML Toml where
  tomlDecoder = do
    dataDir <- TOML.getFieldOptWith decodeOsPath "data"
    chartRequestsPath <- TOML.getFieldOptWith decodeOsPath "chart-requests"
    runsPath <- TOML.getFieldOptWith decodeOsPath "runs"

    pure
      $ MkToml
        { dataDir,
          chartRequestsPath,
          runsPath
        }
    where
      decodeOsPath = tomlDecoder >>= OsPath.encodeValidFail
