{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Config.Toml
  ( Toml (..),
    ChartConfig (..),
    TomlWithPath (..),
  )
where

import FileSystem.OsPath qualified as OsPath
import Pacer.Config.Logging (LogLevelParam)
import Pacer.Prelude
import Pacer.Utils qualified as U
import TOML (DecodeTOML (tomlDecoder), getFieldOptWith)

-- | Toml with its parent directory.
data TomlWithPath = MkTomlWithPath
  { -- | Directory containing the toml file. Used for resolving relative paths.
    dirPath :: Path Abs Dir,
    -- | Toml config.
    toml :: Toml
  }
  deriving stock (Eq, Show)

-- NOTE: [User Path]
--
-- Paths that come from users must be OsPath and not Path since the
-- latter requires us to enforce that the user typed either an absolute or
-- relative path, but not both, but that is overly restrictive.

-- | Toml configuration.
data Toml = MkToml
  { -- | Chart config.
    chartConfig :: Maybe ChartConfig,
    -- | Optional logging.
    logLevel :: Maybe LogLevelParam
  }
  deriving stock (Eq, Show)

instance Semigroup Toml where
  MkToml x1 x2 <> MkToml y1 y2 =
    MkToml (x1 <|> y1) (x2 <|> y2)

instance Monoid Toml where
  mempty = MkToml empty empty

instance DecodeTOML Toml where
  tomlDecoder = do
    chartConfig <- TOML.getFieldOptWith tomlDecoder "chart"
    logLevel <- TOML.getFieldOptWith tomlDecoder "log-level"

    pure
      $ MkToml
        { chartConfig,
          logLevel
        }

data ChartConfig = MkChartConfig
  { -- | Build dir.
    buildDir :: Maybe OsPath,
    -- | Optional path to directory with runs file and chart-requests.toml.
    dataDir :: Maybe OsPath,
    -- | Optional path to chart-requests.toml.
    chartRequestsPath :: Maybe OsPath,
    -- | Optional path to run-labels.toml.
    runLabelsPath :: Maybe OsPath,
    -- | Optional path to runs file.
    runPaths :: List OsPath
  }
  deriving stock (Eq, Show)

instance Semigroup ChartConfig where
  MkChartConfig x1 x2 x3 x4 x5 <> MkChartConfig y1 y2 y3 y4 y5 =
    MkChartConfig
      (x1 <|> y1)
      (x2 <|> y2)
      (x3 <|> y3)
      (x4 <|> y4)
      (x5 ++ y5)

instance Monoid ChartConfig where
  mempty = MkChartConfig Nothing Nothing Nothing Nothing []

instance DecodeTOML ChartConfig where
  tomlDecoder = do
    buildDir <- TOML.getFieldOptWith decodeOsPath "build-dir"
    dataDir <- TOML.getFieldOptWith decodeOsPath "data"
    chartRequestsPath <- TOML.getFieldOptWith decodeOsPath "chart-requests"
    runLabelsPath <- TOML.getFieldOptWith decodeOsPath "run-labels"
    runPaths <- U.getFieldOptArrayOfWith decodeOsPath "runs"

    pure
      $ MkChartConfig
        { buildDir,
          dataDir,
          chartRequestsPath,
          runLabelsPath,
          runPaths
        }
    where
      decodeOsPath = tomlDecoder >>= OsPath.encodeValidFail

makeFieldLabelsNoPrefix ''TomlWithPath
makeFieldLabelsNoPrefix ''Toml
makeFieldLabelsNoPrefix ''ChartConfig
