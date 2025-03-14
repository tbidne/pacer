{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Configuration.Config
  ( Config (..),
    ChartConfig (..),
    ConfigWithPath (..),
  )
where

import Data.Aeson.Types qualified as AsnT
import FileSystem.OsPath qualified as OsPath
import Pacer.Configuration.Logging (LogLevelParam)
import Pacer.Prelude
import Pacer.Utils ((.:?:))
import Pacer.Utils qualified as Utils
import System.IO (FilePath)

-- | Config with its parent directory.
data ConfigWithPath = MkConfigWithPath
  { -- | Directory containing the config file. Used for resolving relative paths.
    dirPath :: Path Abs Dir,
    -- | Config.
    config :: Config
  }
  deriving stock (Eq, Show)

-- NOTE: [User Path]
--
-- Paths that come from users must be OsPath and not Path since the
-- latter requires us to enforce that the user typed either an absolute or
-- relative path, but not both, but that is overly restrictive.

-- | Config configuration.
data Config = MkConfig
  { -- | Chart config.
    chartConfig :: Maybe ChartConfig,
    -- | Optional logging.
    logLevel :: Maybe LogLevelParam
  }
  deriving stock (Eq, Show)

instance Semigroup Config where
  MkConfig x1 x2 <> MkConfig y1 y2 =
    MkConfig (x1 <|> y1) (x2 <|> y2)

instance Monoid Config where
  mempty = MkConfig empty empty

instance FromJSON Config where
  parseJSON = asnWithObject "Config" $ \v -> do
    chartConfig <- v .:? "chart"
    logLevel <- v .:? "log-level"
    Utils.failUnknownFields "Config" ["chart", "log-level"] v
    pure
      $ MkConfig
        { chartConfig,
          logLevel
        }

data ChartConfig = MkChartConfig
  { -- | Build dir.
    buildDir :: Maybe OsPath,
    -- | Optional path to directory with runs file and chart-requests.json.
    dataDir :: Maybe OsPath,
    -- | Optional path to chart-requests.json.
    chartRequestsPath :: Maybe OsPath,
    -- | Optional path to run-labels.json.
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

instance FromJSON ChartConfig where
  parseJSON = asnWithObject "ChartConfig" $ \v -> do
    buildDir <- parseOsPath $ v .:? "build-dir"
    dataDir <- parseOsPath $ v .:? "data"
    chartRequestsPath <- parseOsPath $ v .:? "chart-requests"
    runLabelsPath <- parseOsPath $ v .:? "run-labels"
    runPaths <- parseOsPath $ v .:?: "runs"

    Utils.failUnknownFields
      "ChartConfig"
      [ "build-dir",
        "chart-requests",
        "data",
        "run-labels",
        "runs"
      ]
      v
    pure
      $ MkChartConfig
        { buildDir,
          dataDir,
          chartRequestsPath,
          runLabelsPath,
          runPaths
        }
    where
      parseOsPath ::
        forall f.
        (Traversable f) =>
        AsnT.Parser (f FilePath) ->
        AsnT.Parser (f OsPath)
      parseOsPath p = p >>= traverse OsPath.encodeValidFail

makeFieldLabelsNoPrefix ''ConfigWithPath
makeFieldLabelsNoPrefix ''Config
makeFieldLabelsNoPrefix ''ChartConfig
