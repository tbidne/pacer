{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Configuration.Config
  ( Config (..),
    LogConfig (..),
    ChartConfig (..),
    ConfigWithPath (..),
  )
where

import Data.Aeson.Types qualified as AsnT
import Data.Sequence (Seq (Empty))
import FileSystem.OsPath qualified as OsPath
import Pacer.Configuration.Logging (LogLevelParam, LogVerbosity)
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
    logConfig :: Maybe LogConfig
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
    logConfig <- v .:? "log"
    Utils.failUnknownFields "Config" ["chart", "log"] v
    pure
      $ MkConfig
        { chartConfig,
          logConfig
        }

data LogConfig = MkLogConfig
  { level :: Maybe LogLevelParam,
    verbosity :: Maybe LogVerbosity
  }
  deriving stock (Eq, Show)

instance Semigroup LogConfig where
  MkLogConfig x1 x2 <> MkLogConfig y1 y2 =
    MkLogConfig (x1 <|> y1) (x2 <|> y2)

instance Monoid LogConfig where
  mempty = MkLogConfig empty empty

instance FromJSON LogConfig where
  parseJSON = asnWithObject "LogConfig" $ \v -> do
    level <- v .:? "level"
    verbosity <- v .:? "verbosity"
    Utils.failUnknownFields "LogConfig" ["level", "verbosity"] v
    pure
      $ MkLogConfig
        { level,
          verbosity
        }

data ChartConfig = MkChartConfig
  { -- | Optional path to activity-labels.json.
    activityLabelsPath :: Maybe OsPath,
    -- | Optional path to activities file.
    activityPaths :: Seq OsPath,
    -- | Build dir.
    buildDir :: Maybe OsPath,
    -- | Optional path to directory with activities file(s) and
    -- chart-requests.json.
    dataDir :: Maybe OsPath,
    -- | Optional path to chart-requests.json.
    chartRequestsPath :: Maybe OsPath
  }
  deriving stock (Eq, Show)

instance Semigroup ChartConfig where
  MkChartConfig x1 x2 x3 x4 x5 <> MkChartConfig y1 y2 y3 y4 y5 =
    MkChartConfig
      (x1 <|> y1)
      (x2 <> y2)
      (x3 <|> y3)
      (x4 <|> y4)
      (x5 <|> y5)

instance Monoid ChartConfig where
  mempty = MkChartConfig Nothing Empty Nothing Nothing Nothing

instance FromJSON ChartConfig where
  parseJSON = asnWithObject "ChartConfig" $ \v -> do
    activityLabelsPath <- parseOsPath $ v .:? "activity-labels"
    activityPaths <- parseOsPath $ v .:?: "activities"
    buildDir <- parseOsPath $ v .:? "build-dir"
    dataDir <- parseOsPath $ v .:? "data"
    chartRequestsPath <- parseOsPath $ v .:? "chart-requests"

    Utils.failUnknownFields
      "ChartConfig"
      [ "build-dir",
        "chart-requests",
        "data",
        "activity-labels",
        "activities"
      ]
      v
    pure
      $ MkChartConfig
        { activityLabelsPath,
          activityPaths,
          buildDir,
          dataDir,
          chartRequestsPath
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
makeFieldLabelsNoPrefix ''LogConfig
makeFieldLabelsNoPrefix ''ChartConfig
