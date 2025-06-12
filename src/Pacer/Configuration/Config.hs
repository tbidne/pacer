{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Configuration.Config
  ( -- * Config
    Config (..),
    ConfigWithPath (..),

    -- * Logging
    LogConfig (..),

    -- * Chart
    ChartConfig (..),
    ChartThemeConfig (..),
    ChartTheme (..),
  )
where

import Data.Sequence (Seq (Empty))
import Data.Set (Set)
import FileSystem.OsPath qualified as OsPath
import Pacer.Configuration.Logging (LogLevelParam, LogVerbosity)
import Pacer.Prelude
import Pacer.Utils.Json
  ( FromJSON (parseJSON),
    JsonParser,
    ToJSON (toJSON),
    (.:),
    (.:?),
    (.:?:),
    (.=),
  )
import Pacer.Utils.Json qualified as Json
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
  parseJSON = Json.withObject "Config" $ \v -> do
    chartConfig <- v .:? "chart"
    logConfig <- v .:? "log"
    Json.failUnknownFields "Config" ["chart", "log"] v
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
  parseJSON = Json.withObject "LogConfig" $ \v -> do
    level <- v .:? "level"
    verbosity <- v .:? "verbosity"
    Json.failUnknownFields "LogConfig" ["level", "verbosity"] v
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
    -- | Optional path to chart-requests.json.
    chartRequestsPath :: Maybe OsPath,
    -- | Optional path to directory with activities file(s) and
    -- chart-requests.json.
    dataDir :: Maybe OsPath,
    -- | Optional theme config.
    themeConfig :: Maybe ChartThemeConfig
  }
  deriving stock (Eq, Show)

instance Semigroup ChartConfig where
  MkChartConfig x1 x2 x3 x4 x5 x6 <> MkChartConfig y1 y2 y3 y4 y5 y6 =
    MkChartConfig
      (x1 <|> y1)
      (x2 <> y2)
      (x3 <|> y3)
      (x4 <|> y4)
      (x5 <|> y5)
      (x6 <|> y6)

instance Monoid ChartConfig where
  mempty = MkChartConfig Nothing Empty Nothing Nothing Nothing Nothing

instance FromJSON ChartConfig where
  parseJSON = Json.withObject "ChartConfig" $ \v -> do
    activityLabelsPath <- parseOsPath $ v .:? "activity-labels"
    activityPaths <- parseOsPath $ v .:?: "activities"
    buildDir <- parseOsPath $ v .:? "build-dir"
    dataDir <- parseOsPath $ v .:? "data"
    chartRequestsPath <- parseOsPath $ v .:? "chart-requests"
    themeConfig <- v .:? "theme"

    Json.failUnknownFields
      "ChartConfig"
      [ "activity-labels",
        "activities",
        "build-dir",
        "chart-requests",
        "data",
        "theme"
      ]
      v
    pure
      $ MkChartConfig
        { activityLabelsPath,
          activityPaths,
          buildDir,
          chartRequestsPath,
          dataDir,
          themeConfig
        }
    where
      parseOsPath ::
        forall f.
        (Traversable f) =>
        JsonParser (f FilePath) ->
        JsonParser (f OsPath)
      parseOsPath p = p >>= traverse OsPath.encodeValidFail

data ChartTheme = MkChartTheme
  { background :: Text,
    grid :: Text,
    name :: Text,
    selectorBorder :: Text,
    text :: Text,
    tooltipBackground :: Text,
    tooltip :: Text,
    yBackground :: Text,
    y1Background :: Text,
    zoomDragBackground :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

instance Eq ChartTheme where
  x == y = x.name == y.name

instance Ord ChartTheme where
  x <= y = x.name <= y.name

instance ToJSON ChartTheme where
  toJSON ct =
    Json.object
      [ "background" .= ct.background,
        "grid" .= ct.grid,
        "name" .= ct.name,
        "selectorBorder" .= ct.selectorBorder,
        "text" .= ct.text,
        "tooltipBackground" .= ct.tooltipBackground,
        "tooltip" .= ct.tooltip,
        "yBackground" .= ct.yBackground,
        "y1Background" .= ct.y1Background,
        "zoomDragBackground" .= ct.zoomDragBackground
      ]

instance FromJSON ChartTheme where
  parseJSON = Json.withObject "ChartTheme" $ \v -> do
    background <- v .: "background"
    grid <- v .: "grid"
    name <- v .: "name"
    selectorBorder <- v .: "selectorBorder"
    text <- v .: "text"
    tooltipBackground <- v .: "tooltipBackground"
    tooltip <- v .: "tooltip"
    yBackground <- v .: "yBackground"
    y1Background <- v .: "y1Background"
    zoomDragBackground <- v .: "zoomDragBackground"

    Json.failUnknownFields
      "ChartTheme"
      [ "background",
        "grid",
        "name",
        "selectorBorder",
        "text",
        "tooltipBackground",
        "tooltip",
        "yBackground",
        "y1Background",
        "zoomDragBackground"
      ]
      v

    pure
      $ MkChartTheme
        { background,
          grid,
          name,
          selectorBorder,
          text,
          tooltipBackground,
          tooltip,
          yBackground,
          y1Background,
          zoomDragBackground
        }

data ChartThemeConfig = MkChartThemeConfig
  { defaultTheme :: Maybe Text,
    themes :: Set ChartTheme
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance ToJSON ChartThemeConfig where
  toJSON ct =
    Json.object
      $ Json.encodeMaybe ("default", ct.defaultTheme)
      ++ Json.encodeMonoid ("themes", ct.themes)

instance FromJSON ChartThemeConfig where
  parseJSON = Json.withObject "ChartThemes" $ \v -> do
    defaultTheme <- v .:? "default"
    themes <- v .:?: "themes"
    Json.failUnknownFields
      "ChartThemeConfig"
      [ "default",
        "themes"
      ]
      v
    pure
      $ MkChartThemeConfig
        { defaultTheme,
          themes
        }

makeFieldLabelsNoPrefix ''ConfigWithPath
makeFieldLabelsNoPrefix ''Config
makeFieldLabelsNoPrefix ''LogConfig
makeFieldLabelsNoPrefix ''ChartConfig
makeFieldLabelsNoPrefix ''ChartThemeConfig
makeFieldLabelsNoPrefix ''ChartTheme
