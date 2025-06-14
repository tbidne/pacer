{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.ChartRequest
  ( -- * ChartRequest
    ChartRequest (..),
    YAxisType (..),

    -- ** Type
    ChartType (..),
    ChartSumPeriod (..),

    -- ** Smoothing
    ChartSmoothType (..),
    ChartSmooth (..),

    -- * Garmin
    GarminSettings (..),

    -- * ChartRequests
    ChartRequests (..),
  )
where

import Pacer.Class.Parser (Parser (parser))
import Pacer.Class.Parser qualified as P
import Pacer.Command.Chart.Data.Expr (FilterExpr)
import Pacer.Configuration.Config (ChartThemeConfig)
import Pacer.Data.Distance (DistanceUnit)
import Pacer.Prelude
import Pacer.Utils.Json
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    (.:),
    (.:?),
    (.:?:),
  )
import Pacer.Utils.Json qualified as Json
import Text.Megaparsec.Char qualified as MPC

-- | Possibly y-axes.
data YAxisType
  = YAxisDistance
  | YAxisDuration
  | YAxisPace
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance FromJSON YAxisType where
  parseJSON = Json.withText "YAxisType" $ \case
    "distance" -> pure YAxisDistance
    "duration" -> pure YAxisDuration
    "pace" -> pure YAxisPace
    other -> fail $ unpackText err
      where
        err =
          mconcat
            [ "Unexpected y-axis, '",
              other,
              "', expected one of (distance|duration|pace)."
            ]

instance ToJSON YAxisType where
  toJSON YAxisDistance = "distance"
  toJSON YAxisDuration = "duration"
  toJSON YAxisPace = "pace"

data ChartSumPeriod
  = -- | Arbitrary number of days.
    ChartSumDays PWord16
  | -- | Calendar week.
    ChartSumWeek
  | -- | Calendar month.
    ChartSumMonth
  | -- | Calendar year.
    ChartSumYear
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Parser ChartSumPeriod where
  parser =
    asum
      [ parseDays,
        parseWeek,
        parseMonth,
        parseYear
      ]
    where
      parseDays = do
        numDays <- parser
        MPC.space
        MPC.string "days"
        pure $ ChartSumDays numDays
      parseWeek = ChartSumWeek <$ MPC.string "week"
      parseMonth = ChartSumMonth <$ MPC.string "month"
      parseYear = ChartSumYear <$ MPC.string "year"

instance FromJSON ChartSumPeriod where
  parseJSON = Json.withText "ChartSumPeriod" (failErr . P.parseAll)

data ChartSmoothType
  = ChartSmoothRolling
  | ChartSmoothWindow
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving anyclass (NFData)

instance FromJSON ChartSmoothType where
  parseJSON = Json.withText "ChartSmoothType" $ \case
    "rolling" -> pure ChartSmoothRolling
    "window" -> pure ChartSmoothWindow
    other ->
      let msg = "Expected one of (rolling|windows), received: " <> other
       in fail $ unpackText msg

data ChartSmooth = MkChartSmooth
  { smoothPeriod :: PWord8,
    smoothType :: ChartSmoothType
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance FromJSON ChartSmooth where
  parseJSON = Json.withObject "ChartSmooth" $ \v -> do
    smoothPeriod <- mkPositiveFail =<< v .: "period"
    smoothType <- v .: "type"

    Json.failUnknownFields
      "ChartSmooth"
      [ "period",
        "type"
      ]
      v

    pure
      $ MkChartSmooth
        { smoothPeriod,
          smoothType
        }

data ChartType
  = ChartTypeDefault
  | ChartTypeSum ChartSumPeriod (Maybe ChartSmooth)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance FromJSON ChartType where
  parseJSON = Json.withObject "ChartType" $ \v -> do
    n :: Text <- v .: "name"
    case n of
      "default" -> do
        Json.failUnknownFields "ChartType" ["name"] v
        pure ChartTypeDefault
      "sum" -> do
        period <- v .: "period"
        mSmooth <- v .:? "smooth"

        Json.failUnknownFields "ChartType" ["name", "period", "smooth"] v
        pure $ ChartTypeSum period mSmooth
      other -> fail $ unpackText $ "Unrecognized chart type: " <> other

-- | Chart request type.
data ChartRequest a = MkChartRequest
  { -- | Optional chart type.
    chartType :: Maybe ChartType,
    -- | Optional text description.
    description :: Maybe Text,
    -- | Optional list of filters to apply. The filters are "AND'd" together.
    filters :: List (FilterExpr a),
    -- | Title for this chart.
    title :: Text,
    -- | Optional output unit.
    unit :: Maybe DistanceUnit,
    -- | Y-axis value.
    yAxis :: YAxisType,
    -- | Optional second y-axis.
    y1Axis :: Maybe YAxisType
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance
  ( Fromℚ a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  FromJSON (ChartRequest a)
  where
  parseJSON = Json.withObject "ChartRequest" $ \v -> do
    chartType <- v .:? "type"
    description <- v .:? "description"
    filters <- v .:?: "filters"
    title <- v .: "title"
    unit <- v .:? "unit"
    yAxis <- v .: "y-axis"
    y1Axis <- v .:? "y1-axis"

    Json.failUnknownFields
      "ChartRequest"
      [ "description",
        "filters",
        "title",
        "type",
        "unit",
        "y-axis",
        "y1-axis"
      ]
      v

    pure
      $ MkChartRequest
        { chartType,
          description,
          filters,
          title,
          unit,
          yAxis,
          y1Axis
        }

-- | Garmin settings for chart requests.
newtype GarminSettings = MkGarminSettings
  { distanceUnit :: Maybe DistanceUnit
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance FromJSON GarminSettings where
  parseJSON = Json.withObject "GarminSettings" $ \v -> do
    distanceUnit <- v .:? "unit"
    Json.failUnknownFields "GarminSettings" ["unit"] v
    pure
      $ MkGarminSettings
        { distanceUnit
        }

-- | List of chart requests.
data ChartRequests a = MkChartRequests
  { -- | Individual chart requests.
    chartRequests :: NESeq (ChartRequest a),
    -- | Global filters.
    filters :: List (FilterExpr a),
    -- | Garming settings.
    garminSettings :: Maybe GarminSettings,
    -- | Optional theme config.
    themeConfig :: Maybe ChartThemeConfig
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance
  ( Fromℚ a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  FromJSON (ChartRequests a)
  where
  parseJSON = Json.withObject "ChartRequests" $ \v -> do
    garminSettings <- v .:? "garmin"
    chartRequests <- v .: "charts"
    filters <- v .:?: "filters"
    themeConfig <- v .:? "theme"
    Json.failUnknownFields
      "ChartRequests"
      [ "charts",
        "filters",
        "garmin",
        "theme"
      ]
      v
    pure
      $ MkChartRequests
        { chartRequests,
          filters,
          garminSettings,
          themeConfig
        }

makeFieldLabelsNoPrefix ''GarminSettings
makeFieldLabelsNoPrefix ''ChartRequest
makeFieldLabelsNoPrefix ''ChartRequests
