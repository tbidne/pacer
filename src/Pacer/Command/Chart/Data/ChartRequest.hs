{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.ChartRequest
  ( -- * ChartRequest
    ChartRequest (..),
    YAxisType (..),

    -- ** Type
    ChartType (..),
    ChartSumPeriod (..),

    -- * Garmin
    GarminSettings (..),

    -- * ChartRequests
    ChartRequests (..),
  )
where

import Pacer.Class.Parser (Parser)
import Pacer.Command.Chart.Data.Expr (FilterExpr)
import Pacer.Data.Distance (DistanceUnit)
import Pacer.Prelude
import Pacer.Utils ((.:?:))
import Pacer.Utils qualified as Utils

-- | Possibly y-axes.
data YAxisType
  = YAxisDistance
  | YAxisDuration
  | YAxisPace
  deriving stock (Eq, Show)

instance FromJSON YAxisType where
  parseJSON = asnWithText "YAxisType" $ \case
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
  = ChartSumWeek
  | ChartSumMonth
  | ChartSumYear
  deriving stock (Eq, Show)

instance FromJSON ChartSumPeriod where
  parseJSON = asnWithText "YAxisType" $ \case
    "week" -> pure ChartSumWeek
    "month" -> pure ChartSumMonth
    "year" -> pure ChartSumYear
    other -> fail $ unpackText $ "Unrecognized sum period: " <> other

data ChartType
  = ChartTypeDefault
  | ChartTypeSum ChartSumPeriod
  deriving stock (Eq, Show)

instance FromJSON ChartType where
  parseJSON = asnWithObject "ChartType" $ \v -> do
    n :: Text <- v .: "name"
    case n of
      "default" -> do
        Utils.failUnknownFields "ChartType" ["name"] v
        pure ChartTypeDefault
      "sum" -> do
        period <- v .: "period"
        Utils.failUnknownFields "ChartType" ["name", "period"] v
        pure $ ChartTypeSum period
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
  deriving stock (Eq, Show)

instance
  ( Fromℚ a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  FromJSON (ChartRequest a)
  where
  parseJSON = asnWithObject "ChartRequest" $ \v -> do
    chartType <- v .:? "type"
    description <- v .:? "description"
    filters <- v .:?: "filters"
    title <- v .: "title"
    unit <- v .:? "unit"
    yAxis <- v .: "y-axis"
    y1Axis <- v .:? "y1-axis"

    Utils.failUnknownFields
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
  deriving stock (Eq, Show)

instance FromJSON GarminSettings where
  parseJSON = asnWithObject "GarminSettings" $ \v -> do
    distanceUnit <- v .:? "unit"
    Utils.failUnknownFields "GarminSettings" ["unit"] v
    pure
      $ MkGarminSettings
        { distanceUnit
        }

-- | List of chart requests.
data ChartRequests a = MkChartRequests
  { chartRequests :: Seq (ChartRequest a),
    garminSettings :: Maybe GarminSettings
  }
  deriving stock (Eq, Show)

instance
  ( Fromℚ a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  FromJSON (ChartRequests a)
  where
  parseJSON = asnWithObject "ChartRequests" $ \v -> do
    garminSettings <- v .:? "garmin"
    chartRequests <- v .: "charts"
    Utils.failUnknownFields "ChartRequests" ["charts", "garmin"] v
    pure
      $ MkChartRequests
        { chartRequests,
          garminSettings
        }

makeFieldLabelsNoPrefix ''GarminSettings
makeFieldLabelsNoPrefix ''ChartRequest
makeFieldLabelsNoPrefix ''ChartRequests
