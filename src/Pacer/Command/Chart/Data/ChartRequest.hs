module Pacer.Command.Chart.Data.ChartRequest
  ( -- * ChartRequest
    ChartRequest (..),
    YAxisType (..),

    -- * ChartRequests
    ChartRequests (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (ToJSON (toJSON))
import Pacer.Class.Parser (Parser)
import Pacer.Command.Chart.Data.Expr
import Pacer.Data.Distance (DistanceUnit)
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import TOML
  ( DecodeTOML (tomlDecoder),
  )
import TOML qualified

-- | Possibly y-axes.
data YAxisType
  = YAxisDistance
  | YAxisDuration
  | YAxisPace
  deriving stock (Eq, Show)

instance DecodeTOML YAxisType where
  tomlDecoder =
    tomlDecoder @Text >>= \case
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

-- | Chart request type.
data ChartRequest a = MkChartRequest
  { -- | Optional text description.
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
  ( FromRational a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  DecodeTOML (ChartRequest a)
  where
  tomlDecoder = do
    description <- TOML.getFieldOptWith tomlDecoder "description"
    filters <- Utils.getFieldOptArrayOf "filters"
    title <- TOML.getFieldWith tomlDecoder "title"
    unit <- TOML.getFieldOptWith tomlDecoder "unit"
    yAxis <- TOML.getFieldWith tomlDecoder "y-axis"
    y1Axis <- TOML.getFieldOptWith tomlDecoder "y1-axis"
    pure
      $ MkChartRequest
        { description,
          filters,
          title,
          unit,
          yAxis,
          y1Axis
        }

-- | List of chart requests.
newtype ChartRequests a = MkChartRequests {unChartRequests :: Seq (ChartRequest a)}
  deriving stock (Eq, Show)

instance
  ( FromRational a,
    Ord a,
    Parser a,
    Semifield a,
    Show a
  ) =>
  DecodeTOML (ChartRequests a)
  where
  tomlDecoder = MkChartRequests <$> TOML.getFieldWith tomlDecoder "charts"
