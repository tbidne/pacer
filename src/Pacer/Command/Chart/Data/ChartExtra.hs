module Pacer.Command.Chart.Data.ChartExtra
  ( ChartExtra (..),
    mkChartExtra,
  )
where

import Data.Aeson qualified as Asn
import Pacer.Command.Chart.Data.ChartRequest
  ( ChartRequest
      ( chartType,
        description
      ),
    ChartType (ChartTypeSum),
  )
import Pacer.Prelude
import Pacer.Utils qualified as Utils

-- | Extra chart data.
data ChartExtra = MkChartExtra
  { description :: Maybe Text,
    smoothCurve :: Maybe Double
  }
  deriving stock (Eq, Show)

instance ToJSON ChartExtra where
  toJSON ce =
    Asn.object
      $ Utils.encodeMaybe ("description", ce.description)
      ++ Utils.encodeMaybe ("smoothCurve", ce.smoothCurve)

mkChartExtra :: ChartRequest a -> ChartExtra
mkChartExtra r =
  MkChartExtra
    { description = r.description,
      smoothCurve
    }
  where
    -- If there is any smoothing going on, hardcode 0.25.
    smoothCurve = case r.chartType of
      Just (ChartTypeSum _ mSmooth) -> case mSmooth of
        Just _ -> Just 0.25
        Nothing -> Nothing
      _ -> Nothing
