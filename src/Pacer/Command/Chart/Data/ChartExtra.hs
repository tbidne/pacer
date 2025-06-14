{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.ChartExtra
  ( ChartExtra (..),
    mkChartExtra,
  )
where

import Pacer.Command.Chart.Data.ChartRequest
  ( ChartRequest,
    ChartType (ChartTypeSum),
  )
import Pacer.Prelude
import Pacer.Utils.Json (ToJSON (toJSON))
import Pacer.Utils.Json qualified as Json

-- | Extra chart data.
data ChartExtra = MkChartExtra
  { description :: Maybe Text,
    smoothCurve :: Maybe Double
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''ChartExtra

instance ToJSON ChartExtra where
  toJSON ce =
    Json.object
      $ Json.encodeMaybe ("description", ce ^. #description)
      ++ Json.encodeMaybe ("smoothCurve", ce ^. #smoothCurve)

mkChartExtra :: ChartRequest a -> ChartExtra
mkChartExtra r =
  MkChartExtra
    { description = r ^. #description,
      smoothCurve
    }
  where
    -- If there is any smoothing going on, hardcode 0.25.
    smoothCurve = case r ^. #chartType of
      Just (ChartTypeSum _ mSmooth) -> case mSmooth of
        Just _ -> Just 0.25
        Nothing -> Nothing
      _ -> Nothing
