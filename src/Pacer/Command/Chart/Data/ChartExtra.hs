module Pacer.Command.Chart.Data.ChartExtra
  ( ChartExtra (..),
    mkChartExtra,
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Asn
import Pacer.Command.Chart.Data.ChartRequest (ChartRequest (description))
import Pacer.Prelude
import Pacer.Utils qualified as Utils

-- | Extra chart data.
newtype ChartExtra = MkChartExtra
  { description :: Maybe Text
  }
  deriving stock (Eq, Show)

instance ToJSON ChartExtra where
  toJSON ce = Asn.object $ Utils.encodeMaybe ("description", ce.description)

mkChartExtra :: ChartRequest a -> ChartExtra
mkChartExtra r =
  MkChartExtra
    { description = r.description
    }
