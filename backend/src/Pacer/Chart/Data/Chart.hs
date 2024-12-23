module Pacer.Chart.Data.Chart
  ( Chart (..),
    mkChart,
    mkCharts,
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import Data.Aeson qualified as Asn
import Pacer.Chart.Data.ChartData (ChartData)
import Pacer.Chart.Data.ChartData qualified as ChartData
import Pacer.Chart.Data.ChartOptions
import Pacer.Chart.Data.ChartOptions qualified as ChartOptions
import Pacer.Chart.Data.ChartRequest
  ( ChartRequest,
    ChartRequests (unChartRequests),
  )
import Pacer.Chart.Data.Run (SomeRuns)
import Pacer.Data.Distance (HasDistance (distanceUnitOf))
import Pacer.Exception (CreateChartE)
import Pacer.Prelude

-- | Holds all data associated to a single chart.
data Chart = MkChart
  { -- | Chart data.
    chartData :: ChartData,
    -- | Chart options.
    chartOptions :: ChartOptions
  }
  deriving stock (Eq, Show)

instance ToJSON Chart where
  toJSON c =
    Asn.object
      [ "datasets" .= c.chartData,
        "options" .= c.chartOptions
      ]

-- | Given runs and chart requests, generates a series of charts, or the
mkCharts ::
  forall a.
  ( FromInteger a,
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a,
    ToReal a
  ) =>
  SomeRuns a ->
  ChartRequests a ->
  Either CreateChartE (Seq Chart)
mkCharts runs = traverse (mkChart runs) . (.unChartRequests)

mkChart ::
  forall a.
  ( FromInteger a,
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a,
    ToReal a
  ) =>
  SomeRuns a ->
  ChartRequest a ->
  Either CreateChartE Chart
mkChart someRuns request = (\cd -> MkChart cd opts) <$> eChartData
  where
    eChartData = ChartData.mkChartData someRuns request
    opts = ChartOptions.mkChartOptions (distanceUnitOf someRuns) request
