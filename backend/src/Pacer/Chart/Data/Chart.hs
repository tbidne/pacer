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
import Pacer.Chart.Data.ChartExtra (ChartExtra)
import Pacer.Chart.Data.ChartExtra qualified as ChartExtra
import Pacer.Chart.Data.ChartOptions (ChartOptions)
import Pacer.Chart.Data.ChartOptions qualified as ChartOptions
import Pacer.Chart.Data.ChartRequest
  ( ChartRequest (unit),
    ChartRequests (unChartRequests),
  )
import Pacer.Chart.Data.Run (SomeRuns)
import Pacer.Data.Distance (DistanceUnit, HasDistance (distanceUnitOf))
import Pacer.Exception (CreateChartE)
import Pacer.Prelude

-- | Holds all data associated to a single chart.
data Chart = MkChart
  { -- | Chart data.
    chartData :: ChartData,
    -- | Chart extra.
    chartExtra :: ChartExtra,
    -- | Chart options.
    chartOptions :: ChartOptions
  }
  deriving stock (Eq, Show)

instance ToJSON Chart where
  toJSON c =
    Asn.object
      [ "datasets" .= c.chartData,
        "extra" .= c.chartExtra,
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
mkChart someRuns request = toChart <$> eChartData
  where
    eChartData = ChartData.mkChartData finalDistUnit someRuns request
    chartExtra = ChartExtra.mkChartExtra request
    chartOptions = ChartOptions.mkChartOptions finalDistUnit request

    toChart chartData =
      MkChart
        { chartData,
          chartExtra,
          chartOptions
        }

    finalDistUnit :: DistanceUnit
    finalDistUnit = fromMaybe (distanceUnitOf someRuns) request.unit
