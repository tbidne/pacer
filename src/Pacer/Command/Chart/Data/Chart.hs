module Pacer.Command.Chart.Data.Chart
  ( Charts (..),
    Chart (..),
    mkChart,
    mkCharts,
  )
where

import Data.Aeson qualified as Asn
import Pacer.Command.Chart.Data.Activity (SomeActivities)
import Pacer.Command.Chart.Data.ChartData (ChartData)
import Pacer.Command.Chart.Data.ChartData qualified as ChartData
import Pacer.Command.Chart.Data.ChartExtra (ChartExtra)
import Pacer.Command.Chart.Data.ChartExtra qualified as ChartExtra
import Pacer.Command.Chart.Data.ChartRequest
  ( ChartRequest (title, unit),
    ChartRequests (chartRequests, filters, themeConfig),
    ChartThemeConfig,
  )
import Pacer.Command.Chart.Data.Expr (FilterExpr)
import Pacer.Configuration.Env.Types (LogEnv)
import Pacer.Data.Distance (DistanceUnit, HasDistance (distanceUnitOf))
import Pacer.Exception (CreateChartE)
import Pacer.Prelude
import Pacer.Utils qualified as Utils

-- | Holds multiple charts.
data Charts = MkCharts
  { -- | Individual charts.
    charts :: NESeq Chart,
    -- | Optional theme config.
    theme :: Maybe ChartThemeConfig
  }
  deriving stock (Eq, Show)

instance ToJSON Charts where
  toJSON c =
    Asn.object
      $ [ "charts" .= c.charts
        ]
      ++ Utils.encodeMaybe ("theme", c.theme)

-- | Holds all data associated to a single chart.
data Chart = MkChart
  { -- | Chart data.
    chartData :: ChartData,
    -- | Chart extra.
    chartExtra :: ChartExtra,
    -- | Chart title.
    title :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON Chart where
  toJSON c =
    Asn.object
      [ "datasets" .= c.chartData,
        "extra" .= c.chartExtra,
        "title" .= c.title
      ]

-- | Given activities and chart requests, generates a series of charts, or the
mkCharts ::
  forall es a.
  ( Display a,
    Fromℤ a,
    Logger :> es,
    MetricSpace a,
    Ord a,
    Reader LogEnv :> es,
    Semifield a,
    Show a,
    Toℝ a
  ) =>
  SomeActivities a ->
  ChartRequests a ->
  Eff es (Result CreateChartE Charts)
mkCharts activities requests =
  fmap toCharts
    . traverse (mkChart requests.filters activities)
    . (.chartRequests)
    $ requests
  where
    toCharts :: (NESeq (Result CreateChartE Chart)) -> Result CreateChartE Charts
    toCharts =
      fmap (\xs -> MkCharts xs requests.themeConfig)
        . sequenceA

mkChart ::
  forall es a.
  ( Display a,
    Fromℤ a,
    Logger :> es,
    MetricSpace a,
    Ord a,
    Reader LogEnv :> es,
    Semifield a,
    Show a,
    Toℝ a
  ) =>
  List (FilterExpr a) ->
  SomeActivities a ->
  ChartRequest a ->
  Eff es (Result CreateChartE Chart)
mkChart globalFilters someActivities request = fmap toChart <$> eChartData
  where
    eChartData =
      ChartData.mkChartData
        finalDistUnit
        globalFilters
        someActivities
        request
    chartExtra = ChartExtra.mkChartExtra request

    toChart chartData =
      MkChart
        { chartData,
          chartExtra,
          title = request.title
        }

    finalDistUnit :: DistanceUnit
    finalDistUnit = fromMaybe (distanceUnitOf someActivities) request.unit
