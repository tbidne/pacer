{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.Chart
  ( Charts (..),
    Chart (..),
    mkChart,
    mkCharts,
  )
where

import Data.Set qualified as Set
import Effectful.Logger.Dynamic qualified as Logger
import Pacer.Command.Chart.Data.Activity (SomeActivities)
import Pacer.Command.Chart.Data.ChartData (ChartData)
import Pacer.Command.Chart.Data.ChartData qualified as ChartData
import Pacer.Command.Chart.Data.ChartExtra (ChartExtra)
import Pacer.Command.Chart.Data.ChartExtra qualified as ChartExtra
import Pacer.Command.Chart.Data.ChartRequest (ChartRequest, ChartRequests)
import Pacer.Command.Chart.Data.Expr (FilterExpr)
import Pacer.Configuration.Config
  ( ChartConfig,
    ChartThemeConfig
      ( MkChartThemeConfig
      ),
  )
import Pacer.Configuration.Env.Types (LogEnv)
import Pacer.Data.Distance (DistanceUnit, HasDistance (distanceUnitOf))
import Pacer.Exception (CreateChartE)
import Pacer.Prelude
import Pacer.Utils.Json (ToJSON (toJSON), (.=))
import Pacer.Utils.Json qualified as Json
import Pacer.Utils.Show qualified as Show

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

makeFieldLabelsNoPrefix ''Chart

instance ToJSON Chart where
  toJSON c =
    Json.object
      [ "datasets" .= (c ^. #chartData),
        "extra" .= (c ^. #chartExtra),
        "title" .= (c ^. #title)
      ]

-- | Holds multiple charts.
data Charts = MkCharts
  { -- | Individual charts.
    charts :: NESeq Chart,
    -- | Optional theme config.
    theme :: Maybe ChartThemeConfig
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Charts

instance ToJSON Charts where
  toJSON c =
    Json.object
      $ [ "charts" .= (c ^. #charts)
        ]
      ++ Json.encodeMaybe ("theme", c ^. #theme)

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
  Maybe ChartConfig ->
  SomeActivities a ->
  ChartRequests a ->
  Eff es (Result CreateChartE Charts)
mkCharts mConfig activities requests =
  (>>= toCharts)
    . traverse (mkChart (requests ^. #filters) activities)
    . (view #chartRequests)
    $ requests
  where
    toCharts ::
      (NESeq (Result CreateChartE Chart)) ->
      Eff es (Result CreateChartE Charts)
    toCharts xs = do
      themeConfig <- case (configChartThemeCfg, requestsChartThemeCfg) of
        (Nothing, Nothing) -> pure Nothing
        (Just cCfg, Nothing) -> pure $ Just cCfg
        (Nothing, Just rCfg) -> pure $ Just rCfg
        (Just cCfg, Just rCfg) -> do
          -- 1. Get the default theme.
          defaultTheme <- case rCfg ^. #defaultTheme of
            -- 1.1. Requests defaultTheme takes priority.
            Just rDefault -> pure $ Just rDefault
            -- 1.2. Requests defaultTheme does not exist; Take the chart config,
            --      if it exists.
            _ -> pure $ cCfg ^. #defaultTheme

          -- Combine duplicates; warn about duplicates.
          let dupes = (cCfg ^. #themes) `Set.intersection` (rCfg ^. #themes)
              -- Chart requests on LHS since we want it to get priority.
              themes = (rCfg ^. #themes) `Set.union` (cCfg ^. #themes)
          unless (Set.null dupes) $ do
            let msg =
                  mconcat
                    [ "Overriding duplicate themes found in config with ",
                      "those from chart-requests: ",
                      Show.showMapListInline (view #name) dupes
                    ]
            $(Logger.logWarn) msg

          pure $ Just $ MkChartThemeConfig defaultTheme themes

      pure $ fmap (\ys -> MkCharts ys themeConfig) (sequenceA xs)

    configChartThemeCfg = preview (_Just % #themeConfig % _Just) mConfig

    requestsChartThemeCfg = preview (#themeConfig % _Just) requests

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
          title = request ^. #title
        }

    finalDistUnit :: DistanceUnit
    finalDistUnit = fromMaybe (distanceUnitOf someActivities) (request ^. #unit)
