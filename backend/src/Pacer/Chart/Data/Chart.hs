module Pacer.Chart.Data.Chart
  ( Chart (..),
    ChartData (..),
    mkChart,
    mkCharts,
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import Data.Aeson qualified as Asn
import Data.Foldable1 (Foldable1 (foldMap1))
import Data.List (all)
import Data.List qualified as L
import Data.Sequence (Seq (Empty))
import Data.Sequence.NonEmpty qualified as NESeq
import Pacer.Chart.Data.ChartRequest
  ( ChartRequest (filters, title, yAxis, yAxis1),
    ChartRequests (unChartRequests),
    FilterExpr,
    FilterType (FilterLabel),
    YAxisType
      ( YAxisDistance,
        YAxisDuration,
        YAxisPace
      ),
    eval,
  )
import Pacer.Chart.Data.Run
  ( Run (datetime, distance, duration),
    RunTimestamp,
    SomeRun (MkSomeRun),
    SomeRuns (MkSomeRuns),
  )
import Pacer.Chart.Data.Run qualified as Run
import Pacer.Data.Distance (Distance (unDistance))
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Pacer.Data.Duration (Duration (unDuration))
import Pacer.Derive qualified as Derive
import Pacer.Prelude

-- | Holds all data associated to a chart.
data Chart = MkChart
  { chartData :: ChartData,
    title :: Text
  }
  deriving stock (Eq, Show)

-- | Sum type for data field.
data ChartData
  = MkChartDataY ChartY
  | MkChartDataY1 ChartY1
  deriving stock (Eq, Show)

instance ToJSON ChartData where
  toJSON (MkChartDataY c) = toJSON c
  toJSON (MkChartDataY1 c) = toJSON c

instance ToJSON Chart where
  toJSON c =
    Asn.object
      [ "title" .= c.title,
        "data" .= c.chartData
      ]

-- | Data for a chart with a single Y axis.
data ChartY = MkChartY
  { -- | X and Y axis data.
    values :: NESeq (Tuple2 RunTimestamp Double),
    -- | Y label.
    yLabel :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON ChartY where
  toJSON c =
    Asn.object
      [ "x" .= x,
        "y"
          .= Asn.object
            [ "values" .= y,
              "label" .= c.yLabel
            ]
      ]
    where
      (x, y) = L.unzip $ toList c.values

-- | Data for a chart with two Y axes.
data ChartY1 = MkChartY1
  { -- | Data for a chart with two y Axes.
    values :: NESeq (Tuple3 RunTimestamp Double Double),
    -- | Label for first Y axis.
    yLabel :: Text,
    -- | Label for second Y axis.
    y1Label :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON ChartY1 where
  toJSON c =
    Asn.object
      [ "x" .= x,
        "y"
          .= Asn.object
            [ "values" .= y,
              "label" .= c.yLabel
            ],
        "y1"
          .= Asn.object
            [ "values" .= y1,
              "label" .= c.y1Label
            ]
      ]
    where
      (x, y, y1) = L.unzip3 $ toList c.values

-- | Accumulator for chart with a single Y axis.
type AccY = NESeq (Tuple2 RunTimestamp Double)

-- | Accumulator for chart with two Y axes.
type AccY1 = NESeq (Tuple3 RunTimestamp Double Double)

-- | Turns a sequence of runs and chart requests into charts.
mkCharts ::
  ( FromInteger a,
    Ord a,
    Semifield a,
    Show a,
    ToReal a
  ) =>
  SomeRuns a ->
  ChartRequests ->
  Seq (Either Text Chart)
mkCharts runs = fmap (mkChart runs) . (.unChartRequests)

-- NOTE: HLint incorrectly thinks some brackets are unnecessary.
-- See NOTE: [Brackets with OverloadedRecordDot].
--
{- HLINT ignore "Redundant bracket" -}

-- | Turns a sequence of runs and a chart request into a chart.
mkChart ::
  forall a.
  ( FromInteger a,
    Ord a,
    Semifield a,
    Show a,
    ToReal a
  ) =>
  -- | List of runs.
  SomeRuns a ->
  -- | Chart request.
  ChartRequest ->
  -- | Chart result. Nothing if no runs passed the request's filter.
  Either Text Chart
mkChart (MkSomeRuns someRuns@((MkSomeRun @distUnit sd _) :<|| _)) request =
  case filteredRuns of
    Empty -> Left request.title
    r :<| rs -> Right $ MkChart (mkChartData (r :<|| rs)) request.title
  where
    filteredRuns = filterRuns someRuns request.filters

    finalDistUnit :: DistanceUnit
    finalDistUnit = case sd of
      SMeter -> Kilometer
      SKilometer -> Kilometer
      SMile -> Mile

    mkChartData :: NESeq (SomeRun a) -> ChartData
    mkChartData runs = case request.yAxis1 of
      Nothing ->
        let vals = withSingI sd $ foldMap1 toAccY runs
            lbl = mkYLabel request.yAxis
         in MkChartDataY (MkChartY vals lbl)
      Just yAxis1 ->
        let vals = withSingI sd $ foldMap1 (toAccY1 yAxis1) runs
            lbl = mkYLabel request.yAxis
            lbl1 = mkYLabel yAxis1
         in MkChartDataY1 (MkChartY1 vals lbl lbl1)

    mkYLabel :: YAxisType -> Text
    mkYLabel = \case
      YAxisDistance -> dstTxt
      YAxisDuration -> "time"
      YAxisPace -> "/" <> dstTxt
      where
        dstTxt = display $ withSingI sd $ fromSingI @_ @distUnit

    toAccY :: SomeRun a -> AccY
    toAccY sr@(MkSomeRun _ r) = NESeq.singleton (r.datetime, toY sr)

    toAccY1 :: YAxisType -> SomeRun a -> AccY1
    toAccY1 yAxisType sr@(MkSomeRun _ r) =
      NESeq.singleton (r.datetime, toY sr, toYHelper yAxisType sr)

    toY :: SomeRun a -> Double
    toY = toYHelper request.yAxis

    toYHelper :: YAxisType -> SomeRun a -> Double
    toYHelper axisType (MkSomeRun s r) = case axisType of
      YAxisDistance ->
        withSingI s $ toℝ $ case finalDistUnit of
          -- NOTE: [Brackets with OverloadedRecordDot]
          Meter -> (Run.convertDistance @Kilometer r).distance.unDistance
          Kilometer -> (Run.convertDistance @Kilometer r).distance.unDistance
          Mile -> (Run.convertDistance @Mile r).distance.unDistance
      YAxisDuration -> toℝ r.duration.unDuration
      YAxisPace ->
        withSingI s $ toℝ $ case finalDistUnit of
          Meter -> runToPace (Run.convertDistance @Kilometer r)
          Kilometer -> runToPace (Run.convertDistance @Kilometer r)
          Mile -> runToPace (Run.convertDistance @Kilometer r)
        where
          runToPace runUnits =
            let pace =
                  Derive.derivePace
                    runUnits.distance
                    ((.unPositive) <$> runUnits.duration)
             in pace.unPace.unDuration

filterRuns :: NESeq (SomeRun a) -> List FilterExpr -> Seq (SomeRun a)
filterRuns rs filters = NESeq.filter filterRun rs
  where
    filterRun :: SomeRun a -> Bool
    filterRun r = all (eval (applyFilter r)) filters

    applyFilter :: SomeRun a -> FilterType -> Bool
    applyFilter (MkSomeRun _ r) (FilterLabel lbl) = lbl `elem` r.labels
