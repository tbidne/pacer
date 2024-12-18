module Pacer.Chart.Data.Chart
  ( Chart (..),
    ChartData (..),
    mkChart,
    mkCharts,
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import Data.Aeson qualified as Asn
import Data.List (all)
import Data.List qualified as L
import Data.Sequence (Seq (Empty))
import Data.Sequence.NonEmpty qualified as NESeq
import Pacer.Chart.Data.ChartRequest
  ( ChartRequest (filters, title, y1Axis, yAxis),
    ChartRequests (unChartRequests),
    FilterExpr,
    FilterOp (FilterEq, FilterGt, FilterGte, FilterLt, FilterLte),
    FilterType (FilterDistance, FilterDuration, FilterLabel, FilterPace),
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
    SomeRunsKey (MkSomeRunsKey, unSomeRunsKey),
  )
import Pacer.Chart.Data.Run qualified as Run
import Pacer.Data.Distance
  ( Distance (unDistance),
    SomeDistance (MkSomeDistance),
  )
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
    SDistanceUnit (SKilometer, SMeter, SMile),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration (Duration (unDuration))
import Pacer.Data.Pace (SomePace (MkSomePace))
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
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a,
    ToReal a
  ) =>
  SomeRuns a ->
  ChartRequests a ->
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
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a,
    ToReal a
  ) =>
  -- | List of runs.
  SomeRuns a ->
  -- | Chart request.
  ChartRequest a ->
  -- | Chart result. Nothing if no runs passed the request's filter.
  Either Text Chart
mkChart
  ( MkSomeRuns
      (SetToSeqNE someRuns@(MkSomeRunsKey (MkSomeRun @distUnit sd _) :<|| _))
    )
  request =
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
      mkChartData runs = case request.y1Axis of
        Nothing ->
          let vals = withSingI sd $ foldMap1 toAccY runs
              lbl = mkYLabel request.yAxis
           in MkChartDataY (MkChartY vals lbl)
        Just y1Axis ->
          let vals = withSingI sd $ foldMap1 (toAccY1 y1Axis) runs
              lbl = mkYLabel request.yAxis
              lbl1 = mkYLabel y1Axis
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
            Meter -> (DistU.convertDistance_ @_ @Kilometer r).distance.unDistance
            Kilometer -> (DistU.convertDistance_ @_ @Kilometer r).distance.unDistance
            Mile -> (DistU.convertDistance_ @_ @Mile r).distance.unDistance
        YAxisDuration -> toℝ r.duration.unDuration
        YAxisPace ->
          withSingI s $ toℝ $ case finalDistUnit of
            Meter -> runToPace (DistU.convertDistance_ @_ @Kilometer r)
            Kilometer -> runToPace (DistU.convertDistance_ @_ @Kilometer r)
            -- TODO: Previously this was converting to Kilometers, but that
            -- was almost certainly a bug that tests did not catch.
            -- Let's write one.
            Mile -> runToPace (DistU.convertDistance_ @_ @Mile r)
          where
            runToPace runUnits =
              (Run.derivePace runUnits).unPace.unDuration

filterRuns ::
  forall a.
  ( FromInteger a,
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  NESeq (SomeRunsKey a) ->
  List (FilterExpr a) ->
  Seq (SomeRun a)
filterRuns rs filters = (.unSomeRunsKey) <$> NESeq.filter filterRun rs
  where
    filterRun :: SomeRunsKey a -> Bool
    filterRun r = all (eval (applyFilter r)) filters

    applyFilter :: SomeRunsKey a -> FilterType a -> Bool
    applyFilter (MkSomeRunsKey (MkSomeRun _ r)) (FilterLabel lbl) = lbl `elem` r.labels
    applyFilter (MkSomeRunsKey (MkSomeRun @runDist sr r)) (FilterDistance op (MkSomeDistance sd d)) =
      let d' = withSingI sr $ withSingI sd $ DistU.convertDistance_ @_ @runDist d
       in withSingI sr $ (opToFun op) r.distance d'
    applyFilter (MkSomeRunsKey (MkSomeRun _ r)) (FilterDuration op d) = (opToFun op) r.duration d
    applyFilter (MkSomeRunsKey someRun) (FilterPace op (MkSomePace sfp filterPace)) =
      -- 1. convert someRun to runPace
      case Run.deriveSomePace someRun of
        (MkSomePace @runDist srp runPace) ->
          -- 2. convert filterPace to runPace's units
          withSingI srp
            $ withSingI sfp
            $ case DistU.convertDistance_ @_ @runDist filterPace of
              p' -> (opToFun op) runPace ((.unPositive) <$> p')

    opToFun FilterLt = (<)
    opToFun FilterLte = (>=)
    opToFun FilterEq = (==)
    opToFun FilterGt = (>)
    opToFun FilterGte = (>=)
