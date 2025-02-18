module Pacer.Command.Chart.Data.ChartData
  ( ChartData (..),
    ChartY (..),
    ChartY1 (..),
    mkChartData,
    mkChartDatas,
  )
where

import Data.Aeson (Value)
import Data.Aeson qualified as Asn
import Data.Foldable1 (foldl1')
import Data.List (all)
import Data.List qualified as L
import Data.Sequence (Seq (Empty))
import Data.Sequence.NonEmpty qualified as NESeq
import Pacer.Command.Chart.Data.ChartRequest
  ( ChartRequest (chartType, filters, title, y1Axis, yAxis),
    ChartRequests (chartRequests),
    ChartSumPeriod (ChartSumMonth, ChartSumWeek, ChartSumYear),
    ChartType (ChartTypeDefault, ChartTypeSum),
    YAxisType
      ( YAxisDistance,
        YAxisDuration,
        YAxisPace
      ),
  )
import Pacer.Command.Chart.Data.Expr
  ( FilterExpr,
    FilterOp
      ( FilterOpEq,
        FilterOpGt,
        FilterOpGte,
        FilterOpLt,
        FilterOpLte,
        FilterOpNeq
      ),
    FilterType
      ( FilterDate,
        FilterDistance,
        FilterDuration,
        FilterLabel,
        FilterPace
      ),
    eval,
  )
import Pacer.Command.Chart.Data.Run
  ( Run (MkRun, datetime, distance, duration),
    SomeRun (MkSomeRun),
    SomeRuns (MkSomeRuns),
    SomeRunsKey,
  )
import Pacer.Command.Chart.Data.Run qualified as Run
import Pacer.Command.Chart.Data.Time
  ( Moment (MomentTimestamp),
    Timestamp,
    (./=),
    (.<),
    (.<=),
    (.==),
    (.>),
    (.>=),
  )
import Pacer.Command.Chart.Data.Time qualified as Time
import Pacer.Data.Distance (Distance (unDistance), SomeDistance)
import Pacer.Data.Distance qualified as Dist
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration (Duration (unDuration))
import Pacer.Data.Pace (SomePace (MkSomePace))
import Pacer.Exception (CreateChartE (CreateChartFilterEmpty))
import Pacer.Prelude
import Pacer.Utils qualified as U

-- | Holds chart data.
data ChartData
  = ChartDataY ChartY
  | ChartDataY1 ChartY1
  deriving stock (Eq, Show)

instance ToJSON ChartData where
  toJSON (ChartDataY x) = toJSON x
  toJSON (ChartDataY1 x) = toJSON x

-- | Data for a chart with a single Y axis.
data ChartY = MkChartY
  { -- | X and Y axis data.
    values :: NESeq (Tuple2 Timestamp Double),
    -- | Y axis type. This is used for the label on the line itself, __not__
    -- the y-axis (that label is on ChartOptions).
    yType :: YAxisType
  }
  deriving stock (Eq, Show)

instance ToJSON ChartY where
  toJSON c =
    Asn.object
      [ "xAxis" .= x,
        "yAxes" .= [yAxis]
      ]
    where
      (x, y) = L.unzip $ toList c.values

      yAxis = mkYJson y c.yType "y"

-- | Data for a chart with two Y axes.
data ChartY1 = MkChartY1
  { -- | Data for a chart with two y Axes.
    values :: NESeq (Tuple3 Timestamp Double Double),
    -- | Type of first Y axis.
    yType :: YAxisType,
    -- | Type of second Y axis.
    y1Type :: YAxisType
  }
  deriving stock (Eq, Show)

instance ToJSON ChartY1 where
  toJSON c =
    Asn.object
      [ "xAxis" .= x,
        "yAxes" .= [yAxis, y1Axis]
      ]
    where
      (x, y, y1) = L.unzip3 $ toList c.values
      yAxis = mkYJson y c.yType "y"
      y1Axis = mkYJson y1 c.y1Type "y1"

mkYJson :: [Double] -> YAxisType -> Text -> Value
mkYJson yVal yType yId =
  Asn.object
    [ "data" .= yVal,
      "label" .= yType,
      "fill" .= False,
      "pointHoverRadius" .= i 20, -- point size on hover
      "tension" .= i 0,
      "yAxisID" .= yId
    ]

-- | Accumulator for chart with a single Y axis.
type AccY = NESeq (Tuple2 Timestamp Double)

-- | Accumulator for chart with two Y axes.
type AccY1 = NESeq (Tuple3 Timestamp Double Double)

-- | Turns a sequence of runs and chart requests into charts.
mkChartDatas ::
  ( Fromℤ a,
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a,
    Toℝ a
  ) =>
  -- | Final distance unit to use.
  DistanceUnit ->
  SomeRuns a ->
  ChartRequests a ->
  Either CreateChartE (Seq ChartData)
mkChartDatas finalDistUnit runs =
  traverse (mkChartData finalDistUnit runs) . (.chartRequests)

-- NOTE: HLint incorrectly thinks some brackets are unnecessary.
-- See NOTE: [Brackets with OverloadedRecordDot].
--
{- HLINT ignore "Redundant bracket" -}

-- | Turns a sequence of runs and a chart request into a chart.
mkChartData ::
  forall a.
  ( Fromℤ a,
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a,
    Toℝ a
  ) =>
  -- | Final distance unit to use.
  DistanceUnit ->
  -- | List of runs.
  SomeRuns a ->
  -- | Chart request.
  ChartRequest a ->
  -- | ChartData result. Nothing if no runs passed the request's filter.
  Either CreateChartE ChartData
mkChartData finalDistUnit (MkSomeRuns (SetToSeqNE someRuns)) request =
  case finalRuns of
    Empty -> Left $ CreateChartFilterEmpty request.title
    r :<| rs -> Right (mkChartDataSets finalDistUnit request (r :<|| rs))
  where
    filteredRuns = filterRuns someRuns request.filters

    finalRuns = handleChartType request.chartType filteredRuns

handleChartType ::
  (Fromℤ a, Ord a, Semifield a, Show a) =>
  Maybe ChartType -> Seq (SomeRun a) -> Seq (SomeRun a)
handleChartType @a mChartType someRuns = case mChartType of
  Nothing -> someRuns
  Just ChartTypeDefault -> someRuns
  Just (ChartTypeSum period) -> sumPeriod period
  where
    -- groupBy on Mapping to period
    sumPeriod :: ChartSumPeriod -> Seq (SomeRun a)
    sumPeriod period = periodSum
      where
        compPeriod :: Timestamp -> Timestamp -> Bool
        roundFn :: Timestamp -> Timestamp

        (compPeriod, roundFn) = case period of
          ChartSumWeek -> (Time.sameWeek, Time.roundTimestampWeek)
          ChartSumMonth -> (Time.sameMonth, Time.roundTimestampMonth)
          ChartSumYear -> (Time.sameYear, Time.roundTimestampYear)

        periodSum :: Seq (SomeRun a)
        periodSum = fmap sumRuns . groupByPeriod $ someRuns

        sumRuns :: NESeq (SomeRun a) -> SomeRun a
        sumRuns (MkSomeRun sy y :<|| ys) = foldl1' go (init :<|| ys)
          where
            go :: SomeRun a -> SomeRun a -> SomeRun a
            go (MkSomeRun sacc acc) (MkSomeRun sr r) =
              MkSomeRun sacc
                $ MkRun
                  { datetime = acc.datetime,
                    distance = newDist,
                    duration = acc.duration .+. r.duration,
                    labels = acc.labels,
                    title = acc.title
                  }
              where
                newDist =
                  withSingI sacc
                    $ withSingI sr
                    $ Dist.liftDistLeft2
                      (.+.)
                      acc.distance
                      r.distance

            init =
              MkSomeRun sy
                $ MkRun
                  { datetime = roundFn y.datetime,
                    distance = y.distance,
                    duration = y.duration,
                    labels = mempty,
                    title = Nothing
                  }

        groupByPeriod ::
          Seq (SomeRun a) ->
          Seq (NESeq (SomeRun a))
        groupByPeriod =
          U.seqGroupBy (\r1 r2 -> compPeriod (toDatetime r1) (toDatetime r2))

    toDatetime :: SomeRun a -> Timestamp
    toDatetime (MkSomeRun _ rs) = rs.datetime

mkChartDataSets ::
  forall a.
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a,
    Toℝ a
  ) =>
  DistanceUnit ->
  ChartRequest a ->
  NESeq (SomeRun a) ->
  ChartData
mkChartDataSets finalDistUnit request runs@(MkSomeRun sd _ :<|| _) =
  case request.y1Axis of
    Nothing ->
      let vals = withSingI sd $ foldMap1 toAccY runs
          yType = request.yAxis
       in ChartDataY (MkChartY vals yType)
    Just y1Type ->
      let vals = withSingI sd $ foldMap1 (toAccY1 y1Type) runs
          yType = request.yAxis
       in ChartDataY1 (MkChartY1 vals yType y1Type)
  where
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
          -- REVIEW: Should this be (DistU.convertDistance Meter)? Or do we
          -- only allow km/mi in charts?
          Meter -> (DistU.convertToKilometers r).distance.unDistance
          Kilometer -> (DistU.convertToKilometers r).distance.unDistance
          Mile -> (DistU.convertDistance Mile r).distance.unDistance
      YAxisDuration -> toℝ r.duration.unDuration
      YAxisPace ->
        withSingI s $ toℝ $ case finalDistUnit of
          -- REVIEW: Ideally this would error, not convert. But probably we
          -- already throw errors somewhere, and the Meters are only here
          -- because we haven't proven to GHC that it is impossible. It would
          -- be nice to come up with something more robust.
          Meter -> runToPace (DistU.convertToKilometers r)
          Kilometer -> runToPace (DistU.convertToKilometers r)
          Mile -> runToPace (DistU.convertDistance Mile r)
        where
          runToPace runUnits =
            (Run.derivePace runUnits).unPace.unDuration

filterRuns ::
  forall a.
  ( Fromℤ a,
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
    applyFilter srk (FilterLabel lbl) = applyLabel srk.unSomeRunsKey lbl
    applyFilter srk (FilterDate op m) = applyDate srk.unSomeRunsKey op m
    applyFilter srk (FilterDistance op d) = applyDist srk.unSomeRunsKey op d
    applyFilter srk (FilterDuration op d) = applyDur srk.unSomeRunsKey op d
    applyFilter srk (FilterPace op p) = applyPace srk.unSomeRunsKey op p

    applyLabel :: SomeRun a -> Text -> Bool
    applyLabel (MkSomeRun _ r) lbl = lbl `elem` r.labels

    applyDate :: SomeRun a -> FilterOp -> Moment -> Bool
    applyDate (MkSomeRun _ r) op m = (opToMFun op) runMoment m
      where
        runMoment = MomentTimestamp r.datetime

    applyDist :: SomeRun a -> FilterOp -> SomeDistance a -> Bool
    applyDist (MkSomeRun @runDist sr r) op fDist =
      withSingI sr $ (opToFun op) r.distance fDist'
      where
        fDist' :: Distance runDist a
        fDist' = withSingI sr $ DistU.convertDistance runDist fDist

    applyDur :: SomeRun a -> FilterOp -> Duration a -> Bool
    applyDur (MkSomeRun _ r) op = (opToFun op) r.duration

    applyPace :: SomeRun a -> FilterOp -> SomePace a -> Bool
    applyPace someRun@(MkSomeRun _ _) op (MkSomePace sfp fPace) =
      -- 1. convert someRun to runPace
      case Run.deriveSomePace someRun of
        (MkSomePace @runDist srp runPace) ->
          -- 2. convert filterPace to runPace's units
          withSingI srp
            $ withSingI sfp
            $ case DistU.convertDistance runDist fPace of
              fPace' -> (opToFun op) runPace fPace'

    opToFun :: forall b. (Ord b) => FilterOp -> (b -> b -> Bool)
    opToFun FilterOpEq = (==)
    opToFun FilterOpNeq = (/=)
    opToFun FilterOpLte = (<=)
    opToFun FilterOpLt = (<)
    opToFun FilterOpGte = (>=)
    opToFun FilterOpGt = (>)

    opToMFun :: FilterOp -> (Moment -> Moment -> Bool)
    opToMFun FilterOpEq = (.==)
    opToMFun FilterOpNeq = (./=)
    opToMFun FilterOpLte = (.<=)
    opToMFun FilterOpLt = (.<)
    opToMFun FilterOpGte = (.>=)
    opToMFun FilterOpGt = (.>)

i :: Int -> Int
i = id
