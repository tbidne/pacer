{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoScopedTypeVariables #-}

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
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful.Logger.Dynamic (LogLevel (LevelDebug))
import Effectful.Logger.Dynamic qualified as Logger
import Pacer.Class.IOrd (IEq ((~~)), (/~), (<.), (<~), (>.), (>~))
import Pacer.Command.Chart.Data.ChartRequest
  ( ChartRequest (chartType, filters, title, y1Axis, yAxis),
    ChartRequests (chartRequests),
    ChartSumPeriod (ChartSumDays, ChartSumMonth, ChartSumWeek, ChartSumYear),
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
        FilterOpNEq
      ),
    FilterType
      ( FilterDate,
        FilterDistance,
        FilterDuration,
        FilterLabel,
        FilterLabels,
        FilterPace
      ),
    eval,
  )
import Pacer.Command.Chart.Data.Expr.Labels
  ( FilterLabelOp
      ( FilterLabelOpEq,
        FilterLabelOpNeq
      ),
    FilterLabelSet
      ( FilterLabelSetMany,
        FilterLabelSetOne
      ),
    FilterLabelSetOpMany
      ( FilterLabelSetOpManyEq,
        FilterLabelSetOpManyNEq,
        FilterLabelSetOpManyPSub,
        FilterLabelSetOpManyPSuper,
        FilterLabelSetOpManySub,
        FilterLabelSetOpManySuper
      ),
    FilterLabelSetOpOne
      ( FilterLabelSetOpOneMember,
        FilterLabelSetOpOneNMember
      ),
  )
import Pacer.Command.Chart.Data.Run
  ( Run (MkRun, datetime, distance, duration),
    SomeRun (MkSomeRun),
    SomeRuns (MkSomeRuns),
    SomeRunsKey,
  )
import Pacer.Command.Chart.Data.Run qualified as Run
import Pacer.Command.Chart.Data.Time.Moment (Moment (MomentTimestamp))
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
import Pacer.Command.Chart.Data.Time.Timestamp qualified as TS
import Pacer.Configuration.Env.Types (LogEnv (logLevel, logVerbosity))
import Pacer.Configuration.Logging (LogVerbosity (LogV1))
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
    yType :: YAxisType,
    -- | Distance unit for y-axis, if it used.
    yDistUnit :: DistanceUnit
  }
  deriving stock (Eq, Show)

instance ToJSON ChartY where
  toJSON c =
    Asn.object
      [ "xAxis" .= x,
        "yAxes"
          .= Asn.object
            [ "y" .= yAxis
            ]
      ]
    where
      (x, y) = L.unzip $ toList c.values

      yAxis = mkYJson c.yDistUnit y c.yType "y"

-- | Data for a chart with two Y axes.
data ChartY1 = MkChartY1
  { -- | Data for a chart with two y Axes.
    values :: NESeq (Tuple3 Timestamp Double Double),
    -- | Type of first Y axis.
    yType :: YAxisType,
    -- | Type of second Y axis.
    y1Type :: YAxisType,
    -- | Distance unit for y-axis, if it used.
    yDistUnit :: DistanceUnit
  }
  deriving stock (Eq, Show)

instance ToJSON ChartY1 where
  toJSON c =
    Asn.object
      [ "xAxis" .= x,
        "yAxes"
          .= Asn.object
            [ "y" .= yAxis,
              "y1" .= y1Axis
            ]
      ]
    where
      (x, y, y1) = L.unzip3 $ toList c.values
      yAxis = mkYJson c.yDistUnit y c.yType "y"
      y1Axis = mkYJson c.yDistUnit y1 c.y1Type "y1"

mkYJson :: DistanceUnit -> [Double] -> YAxisType -> Text -> Value
mkYJson dunit yVal yType yId =
  Asn.object
    [ "data" .= yVal,
      "label" .= mkYLabel yType, -- m, km, mi
      "type" .= yType, -- distance, duration, pace
      "id" .= yId -- y, y1
    ]
  where
    mkYLabel :: YAxisType -> Text
    mkYLabel = \case
      YAxisDistance -> dstTxt
      YAxisDuration -> "time"
      YAxisPace -> "pace /" <> dstTxt
      where
        dstTxt = display dunit

-- | Accumulator for chart with a single Y axis.
type AccY = NESeq (Tuple2 Timestamp Double)

-- | Accumulator for chart with two Y axes.
type AccY1 = NESeq (Tuple3 Timestamp Double Double)

-- | Turns a sequence of runs and chart requests into charts.
mkChartDatas ::
  ( Display a,
    Fromℤ a,
    Logger :> es,
    MetricSpace a,
    Ord a,
    Reader LogEnv :> es,
    Semifield a,
    Show a,
    Toℚ a
  ) =>
  -- | Final distance unit to use.
  DistanceUnit ->
  SomeRuns a ->
  ChartRequests a ->
  Eff es (Result CreateChartE (Seq ChartData))
mkChartDatas finalDistUnit runs =
  fmap sequenceA
    . traverse (mkChartData finalDistUnit runs)
    . (.chartRequests)

-- NOTE: HLint incorrectly thinks some brackets are unnecessary.
-- See NOTE: [Brackets with OverloadedRecordDot].
--
{- HLINT ignore "Redundant bracket" -}

-- | Turns a sequence of runs and a chart request into a chart.
mkChartData ::
  forall es a.
  ( Display a,
    Fromℤ a,
    MetricSpace a,
    Logger :> es,
    Ord a,
    Reader LogEnv :> es,
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
  Eff es (Result CreateChartE ChartData)
mkChartData finalDistUnit (MkSomeRuns (SetToSeqNE someRuns)) request = do
  case filteredRuns of
    Empty -> pure $ Err $ CreateChartFilterEmpty request.title
    r :<| rs -> do
      runs <- handleChartType request.chartType (r :<|| rs)
      pure $ Ok $ mkChartDataSets finalDistUnit request runs
  where
    filteredRuns = filterRuns someRuns request.filters

-- | Transforms the runs based on the chart type.
handleChartType ::
  forall es a.
  ( Display a,
    Fromℤ a,
    Logger :> es,
    Ord a,
    Reader LogEnv :> es,
    Semifield a,
    Show a,
    Toℝ a
  ) =>
  Maybe ChartType ->
  NESeq (SomeRun a) ->
  Eff es (NESeq (SomeRun a))
handleChartType @es @a mChartType someRuns = case mChartType of
  Nothing -> pure someRuns
  Just ChartTypeDefault -> pure someRuns
  Just (ChartTypeSum period) -> sumPeriod (getStartDay someRuns) period
  where
    getStartDay :: NESeq (SomeRun a) -> Day
    getStartDay =
      TS.timestampToDay
        . Run.someRunApplyRun (.datetime)
        . NESeq.head

    -- groupBy on Mapping to period
    sumPeriod :: Day -> ChartSumPeriod -> Eff es (NESeq (SomeRun a))
    sumPeriod startDay period =
      (fmap . fmap) sumRuns
        . groupByPeriod
        $ someRuns
      where
        compPeriod :: Timestamp -> Timestamp -> Bool
        roundFn :: Timestamp -> Timestamp

        -- The idea here is:
        --
        -- 1. We group the runs via the compPeriod function i.e. determine
        --    if they belong in the same time interval.
        --
        -- 2. Round the first element in the group down to the interval
        --    start (e.g. calendary week, arbitrary start day), then sum
        --    up everything in that group as one event.
        (compPeriod, roundFn) = case period of
          ChartSumWeek -> (TS.sameWeek, TS.roundTimestampWeek)
          ChartSumMonth -> (TS.sameMonth, TS.roundTimestampMonth)
          ChartSumYear -> (TS.sameYear, TS.roundTimestampYear)
          ChartSumDays days ->
            (TS.sameInterval startDay days, TS.roundInterval startDay days)

        sumRuns :: NESeq (SomeRun a) -> SomeRun a
        sumRuns (MkSomeRun sy y :<|| ys) = foldl1' go (init :<|| ys)
          where
            go :: SomeRun a -> SomeRun a -> SomeRun a
            go (MkSomeRun sacc acc) (MkSomeRun sr r) =
              MkSomeRun sacc
                $ MkRun
                  { -- Only need to add distance and time, since everything
                    -- else is set in the initial value (init).
                    datetime = acc.datetime,
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
                    -- NOTE: No need to convert the distance to the requested
                    -- distance here, as mkChartDataSets will take care of it.
                    distance = y.distance,
                    duration = y.duration,
                    labels = mempty,
                    title = Nothing
                  }

        groupByPeriod ::
          NESeq (SomeRun a) ->
          Eff es (NESeq (NESeq (SomeRun a)))
        groupByPeriod ys = do
          let xs =
                U.neSeqGroupBy
                  (\r1 r2 -> compPeriod (toDatetime r1) (toDatetime r2))
                  ys

          -- We should only do this when the level is debug and verbosity,
          -- since the json output can be very large. Furthermore, we
          -- want to guard all of this logic first since we do not want to
          -- unnecessarily decode the json to text, unless we are actually
          -- going to use it.
          logEnv <- ask @LogEnv
          case (logEnv.logLevel, logEnv.logVerbosity) of
            (Just LevelDebug, LogV1) -> do
              let json = toStrictBS $ U.encodePretty xs
              jsonTxt <- decodeUtf8ThrowM json
              $(Logger.logDebug) jsonTxt
            _ -> pure ()

          pure xs

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
mkChartDataSets @a finalDistUnit request runs@(MkSomeRun sd _ :<|| _) =
  case request.y1Axis of
    Nothing ->
      let vals = withSingI sd $ foldMap1 toAccY runs
          yType = request.yAxis
       in ChartDataY (MkChartY vals yType finalDistUnit)
    Just y1Type ->
      let vals = withSingI sd $ foldMap1 (toAccY1 y1Type) runs
          yType = request.yAxis
       in ChartDataY1 (MkChartY1 vals yType y1Type finalDistUnit)
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
filterRuns @a rs filters = (.unSomeRunsKey) <$> NESeq.filter filterRun rs
  where
    filterRun :: SomeRunsKey a -> Bool
    filterRun r = all (eval (applyFilter r)) filters

    applyFilter :: SomeRunsKey a -> FilterType a -> Bool
    applyFilter srk (FilterLabel op lbl) = applyLabel srk.unSomeRunsKey op lbl
    applyFilter srk (FilterLabels lblSet) = applyLabels srk.unSomeRunsKey lblSet
    applyFilter srk (FilterDate op m) = applyDate srk.unSomeRunsKey op m
    applyFilter srk (FilterDistance op d) = applyDist srk.unSomeRunsKey op d
    applyFilter srk (FilterDuration op d) = applyDur srk.unSomeRunsKey op d
    applyFilter srk (FilterPace op p) = applyPace srk.unSomeRunsKey op p

    applyLabel :: SomeRun a -> FilterLabelOp -> Text -> Bool
    applyLabel (MkSomeRun _ r) op lbl = (labelOpToFun op) r.labels lbl

    applyLabels :: SomeRun a -> FilterLabelSet -> Bool
    applyLabels (MkSomeRun _ r) = \case
      FilterLabelSetOne op t -> (labelSetOneOpToFun op) r.labels t
      FilterLabelSetMany op set -> (labelSetManyOpToFun op) r.labels set

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
    opToFun FilterOpNEq = (/=)
    opToFun FilterOpLte = (<=)
    opToFun FilterOpLt = (<)
    opToFun FilterOpGte = (>=)
    opToFun FilterOpGt = (>)

    opToMFun :: FilterOp -> (Moment -> Moment -> Bool)
    opToMFun FilterOpEq = (~~)
    opToMFun FilterOpNEq = (/~)
    opToMFun FilterOpLte = (<~)
    opToMFun FilterOpLt = (<.)
    opToMFun FilterOpGte = (>~)
    opToMFun FilterOpGt = (>.)

    labelOpToFun :: forall b. (Ord b) => FilterLabelOp -> (Set b -> b -> Bool)
    labelOpToFun FilterLabelOpEq = flip Set.member
    labelOpToFun FilterLabelOpNeq = flip Set.notMember

    labelSetOneOpToFun :: forall b. (Ord b) => FilterLabelSetOpOne -> (Set b -> b -> Bool)
    labelSetOneOpToFun FilterLabelSetOpOneMember = flip Set.member
    labelSetOneOpToFun FilterLabelSetOpOneNMember = flip Set.notMember

    labelSetManyOpToFun :: forall b. (Ord b) => FilterLabelSetOpMany -> (Set b -> Set b -> Bool)
    labelSetManyOpToFun FilterLabelSetOpManyEq = (==)
    labelSetManyOpToFun FilterLabelSetOpManyNEq = (/=)
    -- Superset rather than subset because that's how the syntax is defined,
    -- since the activity's labels is on the LHS. E.g. to filter on some
    -- labels being a subset of an activity's labels we write:
    --
    --   labels ⊇ {a, b, c}
    --
    -- We respect this order when applying all of labels i.e.
    --
    --    <op> labels {a, b, c}
    --
    -- Hence we need superset, not subset.
    labelSetManyOpToFun FilterLabelSetOpManySuper = isSuperSetOf
    labelSetManyOpToFun FilterLabelSetOpManyPSuper = isProperSuperSetOf
    labelSetManyOpToFun FilterLabelSetOpManySub = Set.isSubsetOf
    labelSetManyOpToFun FilterLabelSetOpManyPSub = Set.isProperSubsetOf

isSuperSetOf :: forall a. (Ord a) => Set a -> Set a -> Bool
isSuperSetOf = flip Set.isSubsetOf

isProperSuperSetOf :: forall a. (Ord a) => Set a -> Set a -> Bool
isProperSuperSetOf = flip Set.isProperSubsetOf
