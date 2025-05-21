{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoScopedTypeVariables #-}

module Pacer.Command.Chart.Data.ChartData
  ( ChartData (..),
    ChartY (..),
    ChartY1 (..),
    mkChartData,
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
import Effectful.Logger.Dynamic (LogLevel (LevelDebug))
import Effectful.Logger.Dynamic qualified as Logger
import Pacer.Command.Chart.Data.Activity
  ( Activity (MkActivity, datetime, distance, duration),
    SomeActivities (MkSomeActivities),
    SomeActivitiesKey,
    SomeActivity (MkSomeActivity),
  )
import Pacer.Command.Chart.Data.Activity qualified as Activity
import Pacer.Command.Chart.Data.ChartRequest
  ( ChartRequest (chartType, filters, title, y1Axis, yAxis),
    ChartSumPeriod (ChartSumDays, ChartSumMonth, ChartSumWeek, ChartSumYear),
    ChartType (ChartTypeDefault, ChartTypeSum),
    YAxisType
      ( YAxisDistance,
        YAxisDuration,
        YAxisPace
      ),
  )
import Pacer.Command.Chart.Data.Expr (FilterExpr, eval)
import Pacer.Command.Chart.Data.Expr.Eq qualified as Eq
import Pacer.Command.Chart.Data.Expr.Filter
  ( FilterType
      ( FilterDate,
        FilterDistance,
        FilterDuration,
        FilterLabel,
        FilterPace,
        FilterType
      ),
  )
import Pacer.Command.Chart.Data.Expr.Ord (FilterOpOrd)
import Pacer.Command.Chart.Data.Expr.Ord qualified as Ord
import Pacer.Command.Chart.Data.Expr.Set
import Pacer.Command.Chart.Data.Expr.Set qualified as ESet
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

-- See NOTE: [Numeric Type]

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

-- NOTE: HLint incorrectly thinks some brackets are unnecessary.
-- See NOTE: [Brackets with OverloadedRecordDot].
--
{- HLINT ignore "Redundant bracket" -}

-- | Turns a sequence of activities and a chart request into a chart.
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
  -- | Global filters
  List (FilterExpr a) ->
  -- | List of activities.
  SomeActivities a ->
  -- | Chart request.
  ChartRequest a ->
  -- | ChartData result. Nothing if no activities passed the request's filter.
  Eff es (Result CreateChartE ChartData)
mkChartData finalDistUnit globalFilters as request = do
  case filteredActivities of
    Empty -> pure $ Err $ CreateChartFilterEmpty request.title
    r :<| rs -> do
      activities <- handleChartType request.chartType (r :<|| rs)
      pure $ Ok $ mkChartDataSets finalDistUnit request activities
  where
    (MkSomeActivities (SetToSeqNE someActivities)) = as
    filteredActivities =
      filterActivities
        someActivities
        (globalFilters ++ request.filters)

-- | Transforms the activities based on the chart type.
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
  NESeq (SomeActivity a) ->
  Eff es (NESeq (SomeActivity a))
handleChartType @es @a mChartType someActivities = case mChartType of
  Nothing -> pure someActivities
  Just ChartTypeDefault -> pure someActivities
  Just (ChartTypeSum period) -> sumPeriod (getStartDay someActivities) period
  where
    getStartDay :: NESeq (SomeActivity a) -> Day
    getStartDay =
      TS.timestampToDay
        . Activity.someActivityApplyActivity (.datetime)
        . NESeq.head

    -- groupBy on Mapping to period
    sumPeriod :: Day -> ChartSumPeriod -> Eff es (NESeq (SomeActivity a))
    sumPeriod startDay period =
      (fmap . fmap) sumActivities
        . groupByPeriod
        $ someActivities
      where
        compPeriod :: Timestamp -> Timestamp -> Bool
        roundFn :: Timestamp -> Timestamp

        -- The idea here is:
        --
        -- 1. We group the activities via the compPeriod function i.e. determine
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

        sumActivities :: NESeq (SomeActivity a) -> SomeActivity a
        sumActivities (MkSomeActivity sy y :<|| ys) = foldl1' go (init :<|| ys)
          where
            go :: SomeActivity a -> SomeActivity a -> SomeActivity a
            go (MkSomeActivity sacc acc) (MkSomeActivity sr r) =
              MkSomeActivity sacc
                $ MkActivity
                  { atype = acc.atype,
                    -- Only need to add distance and time, since everything
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
              MkSomeActivity sy
                $ MkActivity
                  { -- REVIEW: Should this be Nothing? Or something else e.g.
                    -- first element, "Sum", or maybe logic to see if all
                    -- all elements have the same type?
                    atype = Nothing,
                    datetime = roundFn y.datetime,
                    -- NOTE: No need to convert the distance to the requested
                    -- distance here, as mkChartDataSets will take care of it.
                    distance = y.distance,
                    duration = y.duration,
                    labels = mempty,
                    title = Nothing
                  }

        groupByPeriod ::
          NESeq (SomeActivity a) ->
          Eff es (NESeq (NESeq (SomeActivity a)))
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

    toDatetime :: SomeActivity a -> Timestamp
    toDatetime (MkSomeActivity _ rs) = rs.datetime

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
  NESeq (SomeActivity a) ->
  ChartData
mkChartDataSets @a finalDistUnit request activities@(MkSomeActivity sd _ :<|| _) =
  case request.y1Axis of
    Nothing ->
      let vals = withSingI sd $ foldMap1 toAccY activities
          yType = request.yAxis
       in ChartDataY (MkChartY vals yType finalDistUnit)
    Just y1Type ->
      let vals = withSingI sd $ foldMap1 (toAccY1 y1Type) activities
          yType = request.yAxis
       in ChartDataY1 (MkChartY1 vals yType y1Type finalDistUnit)
  where
    toAccY :: SomeActivity a -> AccY
    toAccY sr@(MkSomeActivity _ r) = NESeq.singleton (r.datetime, toY sr)

    toAccY1 :: YAxisType -> SomeActivity a -> AccY1
    toAccY1 yAxisType sr@(MkSomeActivity _ r) =
      NESeq.singleton (r.datetime, toY sr, toYHelper yAxisType sr)

    toY :: SomeActivity a -> Double
    toY = toYHelper request.yAxis

    toYHelper :: YAxisType -> SomeActivity a -> Double
    toYHelper axisType (MkSomeActivity s r) = case axisType of
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
          Meter -> activityToPace (DistU.convertToKilometers r)
          Kilometer -> activityToPace (DistU.convertToKilometers r)
          Mile -> activityToPace (DistU.convertDistance Mile r)
        where
          activityToPace activityUnits =
            (Activity.derivePace activityUnits).unPace.unDuration

filterActivities ::
  forall a.
  ( Fromℤ a,
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  NESeq (SomeActivitiesKey a) ->
  List (FilterExpr a) ->
  Seq (SomeActivity a)
filterActivities @a rs filters = (.unSomeActivitiesKey) <$> NESeq.filter filterActivity rs
  where
    filterActivity :: SomeActivitiesKey a -> Bool
    filterActivity r = all (eval (applyFilter r)) filters

    applyFilter :: SomeActivitiesKey a -> FilterType a -> Bool
    applyFilter srk (FilterLabel lblSet) = applyFilterSet srk.labels lblSet
    applyFilter srk (FilterDate op m) = applyDate srk.unSomeActivitiesKey op m
    applyFilter srk (FilterDistance op d) = applyDist srk.unSomeActivitiesKey op d
    applyFilter srk (FilterDuration op d) = applyDur srk.unSomeActivitiesKey op d
    applyFilter srk (FilterPace op p) = applyPace srk.unSomeActivitiesKey op p
    applyFilter srk (FilterType fe) = case srk.unSomeActivitiesKey of
      MkSomeActivity _ r -> case r.atype of
        Just atype -> applyFilterElem atype fe
        Nothing -> False

    applyFilterElem :: (Ord b) => b -> FilterElem p b -> Bool
    applyFilterElem actVal = \case
      FilterElemEq op t -> Eq.toFun op actVal t
      FilterElemExists op set -> ESet.memberFun op actVal set

    applyFilterSet :: forall b p. (Ord b) => Set b -> FilterSet p b -> Bool
    applyFilterSet set = \case
      FilterSetElem (FilterElemEq op t) -> ESet.existsElemFun op set t
      FilterSetElem (FilterElemExists op t) -> ESet.existsSetFun op set t
      FilterSetHasElem op t -> ESet.hasElemFun op set t
      FilterSetComp op t -> ESet.compFun op set t

    applyDate :: SomeActivity a -> FilterOpOrd -> Moment -> Bool
    applyDate (MkSomeActivity _ r) op m = Ord.toIFun op activityMoment m
      where
        activityMoment = MomentTimestamp r.datetime

    applyDist :: SomeActivity a -> FilterOpOrd -> SomeDistance a -> Bool
    applyDist (MkSomeActivity @activityDist sr r) op fDist =
      withSingI sr $ Ord.toFun op r.distance fDist'
      where
        fDist' :: Distance activityDist a
        fDist' = withSingI sr $ DistU.convertDistance activityDist fDist

    applyDur :: SomeActivity a -> FilterOpOrd -> Duration a -> Bool
    applyDur (MkSomeActivity _ r) op = Ord.toFun op r.duration

    applyPace :: SomeActivity a -> FilterOpOrd -> SomePace a -> Bool
    applyPace someActivity@(MkSomeActivity _ _) op (MkSomePace sfp fPace) =
      -- 1. convert someActivity to activityPace
      case Activity.deriveSomePace someActivity of
        (MkSomePace @activityDist srp activityPace) ->
          -- 2. convert filterPace to activityPace's units
          withSingI srp
            $ withSingI sfp
            $ case DistU.convertDistance activityDist fPace of
              fPace' -> Ord.toFun op activityPace fPace'
