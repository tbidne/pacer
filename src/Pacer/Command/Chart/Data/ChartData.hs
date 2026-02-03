{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Command.Chart.Data.ChartData
  ( -- * Primary
    ChartData (..),
    ChartY (..),
    ChartY1 (..),
    mkChartData,

    -- * Misc
    handleChartType,
    smoothRolling,
    smoothWindow,
    allGroups,
  )
where

import Data.Foldable1 (foldl1')
import Data.List (all)
import Data.List qualified as L
import Data.Sequence (Seq (Empty))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Effectful.Logger.Dynamic (LogLevel (LevelDebug))
import Effectful.Logger.Dynamic qualified as Logger
import Pacer.Command.Chart.Data.Activity
  ( Activity (MkActivity, datetime, distance, duration),
    SomeActivities (MkSomeActivities),
    SomeActivity (MkSomeActivity),
    SomeActivityKey,
  )
import Pacer.Command.Chart.Data.Activity qualified as Activity
import Pacer.Command.Chart.Data.ChartRequest
  ( ChartRequest,
    ChartSmooth (smoothPeriod, smoothType),
    ChartSmoothType (ChartSmoothRolling, ChartSmoothWindow),
    ChartSumPeriod (ChartSumDays, ChartSumMonth, ChartSumWeek, ChartSumYear),
    ChartType (ChartTypeDefault, ChartTypeSum),
    YAxisType
      ( YAxisDistance,
        YAxisDuration,
        YAxisPace
      ),
  )
import Pacer.Command.Chart.Data.Expr (FilterExpr, eval)
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
import Pacer.Command.Chart.Data.Expr.Filter qualified as Filter
import Pacer.Command.Chart.Data.Expr.Ord (FilterOpOrd)
import Pacer.Command.Chart.Data.Expr.Ord qualified as Ord
import Pacer.Command.Chart.Data.Time.Moment (Moment (MomentTimestamp))
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
import Pacer.Command.Chart.Data.Time.Timestamp qualified as TS
import Pacer.Configuration.Env.Types (LogEnv)
import Pacer.Configuration.Logging (LogVerbosity (LogV1))
import Pacer.Data.Distance (Distance, SomeDistance)
import Pacer.Data.Distance qualified as Dist
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
  )
import Pacer.Data.Distance.Units qualified as DistU
import Pacer.Data.Duration (Duration (unDuration))
import Pacer.Data.Pace (PaceDistF, SomePace (MkSomePace))
import Pacer.Exception (CreateChartE (CreateChartFilterEmpty))
import Pacer.Prelude
import Pacer.Utils qualified as U
import Pacer.Utils.Json (JsonValue, ToJSON (toJSON), (.=))
import Pacer.Utils.Json qualified as Json

mkYJson :: DistanceUnit -> List Double -> YAxisType -> Text -> JsonValue
mkYJson dunit yVal yType yId =
  Json.object
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

makeFieldLabelsNoPrefix ''ChartY

instance ToJSON ChartY where
  toJSON c =
    Json.object
      [ "xAxis" .= x,
        "yAxes"
          .= Json.object
            [ "y" .= yAxis
            ]
      ]
    where
      (x, y) = L.unzip $ toList $ c ^. #values

      yAxis = mkYJson (c ^. #yDistUnit) y (c ^. #yType) "y"

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

makeFieldLabelsNoPrefix ''ChartY1

instance ToJSON ChartY1 where
  toJSON c =
    Json.object
      [ "xAxis" .= x,
        "yAxes"
          .= Json.object
            [ "y" .= yAxis,
              "y1" .= y1Axis
            ]
      ]
    where
      (x, y, y1) = L.unzip3 $ toList $ c ^. #values
      yAxis = mkYJson (c ^. #yDistUnit) y (c ^. #yType) "y"
      y1Axis = mkYJson (c ^. #yDistUnit) y1 (c ^. #y1Type) "y1"

-- | Holds chart data.
data ChartData
  = ChartDataY ChartY
  | ChartDataY1 ChartY1
  deriving stock (Eq, Show)

instance ToJSON ChartData where
  toJSON (ChartDataY x) = toJSON x
  toJSON (ChartDataY1 x) = toJSON x

-- | Accumulator for chart with a single Y axis.
type AccY = NESeq (Tuple2 Timestamp Double)

-- | Accumulator for chart with two Y axes.
type AccY1 = NESeq (Tuple3 Timestamp Double Double)

hasPace :: ChartData -> Bool
hasPace (ChartDataY y) = y ^. #yType == YAxisPace
hasPace (ChartDataY1 y) =
  (y ^. #yType == YAxisPace) || (y ^. #y1Type == YAxisPace)

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
    Empty -> pure $ Err $ CreateChartFilterEmpty $ request ^. #title
    r :<| rs -> do
      activities <- handleChartType (request ^. #chartType) (r :<|| rs)
      Ok <$> mkChartDataSets finalDistUnit request activities
  where
    (MkSomeActivities (SetToSeqNE someActivities)) = as
    filteredActivities =
      filterActivities
        someActivities
        (globalFilters ++ request ^. #filters)

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
  Just (ChartTypeSum period mSmooth) ->
    runSmooth mSmooth <$> sumPeriod (getStartDay someActivities) period
  where
    getStartDay :: NESeq (SomeActivity a) -> Day
    getStartDay =
      TS.timestampToDay
        . Activity.someActivityApplyActivity (view #datetime)
        . NESeq.head

    runSmooth ::
      Maybe ChartSmooth ->
      NESeq (SomeActivity a) ->
      NESeq (SomeActivity a)
    runSmooth mSmooth xs = case mSmooth of
      Nothing -> xs
      Just cs -> case cs ^. #smoothType of
        ChartSmoothRolling -> smoothRolling (cs ^. #smoothPeriod) xs
        ChartSmoothWindow -> smoothWindow (cs ^. #smoothPeriod) xs

    -- groupBy on Mapping to period
    sumPeriod :: Day -> ChartSumPeriod -> Eff es (NESeq (SomeActivity a))
    sumPeriod startDay period =
      (fmap . fmap) (sumActivities $ TimestampRound roundFn)
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
          case (logEnv ^. #level, logEnv ^. #verbosity) of
            (Just LevelDebug, LogV1) -> do
              let json = toStrictBS $ Json.encodePretty xs
              jsonTxt <- decodeUtf8ThrowM json
              $(Logger.logDebug) jsonTxt
            _ -> pure ()

          pure xs

    toDatetime :: SomeActivity a -> Timestamp
    toDatetime = view #datetime

mkChartDataSets ::
  forall a es.
  ( Fromℤ a,
    Logger :> es,
    Ord a,
    Semifield a,
    Show a,
    Toℝ a
  ) =>
  DistanceUnit ->
  ChartRequest a ->
  NESeq (SomeActivity a) ->
  Eff es ChartData
mkChartDataSets @a finalDistUnit request activities@(MkSomeActivity sd _ :<|| _) = do
  let chartData = case request ^. #y1Axis of
        Nothing ->
          let vals = withSingI sd $ foldMap1 toAccY activities
              yType = request ^. #yAxis
           in ChartDataY (MkChartY vals yType finalDistUnit)
        Just y1Type ->
          let vals = withSingI sd $ foldMap1 (toAccY1 y1Type) activities
              yType = request ^. #yAxis
           in ChartDataY1 (MkChartY1 vals yType y1Type finalDistUnit)

  -- Log a message if meters and pace were requested.
  when (hasPace chartData && finalDistUnit == Meter) $ do
    let msg =
          mconcat
            [ "Requested meters for chart '",
              request ^. #title,
              "', converting pace to km."
            ]
    $(Logger.logInfo) msg

  pure chartData
  where
    toAccY :: SomeActivity a -> AccY
    toAccY sr@(MkSomeActivity _ r) = NESeq.singleton (r ^. #datetime, toY sr)

    toAccY1 :: YAxisType -> SomeActivity a -> AccY1
    toAccY1 yAxisType sr@(MkSomeActivity _ r) =
      NESeq.singleton (r ^. #datetime, toY sr, toYHelper yAxisType sr)

    toY :: SomeActivity a -> Double
    toY = toYHelper $ request ^. #yAxis

    toYHelper :: YAxisType -> SomeActivity a -> Double
    toYHelper axisType (MkSomeActivity s r) = case axisType of
      YAxisDistance ->
        withSingI s $ case finalDistUnit of
          -- See NOTE: [Chart Units]
          Meter -> toDistRaw (DistU.convertDistance Meter r)
          Kilometer -> toDistRaw (DistU.convertToKilometers r)
          Mile -> toDistRaw (DistU.convertDistance Mile r)
      YAxisDuration -> toDurRaw r
      YAxisPace ->
        withSingI s $ case finalDistUnit of
          -- See NOTE: [Chart Units]
          Meter -> toPaceRaw (DistU.convertToKilometers r)
          Kilometer -> toPaceRaw (DistU.convertToKilometers r)
          Mile -> toPaceRaw (DistU.convertDistance Mile r)

    toPaceRaw :: (PaceDistF d) => Activity d a -> Double
    toPaceRaw = toℝ . view (#unPace % #unDuration) . Activity.derivePace

    toDistRaw :: Activity d a -> Double
    toDistRaw = toℝ . view (#distance % #unDistance)

    toDurRaw :: Activity d a -> Double
    toDurRaw = toℝ . view (#duration % #unDuration)

-- NOTE: [Chart Units]
--
-- In general, the frontend supports all units as the y-axis:
--
--   distance: m, km, mi
--   duration: <time>
--   pace: m, km, mi
--
-- However, we explicitly do not allow pace to be used with meters, so the
-- question of what to do if the user specifies 'meters' in a chart request
-- (charts[i].unit = "m") arises.
--
-- Previously, we converted both YAxisDistance and YAxisPace to km, so no
-- warnings/errors, but only km was allowed. We now decide to let distance
-- respect meters, though pace is still converted. We do this for several
-- reasons:
--
-- - In general, we should respect the user's choice when we can, and meters
--   are perfectly sensible for some distances (e.g. track events).
--
-- - We should allow graphing distance by meters and pace by km, but there is
--   only one distance unit in chart-requests at the moment, so pace cannot
--   then error.
--
-- - If we ever add something like elevation, meters makes sense there as
--   well.
--
-- Hence we log a message for conversions, but otherwise respect the choice.
-- This raises the possibility of specifying multiple units e.g.
-- distance_unit and pace_unit, because as of now we cannot have
-- distance in meters and pace in miles, which is also reasonable. But that
-- is a low priority.

-- | A moving window has the group representative be the median element.
smoothWindow ::
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  PWord8 ->
  NESeq (SomeActivity a) ->
  NESeq (SomeActivity a)
smoothWindow = smooth takeMedian
  where
    takeMedian xs = NESeq.index xs (length xs .%. 2)

-- | Rolling average takes only historical data i.e. the group representative
-- is always the latest.
smoothRolling ::
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  PWord8 ->
  NESeq (SomeActivity a) ->
  NESeq (SomeActivity a)
smoothRolling = smooth NESeq.last

smooth ::
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  (NESeq Timestamp -> Timestamp) ->
  PWord8 ->
  NESeq (SomeActivity a) ->
  NESeq (SomeActivity a)
smooth @a sumTs k = sumChunks . allGroups k
  where
    sumChunks :: NESeq (NESeq (SomeActivity a)) -> NESeq (SomeActivity a)
    sumChunks = fmap sumChunk
      where
        sumChunk :: NESeq (SomeActivity a) -> SomeActivity a
        sumChunk xs =
          divAct len $ sumActivities (TimestampAddAll sumTs) xs
          where
            -- Safe because len > 0
            len =
              unsafePositive
                . fromℤ
                . fromIntegral @Int @Integer
                . length
                $ xs

        divAct :: Positive a -> SomeActivity a -> SomeActivity a
        divAct numActs (MkSomeActivity sd a) =
          MkSomeActivity
            sd
            ( a
                { distance = a ^. #distance .% numActs,
                  duration = a ^. #duration .% numActs
                }
            )

-- | Like 'NESeq.chunksOf', except returns all (overlapping) groups e.g.
--
-- @
--   allGroups 3 [1 .. 5]
--     <=> [ [1,2,3], [2,3,4], [3,4,5] ]
-- @
allGroups :: (HasCallStack, Show a) => PWord8 -> NESeq a -> NESeq (NESeq a)
allGroups @a k = go
  where
    go :: NESeq a -> NESeq (NESeq a)
    go xs = case takeNAndRest k xs of
      (ys, Seq.Empty) -> NESeq.singleton ys
      (ys, z :<| zs) -> ys :<|| NESeq.toSeq (go (z :<|| zs))

takeNAndRest :: (HasCallStack, Show a) => PWord8 -> NESeq a -> Tuple2 (NESeq a) (Seq a)
takeNAndRest k ys@(_ :<|| xs)
  | k' >= length ys = (ys, Seq.Empty)
  | otherwise = (zs, xs)
  where
    k' = toInt $ k ^. #unPositive
    zs = case NESeq.take k' ys of
      Seq.Empty ->
        error
          $ mconcat
            [ "Took empty list for len ",
              show k,
              " and list: ",
              show ys
            ]
      (w :<| ws) -> w :<|| ws

    toInt = fromIntegral @Word8 @Int

data TimestampMod
  = -- | Function to apply to the _first_ timestamp in the group, which is
    -- the group's timestamp.
    TimestampRound (Timestamp -> Timestamp)
  | -- | Function that calculates the group's timestamp based on _all_
    -- timestamps (e.g. taking the median timestamp).
    --
    -- The 'AddAll' should be understood as general monoid add, not numeric
    -- sum.
    TimestampAddAll (NESeq Timestamp -> Timestamp)

sumActivities ::
  ( Fromℤ a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  -- | Timestamp modifier.
  TimestampMod ->
  NESeq (SomeActivity a) ->
  SomeActivity a
sumActivities @a timestampMod acts@(MkSomeActivity sy y :<|| ys) =
  case foldl1' go (init :<|| ys) of
    MkSomeActivity sd summedAct ->
      -- Potentially modify the timestamp.
      MkSomeActivity sd (over' #datetime finalTs summedAct)
  where
    -- Either take the first timestamp or combine all in some fashion,
    -- per tsAddAll.
    finalTs dt = tsAddAll dt (view #datetime <$> acts)

    tsRound :: Timestamp -> Timestamp
    tsAddAll :: Timestamp -> NESeq Timestamp -> Timestamp

    (tsRound, tsAddAll) = case timestampMod of
      -- 1. We are applying a unary function to the first timestamp only,
      --    hence the 'addAll' function merely takes the first argument.
      TimestampRound g -> (g, const)
      -- 2. We are using the 'allAll' function, hence we apply no special
      --    behavior to the first element.
      TimestampAddAll g -> (identity, const g)

    go :: SomeActivity a -> SomeActivity a -> SomeActivity a
    go (MkSomeActivity sacc acc) (MkSomeActivity sr r) =
      MkSomeActivity sacc
        $ MkActivity
          { atype = acc ^. #atype,
            -- Only need to add distance and duration, since everything
            -- else is set in the initial value (init).
            datetime = acc ^. #datetime,
            distance = newDist,
            duration = acc ^. #duration .+. r ^. #duration,
            labels = acc ^. #labels,
            title = acc ^. #title
          }
      where
        newDist =
          withSingI sacc
            $ withSingI sr
            $ Dist.liftDistLeft2
              (.+.)
              (acc ^. #distance)
              (r ^. #distance)

    init =
      MkSomeActivity sy
        $ MkActivity
          { -- REVIEW: Should this be Nothing? Or something else e.g.
            -- first element, "Sum", or maybe logic to see if all
            -- all elements have the same type?
            atype = Nothing,
            datetime = tsRound $ y ^. #datetime,
            -- NOTE: No need to convert the distance to the requested
            -- distance here, as mkChartDataSets will take care of it.
            distance = y ^. #distance,
            duration = y ^. #duration,
            labels = mempty,
            title = Nothing
          }

filterActivities ::
  forall a.
  ( Fromℤ a,
    MetricSpace a,
    Ord a,
    Semifield a,
    Show a
  ) =>
  NESeq (SomeActivityKey a) ->
  List (FilterExpr a) ->
  Seq (SomeActivity a)
filterActivities @a rs filters =
  view #unSomeActivityKey <$> NESeq.filter filterActivity rs
  where
    filterActivity :: SomeActivityKey a -> Bool
    filterActivity r = all (eval (applyFilter r)) filters

    applyFilter :: SomeActivityKey a -> FilterType a -> Bool
    applyFilter srk (FilterLabel lblSet) =
      Filter.applyFilterSet (srk ^. (#unSomeActivityKey % #labels)) lblSet
    applyFilter srk (FilterDate op m) =
      applyDate (srk ^. #unSomeActivityKey) op m
    applyFilter srk (FilterDistance op d) =
      applyDist (srk ^. #unSomeActivityKey) op d
    applyFilter srk (FilterDuration op d) =
      applyDur (srk ^. #unSomeActivityKey) op d
    applyFilter srk (FilterPace op p) =
      applyPace (srk ^. #unSomeActivityKey) op p
    applyFilter srk (FilterType fe) = case srk ^. #unSomeActivityKey of
      MkSomeActivity _ r -> case r ^. #atype of
        Just atype -> Filter.applyFilterElem atype fe
        Nothing -> False

    applyDate :: SomeActivity a -> FilterOpOrd -> Moment -> Bool
    applyDate (MkSomeActivity _ r) op = Ord.toIFun op activityMoment
      where
        activityMoment = MomentTimestamp $ r ^. #datetime

    applyDist :: SomeActivity a -> FilterOpOrd -> SomeDistance a -> Bool
    applyDist (MkSomeActivity @activityDist sr r) op fDist =
      withSingI sr $ Ord.toFun op (r ^. #distance) fDist'
      where
        fDist' :: Distance activityDist a
        fDist' = withSingI sr $ DistU.convertDistance activityDist fDist

    applyDur :: SomeActivity a -> FilterOpOrd -> Duration a -> Bool
    applyDur (MkSomeActivity _ r) op = Ord.toFun op (r ^. #duration)

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
