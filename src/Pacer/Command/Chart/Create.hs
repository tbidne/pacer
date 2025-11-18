{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Chart functionality.
module Pacer.Command.Chart.Create
  ( -- * High-level
    createCharts,

    -- * Low-level
    ChartPaths,
    createChartSeq,
    updateLabels,
  )
where

import Data.Foldable1 (Foldable1 (foldlMap1'))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as MP
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Effectful.Logger.Dynamic qualified as Logger
import Pacer.Command.Chart.Data.Activity (Activity, SomeActivities)
import Pacer.Command.Chart.Data.Activity qualified as Activity
import Pacer.Command.Chart.Data.Activity.ActivityLabel
  ( ActivityLabels (MkActivityLabels),
    Label (unLabel),
  )
import Pacer.Command.Chart.Data.Chart (Charts)
import Pacer.Command.Chart.Data.Chart qualified as Chart
import Pacer.Command.Chart.Data.ChartRequest (ChartRequests)
import Pacer.Command.Chart.Data.Expr (FilterExpr)
import Pacer.Command.Chart.Data.Garmin qualified as Garmin
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
import Pacer.Command.Chart.Params
  ( ActivitiesType (ActivitiesDefault, ActivitiesGarmin),
    ChartParamsFinal,
  )
import Pacer.Configuration.Config (ChartConfig)
import Pacer.Configuration.Env.Types (LogEnv)
import Pacer.Data.Distance.Units (DistanceUnit)
import Pacer.Exception (GarminE (GarminUnitRequired))
import Pacer.Prelude
import Pacer.Utils.Json qualified as Json
import Pacer.Utils.Show qualified as Utils.Show

-- | Creates charts from the config.
createCharts ::
  forall env k es.
  ( HasCallStack,
    FileReader :> es,
    LoggerNS env k es,
    Reader LogEnv :> es
  ) =>
  Maybe ChartConfig ->
  ChartParamsFinal ->
  Eff es Charts
createCharts @env mChartConfig params = addNamespace @env "createCharts" $ do
  $(Logger.logInfo)
    $ "Using chart-requests: "
    <> Utils.Show.showtPath (params ^. #chartRequestsPath)
  for_ (params ^. #activityPaths) $ \r ->
    $(Logger.logInfo) $ "Using activities: " <> Utils.Show.showtPath r

  case params ^. #activityLabelsPath of
    Nothing -> $(Logger.logDebug) "No activity-labels given"
    Just p ->
      $(Logger.logInfo) $ "Using activity-labels: " <> Utils.Show.showtPath p

  createChartSeq @env mChartConfig chartPaths
  where
    chartPaths =
      ( params ^. #chartRequestsPath,
        params ^. #activityLabelsPath,
        params ^. #activityPaths
      )

-- | Given file paths to activities and chart requests, generates a sequence of
-- charts.
createChartSeq ::
  forall env k es.
  ( HasCallStack,
    FileReader :> es,
    LoggerNS env k es,
    Reader LogEnv :> es
  ) =>
  Maybe ChartConfig ->
  ChartPaths ->
  Eff es Charts
createChartSeq @env mChartConfig chartPaths = do
  (chartRequests, activitiesWithLabels) <- readChartInputs @env chartPaths
  throwErr =<< Chart.mkCharts mChartConfig activitiesWithLabels chartRequests

-- | Given file paths to activities and chart requests, reads the inputs.
readChartInputs ::
  forall env k es.
  ( HasCallStack,
    FileReader :> es,
    LoggerNS env k es
  ) =>
  ChartPaths ->
  Eff es (Tuple2 (ChartRequests Double) (SomeActivities Double))
readChartInputs @env @_ @es chartPaths = addNamespace @env "readChartInputs" $ do
  chartRequests <-
    Json.readDecodeJson
      @(ChartRequests Double)
      chartRequestsPath

  activitiesNE <- for activitiesPaths $ \rp -> do
    let mDistUnit =
          preview (#garminSettings %? #distanceUnit % _Just) chartRequests

        globalFilters = chartRequests ^. #filters
    readActivities mDistUnit globalFilters rp

  let combineActivities acc rs = acc >>= Activity.unionSomeActivities rs

  allActivities <-
    case foldlMap1' Ok combineActivities activitiesNE of
      Err err -> throwM err
      Ok xs -> pure xs

  -- If a labels file exists, use it to update the activities.
  activitiesWithLabels <-
    case mActivityLabelsPath of
      Nothing -> pure allActivities
      Just activityLabelsPath -> do
        MkActivityLabels activityLabels <- Json.readDecodeJson activityLabelsPath
        let (newActivities, unusedTimestamps) =
              updateLabels activityLabels allActivities

        -- Warn if any timestamps in the labels file were not used.
        unless (MP.null unusedTimestamps) $ do
          let msg =
                mconcat
                  [ "The following timestamps with labels from '",
                    Utils.Show.showtPath activityLabelsPath,
                    "' were not found in activities files:",
                    Utils.Show.showMapListNewlines
                      displayUnused
                      (MP.toList unusedTimestamps)
                  ]
          $(Logger.logWarn) msg

        pure newActivities

  pure (chartRequests, activitiesWithLabels)
  where
    (chartRequestsPath, mActivityLabelsPath, activitiesPaths) = chartPaths

    displayUnused (ts, labels) =
      mconcat
        [ display ts,
          ": ",
          Utils.Show.showMapListInline (view #unLabel) (toList $ NESet.toList labels)
        ]

    -- DistanceUnit should be set if this is a garmin (csv) file.
    -- If it is a custom activities (json) file, it is ignored.
    readActivities ::
      Maybe DistanceUnit ->
      List (FilterExpr Double) ->
      Path Abs File ->
      Eff es (SomeActivities Double)
    readActivities mInputDistUnit globalFilters activitiesPath =
      Garmin.getActivitiesType @es activitiesPath >>= \case
        ActivitiesDefault ->
          Activity.readActivitiesJson @env globalFilters activitiesPath
        ActivitiesGarmin -> do
          inputDistUnit <- case mInputDistUnit of
            Nothing -> throwM GarminUnitRequired
            Just du -> pure du
          Garmin.readActivitiesCsv @env inputDistUnit globalFilters activitiesPath

type ChartPaths =
  Tuple3
    (Path Abs File) -- chart-requests
    (Maybe (Path Abs File)) -- activity-labels
    (NESeq (Path Abs File)) -- activities

-- | Updates all activities with labels from the given map @m@. Returns a (possibly
-- empty) subset of @m@ that was 'unmatched' i.e. had no corresponding
-- activity in SomeActivities.
updateLabels ::
  -- | Timestamp -> labels map.
  Map Timestamp (NESet Label) ->
  -- | All activities.
  SomeActivities a ->
  -- | (New activities, unmatching timestamps).
  Tuple2 (SomeActivities a) (Map Timestamp (NESet Label))
updateLabels activityLabels rs = (newActivities, unmatched)
  where
    (newActivities, matched) =
      Activity.mapAccumSomeActivities updateActivityLabels rs

    unmatched = MP.withoutKeys activityLabels matched

    updateActivityLabels ::
      forall d a.
      Activity d a ->
      Tuple2 (Activity d a) (Set Timestamp)
    updateActivityLabels r = case MP.lookup (r ^. #datetime) activityLabels of
      Nothing -> (r, Set.empty)
      Just labels ->
        ( over' #labels (Set.union $ NESet.toSet labels) r,
          Set.singleton (r ^. #datetime)
        )
