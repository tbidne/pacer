{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Chart functionality.
module Pacer.Command.Chart
  ( -- * Top level command
    handle,

    -- * Low level functions
    ChartPaths,

    -- ** Misc
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
import Pacer.Command.Chart.Data.ChartRequest (ChartRequests (filters))
import Pacer.Command.Chart.Data.Expr (FilterExpr)
import Pacer.Command.Chart.Data.Garmin qualified as Garmin
import Pacer.Command.Chart.Data.Time.Timestamp (Timestamp)
import Pacer.Command.Chart.Params
  ( ActivitiesType (ActivitiesDefault, ActivitiesGarmin),
    ChartParams
      ( activityLabelsPath,
        activityPaths,
        buildDir,
        chartRequestsPath,
        cleanInstall,
        json,
        port
      ),
    ChartParamsFinal,
  )
import Pacer.Command.Chart.Server (ServerEff)
import Pacer.Command.Chart.Server qualified as Server
import Pacer.Configuration.Env.Types (LogEnv)
import Pacer.Data.Distance.Units (DistanceUnit)
import Pacer.Exception (GarminE (GarminUnitRequired))
import Pacer.Prelude
import Pacer.Utils.Json qualified as Json
import Pacer.Utils.Show qualified as Utils.Show
import Pacer.Web qualified as Web
import Pacer.Web.Paths qualified as WPaths
import System.OsPath qualified as OsPath

-- | Handles chart command.
handle ::
  ( HasCallStack,
    FileReader :> es,
    FileWriter :> es,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    PathWriter :> es,
    Reader LogEnv :> es,
    ServerEff :> es
  ) =>
  ChartParamsFinal ->
  Eff es ()
handle params = do
  charts <- createCharts params

  if params.json
    then do
      $(Logger.logInfo)
        $ "Using build-dir: "
        <> Utils.Show.showtPath params.buildDir

      -- params.json is active, so stop after json generation
      let jsonPath = params.buildDir <</>> jsonName
      createChartsJsonFile charts jsonPath
    else do
      webDir <- WPaths.getWebPath
      Web.ensureWebDirExists webDir params.cleanInstall

      Server.launchServer
        params.port
        (webDir <</>> [reldir|dist|])
        charts

createCharts ::
  ( HasCallStack,
    FileReader :> es,
    Logger :> es,
    LoggerNS :> es,
    Reader LogEnv :> es
  ) =>
  ChartParamsFinal ->
  Eff es Charts
createCharts params = addNamespace "createCharts" $ do
  $(Logger.logInfo)
    $ "Using chart-requests: "
    <> Utils.Show.showtPath params.chartRequestsPath
  for params.activityPaths $ \r ->
    $(Logger.logInfo) $ "Using activities: " <> Utils.Show.showtPath r

  case params.activityLabelsPath of
    Nothing -> $(Logger.logDebug) "No activity-labels given"
    Just p ->
      $(Logger.logInfo) $ "Using activity-labels: " <> Utils.Show.showtPath p

  createChartSeq chartPaths
  where
    chartPaths =
      (params.chartRequestsPath, params.activityLabelsPath, params.activityPaths)

-- | Given 'ChartParamsFinal', generates a json-encoded array of charts, and
-- writes the file to the given location.
createChartsJsonFile ::
  forall es.
  ( HasCallStack,
    FileWriter :> es,
    Logger :> es,
    LoggerNS :> es,
    PathWriter :> es
  ) =>
  Charts ->
  Path Abs File ->
  Eff es ()
createChartsJsonFile charts outJson =
  addNamespace "createChartsJsonFile" $ do
    let bs = Json.encodePretty charts
        (dir, _) = OsPath.splitFileName outJsonOsPath

    createDirectoryIfMissing True dir

    writeBinaryFile outJsonOsPath (toStrictBS bs)

    let msg = "Wrote json file: " <> Utils.Show.showtOsPath outJsonOsPath
    $(Logger.logInfo) msg
  where
    outJsonOsPath = toOsPath outJson

-- | Given file paths to activities and chart requests, generates a sequence of
-- charts.
createChartSeq ::
  forall es.
  ( HasCallStack,
    FileReader :> es,
    Logger :> es,
    LoggerNS :> es,
    Reader LogEnv :> es
  ) =>
  ChartPaths ->
  Eff es Charts
createChartSeq chartPaths = do
  (chartRequests, activitiesWithLabels) <- readChartInputs chartPaths
  throwErr =<< Chart.mkCharts activitiesWithLabels chartRequests

-- | Given file paths to activities and chart requests, reads the inputs.
readChartInputs ::
  forall es.
  ( HasCallStack,
    FileReader :> es,
    Logger :> es,
    LoggerNS :> es
  ) =>
  ChartPaths ->
  Eff es (Tuple2 (ChartRequests Double) (SomeActivities Double))
readChartInputs chartPaths = addNamespace "readChartInputs" $ do
  chartRequests <-
    Json.readDecodeJson
      @(ChartRequests Double)
      chartRequestsPath

  activitiesNE <- for activitiesPaths $ \rp -> do
    let mDistUnit =
          preview (#garminSettings %? #distanceUnit % _Just) chartRequests

        globalFilters = chartRequests.filters
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
          Utils.Show.showMapListInline (.unLabel) (toList $ NESet.toList labels)
        ]

    -- DistanceUnit should be set if this is a garmin (csv) file.
    -- If it is a custom activities (json) file, it is ignored.
    readActivities ::
      Maybe DistanceUnit ->
      List (FilterExpr Double) ->
      Path Abs File ->
      Eff es (SomeActivities Double)
    readActivities mInputDistUnit globalFilters activitiesPath =
      Garmin.getActivitiesType activitiesPath >>= \case
        ActivitiesDefault ->
          Activity.readActivitiesJson globalFilters activitiesPath
        ActivitiesGarmin -> do
          inputDistUnit <- case mInputDistUnit of
            Nothing -> throwM GarminUnitRequired
            Just du -> pure du
          Garmin.readActivitiesCsv inputDistUnit globalFilters activitiesPath

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
    updateActivityLabels r = case MP.lookup r.datetime activityLabels of
      Nothing -> (r, Set.empty)
      Just labels ->
        ( over' #labels (Set.union $ NESet.toSet labels) r,
          Set.singleton r.datetime
        )

jsonName :: Path Rel File
jsonName = [relfile|charts.json|]
