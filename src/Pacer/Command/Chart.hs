{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Chart functionality.
module Pacer.Command.Chart
  ( -- * Top level command
    handle,

    -- * Low level functions
    createCharts,
    createChartsJsonFile,
    createChartsJsonBS,
    ChartPaths,
    readChartInputs,

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
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic
  ( CopyDirConfig (MkCopyDirConfig),
    Overwrite (OverwriteNone),
    TargetName (TargetNameLiteral),
  )
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import Effectful.Logger.Dynamic qualified as Logger
import Effectful.Process.Typed.Dynamic qualified as TP
import FileSystem.OsPath (decodeLenient, decodeThrowM)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Pacer.Command.Chart.Data.Activity
  ( Activity,
    Label,
    SomeActivities,
  )
import Pacer.Command.Chart.Data.Activity qualified as Activity
import Pacer.Command.Chart.Data.ActivityLabel (ActivityLabels (MkActivityLabels))
import Pacer.Command.Chart.Data.Chart (Chart)
import Pacer.Command.Chart.Data.Chart qualified as Chart
import Pacer.Command.Chart.Data.ChartRequest (ChartRequests)
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
        json
      ),
    ChartParamsFinal,
  )
import Pacer.Configuration.Env.Types (CachedPaths, LogEnv)
import Pacer.Configuration.Env.Types qualified as Types
import Pacer.Data.Distance.Units (DistanceUnit)
import Pacer.Exception (GarminE (GarminUnitRequired), NpmE (MkNpmE))
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import Pacer.Web qualified as Web
import Pacer.Web.Paths qualified as WPaths
import System.IO (FilePath)
import System.IO.Error qualified as Error
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
    State CachedPaths :> es,
    TypedProcess :> es
  ) =>
  ChartParamsFinal ->
  Eff es ()
handle = createCharts

createCharts ::
  ( HasCallStack,
    FileReader :> es,
    FileWriter :> es,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    PathWriter :> es,
    Reader LogEnv :> es,
    State CachedPaths :> es,
    TypedProcess :> es
  ) =>
  ChartParamsFinal ->
  Eff es ()
createCharts params = addNamespace "createCharts" $ do
  cwdPath <- Types.getCachedCurrentDirectory
  let cwdOsPath = toOsPath cwdPath

  $(Logger.logInfo)
    $ "Using build-dir: "
    <> Utils.showtPath params.buildDir
  $(Logger.logInfo)
    $ "Using chart-requests: "
    <> Utils.showtPath params.chartRequestsPath
  for params.activityPaths $ \r ->
    $(Logger.logInfo) $ "Using activities: " <> Utils.showtPath r

  case params.activityLabelsPath of
    Nothing -> $(Logger.logDebug) "No activity-labels given"
    Just p ->
      $(Logger.logInfo) $ "Using activity-labels: " <> Utils.showtPath p

  if params.json
    then do
      -- params.json is active, so stop after json generation
      let jsonPath = params.buildDir <</>> jsonName
      createChartsJsonFile
        True
        chartPaths
        jsonPath
    else do
      -- 1. Check that node exists
      --
      -- It is very important we use findExecutable to find the exe, rather
      -- than relying on it being on the PATH via 'npm'. Otherwise windows on
      -- CI dies with mysterious errors about not being able to find certain
      -- files.
      npmPath <-
        PR.findExecutable npmName >>= \case
          Just npmPath -> pure npmPath
          Nothing -> do
            throwPathIOError
              npmName
              "createCharts"
              Error.doesNotExistErrorType
              "Required npm executable not found. Please add it to the PATH."

      -- 2. Create web dir
      webDir <- WPaths.getWebPath
      Web.ensureWebDirExists webDir params.cleanInstall

      let webDataDir = webDir <</>> WPaths.dataDir

      -- 3. Create json file at expected location
      PW.createDirectoryIfMissing True (toOsPath webDataDir)
      let jsonPath = webDataDir <</>> jsonName

      createChartsJsonFile
        False
        chartPaths
        jsonPath

      let setCurrDir = PW.setCurrentDirectory . toOsPath
          restorePrevDir = const (PW.setCurrentDirectory cwdOsPath)
          -- set cwd to webDir, return webDir.
          cwdWeb = setCurrDir webDir $> webDir

      -- 5. Build web. Change current dir to web, use bracket to restore prev
      -- dir after we are done.
      bracket cwdWeb restorePrevDir $ \newCurrDir -> do
        let nodeModulesPath = newCurrDir <</>> nodeModulesDir
            nodeModulesOsPath = toOsPath nodeModulesPath

        nodeModulesExists <- PR.doesDirectoryExist nodeModulesOsPath

        let buildNode = not nodeModulesExists || params.cleanInstall

        npmPathStr <- decodeThrowM npmPath

        -- 5.1 Only install if one of the following two is true:
        --
        -- i. node_modules does not exist.
        -- ii. cleanInstall is true.
        when buildNode $ do
          -- if cleanInstall is true, we need to delete it first.
          when (nodeModulesExists && params.cleanInstall)
            $ PW.removePathForcibly nodeModulesOsPath

          $(Logger.logDebug) "Installing node dependencies"
          runNpm npmPathStr ["install", "--save"]

        -- 5.2 Build the html/js
        $(Logger.logDebug) "Building charts"
        runNpm npmPathStr ["run", "start"]

      -- 6. Copy the build products to current directory
      let destDir = cwdPath <</>> [reldir|build|]
          destDirOsPath = toOsPath destDir
          webDistDir = webDir <</>> WPaths.distDir

      -- first remove directory if it already exists
      PW.removeDirectoryRecursiveIfExists_ destDirOsPath

      -- NOTE: We copy the files individually rather than simply move the
      -- directory for an arguably silly reason: on windows CI, the "XDG"
      -- path (build dir) is apparently on a different drive from the working
      -- directory, and move therefore dies with an error (evidently this is
      -- illegal).
      --
      -- There build dir is about 1mb anyway, hence the copy is cheap, so
      -- it seems reasonable.
      PW.copyDirectoryRecursiveConfig
        copyConfig
        (toOsPath webDistDir)
        cwdOsPath

      $(Logger.logInfo)
        $ mconcat
          [ "Successfully created charts in '",
            packText (decodeLenient buildDirOsPath),
            "' directory"
          ]
  where
    chartPaths =
      (params.chartRequestsPath, params.activityLabelsPath, params.activityPaths)
    buildDirOsPath = toOsPath params.buildDir
    copyConfig =
      MkCopyDirConfig
        { overwrite = OverwriteNone,
          targetName = TargetNameLiteral buildDirOsPath
        }

runNpm ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  FilePath ->
  List String ->
  Eff es ()
runNpm npmPathStr args = do
  let npmCmd = TP.proc npmPathStr args
  (ec, stdout, stderr) <- TP.readProcess npmCmd
  case ec of
    ExitSuccess -> pure ()
    ExitFailure i -> throwM $ MkNpmE npmName args i (stdout <> "\n" <> stderr)

-- | Given 'ChartParamsFinal', generates a json-encoded array of charts, and
-- writes the file to the given location.
createChartsJsonFile ::
  forall es.
  ( HasCallStack,
    FileReader :> es,
    FileWriter :> es,
    Logger :> es,
    LoggerNS :> es,
    PathWriter :> es,
    Reader LogEnv :> es
  ) =>
  -- | Level to log the success message at.
  Bool ->
  ChartPaths ->
  Path Abs File ->
  Eff es ()
createChartsJsonFile logInfoLvl chartPaths outJson =
  addNamespace "createChartsJsonFile" $ do
    bs <- createChartsJsonBS chartPaths

    let (dir, _) = OsPath.splitFileName outJsonOsPath

    createDirectoryIfMissing True dir

    writeBinaryFile outJsonOsPath (toStrictBS bs)

    let msg = "Wrote json file: " <> Utils.showtOsPath outJsonOsPath
    if logInfoLvl
      then $(Logger.logInfo) msg
      else $(Logger.logDebug) msg
  where
    outJsonOsPath = toOsPath outJson

-- | Given file paths to activities and chart requests, returns a lazy
-- json-encoded bytestring of a chart array.
createChartsJsonBS ::
  forall es.
  ( HasCallStack,
    FileReader :> es,
    Logger :> es,
    LoggerNS :> es,
    Reader LogEnv :> es
  ) =>
  ChartPaths ->
  Eff es LazyByteString
createChartsJsonBS params = Utils.encodePretty <$> createChartSeq params

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
  Eff es (Seq Chart)
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
    Utils.readDecodeJson
      @(ChartRequests Double)
      chartRequestsPath

  activitiesNE <- for activitiesPaths $ \rp -> do
    let mDistUnit =
          preview (#garminSettings %? #distanceUnit % _Just) chartRequests
    readActivities mDistUnit rp

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
        MkActivityLabels activityLabels <- Utils.readDecodeJson activityLabelsPath
        let (newActivities, unusedTimestamps) =
              updateLabels activityLabels allActivities

        -- Warn if any timestamps in the labels file were not used.
        unless (MP.null unusedTimestamps) $ do
          let msg =
                mconcat
                  [ "The following timestamps with labels from '",
                    Utils.showtPath activityLabelsPath,
                    "' were not found in activities files:",
                    Utils.showMapListNewlines
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
          Utils.showMapListInline (.unLabel) (toList $ NESet.toList labels)
        ]

    -- DistanceUnit should be set if this is a garmin (csv) file.
    -- If it is a custom activities (json) file, it is ignored.
    readActivities ::
      Maybe DistanceUnit ->
      Path Abs File ->
      Eff es (SomeActivities Double)
    readActivities mInputDistUnit activitiesPath =
      Garmin.getActivitiesType activitiesPath >>= \case
        ActivitiesDefault -> Utils.readDecodeJson activitiesPath
        ActivitiesGarmin -> do
          inputDistUnit <- case mInputDistUnit of
            Nothing -> throwM GarminUnitRequired
            Just du -> pure du
          Garmin.readActivitiesCsv inputDistUnit activitiesPath

type ChartPaths =
  Tuple3
    (Path Abs File) -- chart-requests
    (Maybe (Path Abs File)) -- activity-labels
    (NonEmpty (Path Abs File)) -- activities

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

nodeModulesDir :: Path Rel Dir
nodeModulesDir = [reldir|node_modules|]

npmName :: OsPath

#if WINDOWS
npmName = [osp|npm.cmd|]
#else
npmName = [osp|npm|]
#endif
