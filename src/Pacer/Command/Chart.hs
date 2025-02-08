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

    -- ** Misc
    getRunsType,
  )
where

import Data.Aeson.Encode.Pretty
  ( Config (confIndent, confTrailingNewline),
    Indent (Spaces),
  )
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Data.ByteString.Lazy qualified as BSL
import Data.Csv.Streaming (Records (Cons, Nil))
import Data.Csv.Streaming qualified as Csv
import Data.Text qualified as T
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
import FileSystem.Path qualified as Path
import FileSystem.UTF8 (decodeUtf8Lenient)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Pacer.Command.Chart.Data.Chart (Chart)
import Pacer.Command.Chart.Data.Chart qualified as Chart
import Pacer.Command.Chart.Data.ChartRequest
  ( ChartRequests (garminSettings),
    GarminSettings (distanceUnit),
  )
import Pacer.Command.Chart.Data.Run
  ( Run (MkRun),
    SomeRun (MkSomeRun),
    SomeRuns,
  )
import Pacer.Command.Chart.Data.Run qualified as Run
import Pacer.Command.Chart.Data.Run.Garmin
  ( GarminAct (GarminActOther, GarminActRun),
    GarminRun (datetime, distance, duration, title),
  )
import Pacer.Command.Chart.Data.Time (Timestamp (TimestampTime))
import Pacer.Command.Chart.Params
  ( ChartParams (chartRequestsPath, cleanInstall, json, runsPath, runsType),
    ChartParamsFinal,
    RunsType (RunsDefault, RunsGarmin),
  )
import Pacer.Data.Distance qualified as Distance
import Pacer.Data.Distance.Units
  ( DistanceUnit (Kilometer, Meter, Mile),
  )
import Pacer.Data.Result (Result (Err, Ok))
import Pacer.Exception
  ( GarminE (GarminDecode, GarminMeters, GarminOther, GarminUnitRequired),
    NpmE (MkNpmE),
    TomlE (MkTomlE),
  )
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import Pacer.Web qualified as Web
import Pacer.Web.Paths qualified as WPaths
import System.IO (FilePath)
import System.IO.Error qualified as Error
import System.OsPath qualified as OsPath
import TOML (DecodeTOML, decode)

-- | Handles chart command.
handle ::
  ( HasCallStack,
    FileReader :> es,
    FileWriter :> es,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    PathWriter :> es,
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
    TypedProcess :> es
  ) =>
  ChartParamsFinal ->
  Eff es ()
createCharts params = addNamespace "createCharts" $ do
  cwdOsPath <- PR.getCurrentDirectory
  cwdPath <- Path.parseAbsDir cwdOsPath

  $(Logger.logInfo) $ "Using chart-requests: " <> (Utils.showtPath params.chartRequestsPath)
  $(Logger.logInfo) $ "Using runs: " <> (Utils.showtPath params.runsPath)

  if params.json
    then do
      -- params.json is active, so stop after json generation
      let jsonPath = cwdPath <</>> jsonName
      createChartsJsonFile
        True
        params.runsType
        params.runsPath
        params.chartRequestsPath
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
      PW.createDirectoryIfMissing True (pathToOsPath webDataDir)
      let jsonPath = webDataDir <</>> jsonName

      createChartsJsonFile
        False
        params.runsType
        params.runsPath
        params.chartRequestsPath
        jsonPath

      let setCurrDir = PW.setCurrentDirectory . pathToOsPath
          restorePrevDir = const (PW.setCurrentDirectory cwdOsPath)
          -- set cwd to webDir, return webDir.
          cwdWeb = setCurrDir webDir $> webDir

      -- 5. Build web. Change current dir to web, use bracket to restore prev
      -- dir after we are done.
      bracket cwdWeb restorePrevDir $ \newCurrDir -> do
        let nodeModulesPath = newCurrDir <</>> nodeModulesDir
            nodeModulesOsPath = pathToOsPath nodeModulesPath

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
          destDirOsPath = pathToOsPath destDir
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
        (pathToOsPath webDistDir)
        cwdOsPath

      $(Logger.logInfo)
        $ mconcat
          [ "Successfully created charts in '",
            packText (decodeLenient buildDest),
            "' directory"
          ]
  where
    copyConfig =
      MkCopyDirConfig
        { overwrite = OverwriteNone,
          targetName = TargetNameLiteral buildDest
        }

    buildDest = [osp|build|]

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
    PathWriter :> es
  ) =>
  -- | Level to log the success message at.
  Bool ->
  Maybe RunsType ->
  Path Abs File ->
  Path Abs File ->
  Path Abs File ->
  Eff es ()
createChartsJsonFile logInfoLvl mRunsType runsPath requestsPath outJson =
  addNamespace "createChartsJsonFile" $ do
    bs <- createChartsJsonBS mRunsType runsPath requestsPath

    let (dir, _) = OsPath.splitFileName outJsonOsPath

    createDirectoryIfMissing True dir

    writeBinaryFile outJsonOsPath (toStrictBS bs)

    let msg = "Wrote json file: " <> Utils.showtOsPath outJsonOsPath
    if logInfoLvl
      then $(Logger.logInfo) msg
      else $(Logger.logDebug) msg
  where
    outJsonOsPath = pathToOsPath outJson

-- | Given file paths to runs and chart requests, returns a lazy
-- json-encoded bytestring of a chart array.
createChartsJsonBS ::
  forall es.
  ( HasCallStack,
    FileReader :> es,
    Logger :> es,
    LoggerNS :> es
  ) =>
  Maybe RunsType ->
  -- | Path to runs file. Defaults to 'defRunsPath'.
  Path Abs File ->
  -- | Path to chart-requests.toml. Defaults to 'defChartRequestsPath'.
  Path Abs File ->
  Eff es LazyByteString
createChartsJsonBS mRunsType runsPath chartRequestsPath =
  AsnPretty.encodePretty' cfg
    <$> createChartSeq mRunsType runsPath chartRequestsPath
  where
    cfg =
      AsnPretty.defConfig
        { confIndent = Spaces 2,
          confTrailingNewline = True
        }

-- | Given file paths to runs and chart requests, generates a sequence of
-- charts.
createChartSeq ::
  forall es.
  ( HasCallStack,
    FileReader :> es,
    Logger :> es,
    LoggerNS :> es
  ) =>
  Maybe RunsType ->
  -- | Path to runs file
  Path Abs File ->
  -- | Path to chart-requests.toml
  Path Abs File ->
  Eff es (Seq Chart)
createChartSeq mRunsType runsPath chartRequestsPath = addNamespace "createChartSeq" $ do
  chartRequests <-
    readDecodeToml @(ChartRequests Double) (pathToOsPath chartRequestsPath)

  runs <- readRuns (chartRequests.garminSettings >>= (.distanceUnit))

  throwLeft (Chart.mkCharts runs chartRequests)
  where
    runsOsPath = pathToOsPath runsPath

    -- DistanceUnit should be set if this is a garmin (csv) file.
    -- If it is a toml file, it is ignored.
    readRuns :: Maybe DistanceUnit -> Eff es (SomeRuns Double)
    readRuns mInputDistUnit =
      getRunsType mRunsType runsOsPath >>= \case
        RunsDefault -> readDecodeToml runsOsPath
        RunsGarmin -> do
          inputDistUnit <- case mInputDistUnit of
            Nothing -> throwM GarminUnitRequired
            Just du -> pure du
          readRunsCsv inputDistUnit runsOsPath

    readRunsCsv :: DistanceUnit -> OsPath -> Eff es (SomeRuns Double)
    readRunsCsv inputDistUnit csvPath = do
      -- 1. Read csv into bytestring.
      bs <- readBinaryFile csvPath

      someRunsList <- case inputDistUnit of
        Meter -> throwM GarminMeters
        Kilometer -> bsToRuns Kilometer bs
        Mile -> bsToRuns Mile bs

      case Run.mkSomeRuns someRunsList of
        Err err -> throwM $ GarminOther err
        Ok someRuns -> pure someRuns
      where
        bsToTxt = decodeUtf8Lenient . toStrictBS

        bsToRuns ::
          forall (d :: DistanceUnit) ->
          (SingI d) =>
          ByteString ->
          Eff es (List (SomeRun Double))
        bsToRuns d bs =
          case Csv.decodeByName @(GarminAct d PDouble) (fromStrictBS bs) of
            Left err -> throwM $ GarminDecode err
            Right (_, rs) -> foldGarmin [] rs

        toSomeRun :: (SingI d) => GarminRun d PDouble -> SomeRun Double
        toSomeRun @d gr =
          MkSomeRun (sing @d)
            $ MkRun
              { datetime = TimestampTime gr.datetime,
                distance = Distance.forceUnit gr.distance,
                duration = gr.duration,
                labels = [],
                title = Just gr.title
              }

        foldGarmin ::
          forall d.
          (SingI d) =>
          List (SomeRun Double) ->
          Records (GarminAct d PDouble) ->
          Eff es (List (SomeRun Double))
        foldGarmin acc = \case
          (Nil Nothing leftover) -> do
            unless (BSL.null leftover)
              $ $(Logger.logWarn)
              $ "Csv bytes leftover: "
              <> (bsToTxt $ leftover)

            pure acc
          (Nil (Just err) leftover) -> do
            $(Logger.logWarn) $ "Csv parse error: " <> packText err

            unless (BSL.null leftover)
              $ $(Logger.logWarn)
              $ "Csv bytes leftover: "
              <> (bsToTxt $ leftover)

            pure acc
          Cons (Left err) rs -> do
            $(Logger.logWarn) $ "Csv parse error: " <> packText err

            foldGarmin acc rs
          Cons (Right r) rs -> case r of
            GarminActOther -> foldGarmin acc rs
            GarminActRun gr -> foldGarmin (toSomeRun @d gr : acc) rs

    readDecodeToml ::
      forall a. (DecodeTOML a, HasCallStack) => OsPath -> Eff es a
    readDecodeToml path = do
      contents <- readFileUtf8ThrowM path
      case decode contents of
        Right t -> pure t
        Left err -> throwM $ MkTomlE path err

-- heuristics to decide if we should decode csv or toml.
getRunsType ::
  (Logger :> es) =>
  -- | Specified runs type.
  Maybe RunsType ->
  -- | Runs path.
  OsPath ->
  Eff es RunsType
getRunsType mRunsType runsOsPath = do
  let guess =
        if
          -- 1. Ends w/ .csv -> read garmin csv
          | runsExt == [osp|.csv|] -> RunsGarmin
          -- 2. Ends w/ .toml -> read custom toml
          | runsExt == [osp|.toml|] -> RunsDefault
          -- 3. Name contains the strings 'garmin' or 'activities', assume csv
          | "garmin" `T.isInfixOf` runsBaseNameTxt -> RunsGarmin
          | "activities" `T.isInfixOf` runsBaseNameTxt -> RunsGarmin
          -- 4. Otherwise default to toml
          | otherwise -> RunsDefault

  case mRunsType of
    Nothing -> pure guess
    Just runsType ->
      if runsType == guess
        then pure runsType
        else do
          let msg =
                mconcat
                  [ "Specified runs type '",
                    display runsType,
                    "', but guessed '",
                    display guess,
                    "'; assuming the former."
                  ]
          $(Logger.logWarn) msg
          pure runsType
  where
    runsBaseName = OsPath.takeBaseName runsOsPath
    runsBaseNameTxt = T.toCaseFold $ packText $ decodeLenient runsBaseName
    runsExt = OsPath.takeExtension runsOsPath

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
