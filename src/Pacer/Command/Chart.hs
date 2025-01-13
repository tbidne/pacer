{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Chart functionality.
module Pacer.Command.Chart
  ( -- * Top level command
    handle,

    -- * Low level functions
    createCharts,
    createChartsJsonFile,
    createChartsJsonBS,
  )
where

import Data.Aeson.Encode.Pretty
  ( Config (confIndent, confTrailingNewline),
    Indent (Spaces),
  )
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic
  ( CopyDirConfig (MkCopyDirConfig),
    Overwrite (OverwriteNone),
    TargetName (TargetNameLiteral),
  )
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import Effectful.Process.Typed.Dynamic qualified as TP
import FileSystem.OsPath (decodeLenient, decodeThrowM)
import FileSystem.Path qualified as Path
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Pacer.Command.Chart.Data.Chart (Chart)
import Pacer.Command.Chart.Data.Chart qualified as Chart
import Pacer.Command.Chart.Data.ChartRequest (ChartRequests)
import Pacer.Command.Chart.Data.Run (SomeRuns)
import Pacer.Command.Chart.Params
  ( ChartParams (chartRequestsPath, cleanInstall, json, runsPath),
    ChartParamsFinal,
  )
import Pacer.Exception (NpmE (MkNpmE), TomlE (MkTomlE))
import Pacer.Log qualified as Log
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
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es,
    TypedProcess :> es
  ) =>
  ChartParamsFinal ->
  Eff es ()
handle = createCharts

createCharts ::
  ( HasCallStack,
    FileReader :> es,
    FileWriter :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es,
    TypedProcess :> es
  ) =>
  ChartParamsFinal ->
  Eff es ()
createCharts params = do
  cwdOsPath <- PR.getCurrentDirectory
  cwdPath <- Path.parseAbsDir cwdOsPath

  Log.debug $ "Using chart-requests: " <> (packText $ Utils.showPath params.chartRequestsPath)
  Log.debug $ "Using runs: " <> (packText $ Utils.showPath params.runsPath)

  if params.json
    then do
      -- params.json is active, so stop after json generation
      let jsonPath = cwdPath <</>> jsonName
      createChartsJsonFile params.runsPath params.chartRequestsPath jsonPath
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

      createChartsJsonFile params.runsPath params.chartRequestsPath jsonPath

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

          Log.debug "Installing node dependencies"
          runNpm npmPathStr ["install", "--save"]

        -- 5.2 Build the html/js
        Log.debug "Building charts"
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

      Log.info
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
    PathWriter :> es,
    Terminal :> es
  ) =>
  Path Abs File ->
  Path Abs File ->
  Path Abs File ->
  Eff es ()
createChartsJsonFile runsPath requestsPath outJson = do
  bs <- createChartsJsonBS runsPath requestsPath

  let (dir, _) = OsPath.splitFileName outJsonOsPath

  createDirectoryIfMissing True dir

  writeBinaryFile outJsonOsPath (toStrictByteString bs)

  Log.debug $ "Wrote json file: " <> packText (decodeLenient outJsonOsPath)
  where
    outJsonOsPath = pathToOsPath outJson

-- | Given file paths to runs and chart requests, returns a lazy
-- json-encoded bytestring of a chart array.
createChartsJsonBS ::
  forall es.
  ( HasCallStack,
    FileReader :> es
  ) =>
  -- | Path to runs.toml. Defaults to 'defRunsPath'.
  Path Abs File ->
  -- | Path to chart-requests.toml. Defaults to 'defChartRequestsPath'.
  Path Abs File ->
  Eff es LazyByteString
createChartsJsonBS runsPath chartRequestsPath =
  AsnPretty.encodePretty' cfg <$> createChartSeq runsPath chartRequestsPath
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
    FileReader :> es
  ) =>
  -- | Path to runs.toml
  Path Abs File ->
  -- | Path to chart-requests.toml
  Path Abs File ->
  Eff es (Seq Chart)
createChartSeq runsPath chartRequestsPath = do
  runs <- readDecodeToml @(SomeRuns Double) (pathToOsPath runsPath)
  chartRequests <-
    readDecodeToml @(ChartRequests Double) (pathToOsPath chartRequestsPath)

  throwLeft (Chart.mkCharts runs chartRequests)
  where
    readDecodeToml :: forall a. (DecodeTOML a, HasCallStack) => OsPath -> Eff es a
    readDecodeToml path = do
      contents <- readFileUtf8ThrowM path
      case decode contents of
        Right t -> pure t
        Left err -> throwM $ MkTomlE path err

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
