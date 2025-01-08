{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Pacer.Chart
  ( -- * Params
    ChartParams (..),

    -- * Functions
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
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter
  ( CopyDirConfig (MkCopyDirConfig),
    Overwrite (OverwriteNone),
    TargetName (TargetNameLiteral),
  )
import Effects.FileSystem.PathWriter qualified as PW
import Effects.Process.Typed qualified as TP
import FileSystem.OsPath (decodeLenient, decodeThrowM)
import FileSystem.Path qualified as Path
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Pacer.Chart.Data.Chart (Chart)
import Pacer.Chart.Data.Chart qualified as Chart
import Pacer.Chart.Data.ChartRequest (ChartRequests)
import Pacer.Chart.Data.Run (SomeRuns)
import Pacer.Exception (NpmE (MkNpmE), TomlE (MkTomlE))
import Pacer.Log qualified as Log
import Pacer.Prelude
import Pacer.Web qualified as Web
import Pacer.Web.Paths qualified as WPaths
import System.IO (FilePath)
import System.IO.Error qualified as Error
import System.OsPath qualified as OsPath
import TOML (DecodeTOML, decode)

-- | Chart command params.
data ChartParams = MkChartParams
  { -- | If true, copies clean install of web dir and installs node deps.
    cleanInstall :: Bool,
    -- | Optional path to directory with runs.toml and chart-requests.toml.
    dataDir :: Maybe OsPath,
    -- | If true, stops the build after generating the intermediate json
    -- file.
    json :: Bool,
    -- | Optional path to chart-requests.toml.
    mChartRequestsPath :: Maybe OsPath,
    -- | Optional path to runs.toml.
    mRunsPath :: Maybe OsPath
  }
  deriving stock (Eq, Show)

createCharts ::
  ( HasCallStack,
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadTypedProcess m
  ) =>
  ChartParams ->
  m ()
createCharts @m params = do
  (chartRequestsPath, runsPath) <- getChartInputs

  cwdOsPath <- PR.getCurrentDirectory
  cwdPath <- Path.parseAbsDir cwdOsPath

  if params.json
    then do
      -- params.json is active, so stop after json generation
      let jsonPath = cwdPath <</>> jsonName
      createChartsJsonFile runsPath chartRequestsPath jsonPath
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

      createChartsJsonFile runsPath chartRequestsPath jsonPath

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

    -- Retrieves (Tuple2 chart-requests-path runs-path)
    getChartInputs :: m (Path Abs File, Path Abs File)
    getChartInputs = do
      (mDataDir1, chartRequestsPath) <-
        getChartInput chartRequestsName Nothing params.mChartRequestsPath

      (_, runsPath) <-
        getChartInput runsName mDataDir1 params.mRunsPath

      pure (chartRequestsPath, runsPath)

    -- For @getChartInput fileName mDataDir mInputOsPath@, we use its direct
    -- mInputOsPath if it exists. Otherwise we retrive the path based on the
    -- data directory and fileName.
    --
    -- Since we intend to call this function for each needed file, we take
    -- the data directory as an input so that we only find it once.
    getChartInput ::
      -- Expected file_name e.g. runs.toml
      Path Rel File ->
      -- Maybe data_directory.
      Maybe (Path Abs Dir) ->
      -- Maybe file.
      Maybe OsPath ->
      -- Tuple2 (Maybe data_directory) file
      m (Tuple2 (Maybe (Path Abs Dir)) (Path Abs File))
    getChartInput fileName mDataDir mInputOsPath = do
      case mInputOsPath of
        -- 1. Input path was explicitly given, use it.
        Just inputOsPath -> do
          inputPath <- parseCanonicalAbsFile inputOsPath
          pure (mDataDir, inputPath)
        -- 2. No input direct path, need to derive it from the data dir.
        Nothing -> do
          dataDir <- case mDataDir of
            -- 2.1. Data dir already found, use it.
            Just dataDir -> pure dataDir
            Nothing -> case params.dataDir of
              -- 2.1.1. Data dir given on the params, use it.
              Just p -> parseCanonicalAbsDir p
              -- 2.1.1. Data dir not given, use XDG.
              Nothing -> getXdgConfigPath

          let filePath = dataDir <</>> fileName
          assertExists filePath
          pure (Just dataDir, filePath)

    assertExists p = do
      let p' = pathToOsPath p
      exists <- PR.doesFileExist p'
      unless exists $ do
        throwPathIOError
          p'
          "createCharts"
          Error.doesNotExistErrorType
          "Required file does not exist."

runNpm ::
  ( HasCallStack,
    MonadThrow m,
    MonadTypedProcess m
  ) =>
  FilePath ->
  List String ->
  m ()
runNpm npmPathStr args = do
  let npmCmd = TP.proc npmPathStr args
  (ec, stdout, stderr) <- TP.readProcess npmCmd
  case ec of
    ExitSuccess -> pure ()
    ExitFailure i -> throwM $ MkNpmE npmName args i (stdout <> "\n" <> stderr)

-- | Given 'ChartParamsFinal', generates a json-encoded array of charts, and
-- writes the file to the given location.
createChartsJsonFile ::
  forall m.
  ( HasCallStack,
    MonadFileReader m,
    MonadFileWriter m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Path Abs File ->
  Path Abs File ->
  Path Abs File ->
  m ()
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
  forall m.
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  -- | Path to runs.toml. Defaults to 'defRunsPath'.
  Path Abs File ->
  -- | Path to chart-requests.toml. Defaults to 'defChartRequestsPath'.
  Path Abs File ->
  m LazyByteString
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
  forall m.
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  -- | Path to runs.toml
  Path Abs File ->
  -- | Path to chart-requests.toml
  Path Abs File ->
  m (Seq Chart)
createChartSeq runsPath chartRequestsPath = do
  runs <- readDecodeToml @(SomeRuns Double) (pathToOsPath runsPath)
  chartRequests <-
    readDecodeToml @(ChartRequests Double) (pathToOsPath chartRequestsPath)

  throwLeft (Chart.mkCharts runs chartRequests)
  where
    readDecodeToml :: forall a. (DecodeTOML a, HasCallStack) => OsPath -> m a
    readDecodeToml path = do
      contents <- readFileUtf8ThrowM path
      case decode contents of
        Right t -> pure t
        Left err -> throwM $ MkTomlE path err

runsName :: Path Rel File
runsName = [relfile|runs.toml|]

chartRequestsName :: Path Rel File
chartRequestsName = [relfile|chart-requests.toml|]

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
