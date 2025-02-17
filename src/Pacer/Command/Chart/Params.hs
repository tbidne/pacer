{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Chart parameters.
module Pacer.Command.Chart.Params
  ( -- * Types
    ChartParams (..),
    ChartParamsArgs,
    ChartParamsFinal,
    RunsType (..),

    -- * Functions
    evolvePhase,

    -- * Type families
    BuildDirF,
    MPathF,
    PathF,
  )
where

import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.Path qualified as Path
import GHC.TypeError qualified as TE
import GHC.TypeLits (ErrorMessage (ShowType, (:<>:)), TypeError)
import Pacer.Config.Env.Types (CachedPaths, getCachedXdgConfigPath)
import Pacer.Config.Env.Types qualified as Types
import Pacer.Config.Phase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseFinal),
  )
import Pacer.Config.Toml
  ( ChartConfig (buildDir, dataDir),
    Toml (chartConfig),
    TomlWithPath (dirPath, toml),
  )
import Pacer.Exception
  ( ChartFileMissingE
      ( MkChartFileMissingE,
        cliDataDir,
        expectedFiles,
        tomlDataDir,
        xdgDir
      ),
    FileNotFoundE (MkFileNotFoundE),
  )
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import System.OsPath qualified as OsPath

-- | The type of runs file.
data RunsType
  = -- | Default runs type, runs.toml.
    RunsDefault
  | -- | Garmin runs type, activities.csv.
    RunsGarmin
  deriving stock (Eq, Show)

instance Display RunsType where
  displayBuilder = \case
    RunsDefault -> "default (toml)"
    RunsGarmin -> "garmin (csv)"

-- See NOTE: [User Path]

type PathF :: ConfigPhase -> Type -> Type
type family PathF p t where
  PathF ConfigPhaseArgs _ = Maybe OsPath
  PathF ConfigPhaseFinal File = Path Abs File
  PathF ConfigPhaseFinal Dir = ()
  PathF ConfigPhaseFinal t =
    TypeError
      ( TE.Text "Type '"
          :<>: ShowType t
          :<>: TE.Text "' invalid for PathF."
      )

type MPathF :: ConfigPhase -> Type -> Type
type family MPathF p t where
  MPathF ConfigPhaseArgs _ = Maybe OsPath
  MPathF ConfigPhaseFinal File = Maybe (Path Abs File)
  MPathF ConfigPhaseFinal Dir = ()
  MPathF ConfigPhaseFinal t =
    TypeError
      ( TE.Text "Type '"
          :<>: ShowType t
          :<>: TE.Text "' invalid for MPathF."
      )

type BuildDirF :: ConfigPhase -> Type
type family BuildDirF p where
  BuildDirF ConfigPhaseArgs = Maybe OsPath
  BuildDirF ConfigPhaseFinal = Path Abs Dir

-- | Chart params.
type ChartParams :: ConfigPhase -> Type
data ChartParams p = MkChartParams
  { -- | Build directory.
    buildDir :: BuildDirF p,
    -- | If true, copies clean install of web dir and installs node deps.
    cleanInstall :: Bool,
    -- | Optional path to chart-requests.toml.
    chartRequestsPath :: PathF p File,
    -- | Optional path to directory with runs file and chart-requests.toml.
    dataDir :: PathF p Dir,
    -- | If true, stops the build after generating the intermediate json
    -- file.
    json :: Bool,
    -- | Optional path to run labels file.
    runLabelsPath :: MPathF p File,
    -- | Optional path to runs file.
    runsPath :: PathF p File,
    -- | Optional specification for which runs type file we are using.
    runsType :: Maybe RunsType
  }

type ChartParamsArgs = ChartParams ConfigPhaseArgs

type ChartParamsFinal = ChartParams ConfigPhaseFinal

deriving stock instance
  ( Eq (BuildDirF p),
    Eq (MPathF p File),
    Eq (PathF p Dir),
    Eq (PathF p File)
  ) =>
  Eq (ChartParams p)

deriving stock instance
  ( Show (BuildDirF p),
    Show (MPathF p File),
    Show (PathF p Dir),
    Show (PathF p File)
  ) =>
  Show (ChartParams p)

-- | Evolve chart params' phase.
evolvePhase ::
  ( HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  ChartParamsArgs ->
  Maybe TomlWithPath ->
  Eff es ChartParamsFinal
evolvePhase @es params mTomlWithPath = do
  (chartRequestsPath, runLabelsPath, runsPath) <-
    getChartInputs params mTomlWithPath

  assertExists chartRequestsPath
  assertExists runsPath
  for_ runLabelsPath assertExists

  buildDir <-
    case params.buildDir of
      Just d -> do
        -- --build-dir exists, parse absolute or resolve relative to cwd
        cwdPath <- Types.getCachedCurrentDirectory
        customBuildDir cwdPath d
      Nothing -> case mTomlWithPath of
        Just t ->
          case t.toml.chartConfig >>= (.buildDir) of
            -- toml.build-dir exists, parse absolute or resolve relative to
            -- toml path.
            Just d -> customBuildDir t.dirPath d
            -- otherwise use default, cwd/build
            Nothing -> defaultBuildDir
        Nothing -> defaultBuildDir

  pure
    $ MkChartParams
      { buildDir,
        cleanInstall = params.cleanInstall,
        chartRequestsPath,
        dataDir = (),
        json = params.json,
        runLabelsPath,
        runsPath,
        runsType = params.runsType
      }
  where
    defaultBuildDir :: Eff es (Path Abs Dir)
    defaultBuildDir = do
      cwdPath <- Types.getCachedCurrentDirectory
      pure $ cwdPath <</>> [reldir|build|]

    customBuildDir :: Path Abs Dir -> OsPath -> Eff es (Path Abs Dir)
    customBuildDir absDir unknownDir =
      handleUnknownPath
        absDir
        Path.parseAbsDir
        Path.parseRelDir
        unknownDir

-- Retrieves (Tuple2 chart-requests-path runs-path)
getChartInputs ::
  forall es.
  ( HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  ChartParamsArgs ->
  Maybe TomlWithPath ->
  Eff es (Tuple3 (Path Abs File) (Maybe (Path Abs File)) (Path Abs File))
getChartInputs params mTomlWithPath = do
  chartRequestsPath <-
    resolveRequiredChartInput
      params
      mTomlWithPath
      "chart-requests"
      [chartRequestsName]
      params.chartRequestsPath
      (#chartConfig %? #chartRequestsPath % _Just)

  -- If --runs-type is given, use it to influence the order of files
  -- we try to find. Note that our current implementation does NOT mean
  -- that the specified type will always override the other. It just
  -- determines which one we use when we find both __in the same
  -- location__. In particular, suppose we have the following conditions:
  --
  --   1. --runs-type garmin
  --   2. -d some-dir
  --   3. some-dir/ has runs.toml __only__
  --   4. xdg/ has runs.toml and Activities.garmin
  --
  -- Then we will still use runs.toml because it is in the higher priority
  -- location (-d over xdg). OTOH, if we left out -d, we'd use,
  -- Activities.garmin.
  let runsFileNames = case params.runsType of
        Nothing -> runsTomlName : runsGarmins
        Just RunsDefault -> runsTomlName : runsGarmins
        Just RunsGarmin -> runsGarmins ++ [runsTomlName]

  runsPath <-
    resolveRequiredChartInput
      params
      mTomlWithPath
      "runs"
      runsFileNames
      params.runsPath
      (#chartConfig %? #runsPath % _Just)

  runLabelsPath <-
    resolveChartInput
      params
      mTomlWithPath
      "run-labels"
      [runLabelsName]
      params.runLabelsPath
      (#chartConfig %? #runLabelsPath % _Just)

  pure (chartRequestsPath, runLabelsPath, runsPath)

-- | Like 'resolveChartInput', except throws an exception if that path
-- is not determined. Similarly, does not check actual file existence.
resolveRequiredChartInput ::
  forall es.
  ( HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  ChartParamsArgs ->
  Maybe TomlWithPath ->
  -- Text description
  Text ->
  -- Expected file_name(s) e.g. runs file
  List (Path Rel File) ->
  -- Maybe file.
  Maybe OsPath ->
  -- Toml selector.
  AffineTraversal' Toml OsPath ->
  -- Returned path.
  Eff es (Path Abs File)
resolveRequiredChartInput params mTomlWithPath desc fileNames mInputOsPath =
  resolveChartInput' >=> throwIfMissing'
  where
    resolveChartInput' =
      resolveChartInput params mTomlWithPath desc fileNames mInputOsPath
    throwIfMissing' = throwIfMissing params mTomlWithPath fileNames

-- | Uses CLI, (possible) Toml, and XDG to resolve a chart input path. Note
-- that this does _NOT_ guarantee that the path actually exists. This merely
-- reduces the entire configuration into a single path to be used. For
-- instance, if the user supplies a path on the CLI config, resolveChartInput
-- will return that path (without checking existence) as the CLI has the
-- highest priority.
--
-- Returns 'Nothing' if no explicit paths were given and we do not find any
-- "expected" paths as a fallback.
resolveChartInput ::
  forall es.
  ( HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  ChartParamsArgs ->
  Maybe TomlWithPath ->
  -- Text description
  Text ->
  -- Expected file_name(s) e.g. runs file
  List (Path Rel File) ->
  -- Maybe file.
  Maybe OsPath ->
  -- Toml selector.
  AffineTraversal' Toml OsPath ->
  -- Path, if we were given one or found an expected one.
  Eff es (Maybe (Path Abs File))
resolveChartInput params mTomlWithPath desc fileNames mInputOsPath tomlSel =
  addNamespace "getMChartInput" $ addNamespace desc $ do
    -- 1. Try Cli first.
    findCliPath params.dataDir fileNames mInputOsPath >>= \case
      Just p -> pure $ Just p
      Nothing -> do
        $(Logger.logDebug) "No cli path(s) found, checking toml"
        -- 2. Try Toml next.
        findTomlPath fileNames tomlSel mTomlWithPath >>= \case
          Just p -> pure $ Just p
          -- 3. Finally, fall back to xdg.
          Nothing -> do
            $(Logger.logDebug) "No toml path(s) found, falling back to xdg"
            (findXdgPath fileNames)
  where

findCliPath ::
  ( HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es
  ) =>
  -- | Maybe data dir.
  Maybe OsPath ->
  -- | File names.
  List (Path Rel File) ->
  -- Maybe file.
  Maybe OsPath ->
  Eff es (Maybe (Path Abs File))
findCliPath dataDir fileNames =
  addNamespace "findCliPath" <<< \case
    -- 1.1 Cli path exists, use it.
    Just inputPath -> Just <$> parseCanonicalAbsFile inputPath
    Nothing -> do
      -- 1.2 Try Cli.data-dir/path
      join
        <$> for
          dataDir
          (parseCanonicalAbsDir >=> findFirstMatch fileNames)

findTomlPath ::
  ( HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es
  ) =>
  -- | File names.
  List (Path Rel File) ->
  -- | Toml selector.
  AffineTraversal' Toml OsPath ->
  -- | Maybe toml.
  Maybe TomlWithPath ->
  Eff es (Maybe (Path Abs File))
findTomlPath fileNames tomlSel = \case
  Nothing -> pure Nothing
  Just tomlWithPath -> addNamespace "findTomlPath"
    $ case preview (#toml % tomlSel) tomlWithPath of
      -- 2.1. Toml.path exists, use it
      Just p ->
        Just
          <$> handleUnknownPath
            tomlWithPath.dirPath
            Path.parseAbsFile
            Path.parseRelFile
            p
      Nothing -> case preview tomlDataSel tomlWithPath of
        Just dataDir -> do
          -- 2.2. Toml.data-dir/path exists, try it
          tomlDataDirPath <-
            handleUnknownPath
              tomlWithPath.dirPath
              Path.parseAbsDir
              Path.parseRelDir
              dataDir
          findFirstMatch fileNames tomlDataDirPath
        Nothing -> pure Nothing
  where
    tomlDataSel :: AffineTraversal' TomlWithPath OsPath
    tomlDataSel = #toml % #chartConfig %? #dataDir % _Just

findXdgPath ::
  ( HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  -- | File names.
  List (Path Rel File) ->
  Eff es (Maybe (Path Abs File))
findXdgPath fileNames = addNamespace "findXdgPath" $ do
  -- 3. Fallback to xdg
  xdgDir <- getCachedXdgConfigPath
  findFirstMatch fileNames xdgDir

findFirstMatch ::
  ( HasCallStack,
    Logger :> es,
    PathReader :> es
  ) =>
  -- | File names.
  List (Path Rel File) ->
  -- | Data dir to search.
  Path Abs Dir ->
  Eff es (Maybe (Path Abs File))
findFirstMatch fileNames dataDir = do
  $(Logger.logDebug) msg
  go fileNames
  where
    go [] = pure $ Nothing
    go (f : fs) = do
      let path = dataDir <</>> f
      exists <- PR.doesFileExist (pathToOsPath path)
      if exists
        then pure $ Just path
        else go fs

    msg =
      mconcat
        [ "Searching for path(s) ",
          Utils.showListF Utils.showtPath fileNames,
          " in: ",
          Utils.showtPath dataDir
        ]

runsTomlName :: Path Rel File
runsTomlName = [relfile|runs.toml|]

runsGarmins :: List (Path Rel File)
runsGarmins = [runsGarminName, runsGarminNameLower]

runsGarminName :: Path Rel File
runsGarminName = [relfile|Activities.csv|]

runsGarminNameLower :: Path Rel File
runsGarminNameLower = [relfile|activities.csv|]

chartRequestsName :: Path Rel File
chartRequestsName = [relfile|chart-requests.toml|]

runLabelsName :: Path Rel File
runLabelsName = [relfile|run-labels.toml|]

-- Handles an unknown (wrt. absolute/relative) path, resolving any
-- relative paths. Polymorphic over file/dir path.
handleUnknownPath ::
  (HasCallStack) =>
  -- Containing dir for relative paths.
  Path Abs Dir ->
  -- Absolute parser.
  ((HasCallStack) => OsPath -> Eff es (Path Abs t)) ->
  -- Relative parser.
  ((HasCallStack) => OsPath -> Eff es (Path Rel t)) ->
  -- Path to resolve
  OsPath ->
  Eff es (Path Abs t)
handleUnknownPath parentDir absParser relParser otherPath =
  if OsPath.isAbsolute otherPath
    -- Path is absolute, just parse it.
    then absParser otherPath
    -- Path is relative, parse and combine with parentDir.
    else do
      relFile <- relParser otherPath
      pure $ parentDir <</>> relFile

assertExists :: (HasCallStack, PathReader :> es) => Path b t -> Eff es ()
assertExists p = do
  let p' = pathToOsPath p
  exists <- PR.doesFileExist p'
  unless exists $ throwM (MkFileNotFoundE p')

throwIfMissing ::
  ( HasCallStack,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  ChartParamsArgs ->
  Maybe TomlWithPath ->
  List (Path Rel File) ->
  Maybe a ->
  Eff es a
throwIfMissing params mTomlWithPath fileNames = \case
  Just path -> pure path
  Nothing -> do
    xdgDir <- getCachedXdgConfigPath
    throwM
      $ MkChartFileMissingE
        { cliDataDir = params.dataDir,
          expectedFiles = fileNames,
          tomlDataDir =
            mTomlWithPath >>= \t -> t.toml.chartConfig >>= (.dataDir),
          xdgDir
        }

makeFieldLabelsNoPrefix ''ChartParams
