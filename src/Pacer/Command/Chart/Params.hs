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
    RunPathsF,
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.List.NonEmpty qualified as NE
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.Path qualified as Path
import GHC.TypeError qualified as TE
import GHC.TypeLits (ErrorMessage (ShowType, (:<>:)), TypeError)
import Pacer.Class.FromAlt
  ( FromAlt (Alt1, toAlt1),
    isNonEmpty,
  )
import Pacer.Configuration.Config
  ( ChartConfig (dataDir),
    Config (chartConfig),
    ConfigWithPath (config, dirPath),
  )
import Pacer.Configuration.Env.Types (CachedPaths, getCachedXdgConfigPath)
import Pacer.Configuration.Env.Types qualified as Types
import Pacer.Configuration.Phase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseFinal),
  )
import Pacer.Exception
  ( ChartFileMissingE
      ( MkChartFileMissingE,
        cliDataDir,
        configDataDir,
        expectedFiles,
        xdgDir
      ),
    FileNotFoundE (MkFileNotFoundE),
  )
import Pacer.Prelude
import Pacer.Utils (FileAliases (MkFileAliases), SearchFiles (MkSearchFiles))
import Pacer.Utils qualified as Utils
import System.OsPath qualified as OsPath

-- | The type of runs file.
data RunsType
  = -- | Default runs type, runs.json.
    RunsDefault
  | -- | Garmin runs type, activities.csv.
    RunsGarmin
  deriving stock (Eq, Show)

instance Display RunsType where
  displayBuilder = \case
    RunsDefault -> "default (json)"
    RunsGarmin -> "garmin (csv)"

type BuildDirF :: ConfigPhase -> Type
type family BuildDirF p where
  BuildDirF ConfigPhaseArgs = Maybe OsPath
  BuildDirF ConfigPhaseFinal = Path Abs Dir

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

type RunPathsF :: ConfigPhase -> Type
type family RunPathsF p where
  RunPathsF ConfigPhaseArgs = List OsPath
  RunPathsF ConfigPhaseFinal = NonEmpty (Path Abs File)

-- | Chart params.
type ChartParams :: ConfigPhase -> Type
data ChartParams p = MkChartParams
  { -- | Build directory.
    buildDir :: BuildDirF p,
    -- | If true, copies clean install of web dir and installs node deps.
    cleanInstall :: Bool,
    -- | Optional path to chart-requests.json.
    chartRequestsPath :: PathF p File,
    -- | Optional path to directory with runs file and chart-requests.json.
    dataDir :: PathF p Dir,
    -- | If true, stops the build after generating the intermediate json
    -- file.
    json :: Bool,
    -- | Optional path to run labels file.
    runLabelsPath :: MPathF p File,
    -- | Optional path to runs file.
    runPaths :: RunPathsF p
  }

type ChartParamsArgs = ChartParams ConfigPhaseArgs

type ChartParamsFinal = ChartParams ConfigPhaseFinal

deriving stock instance
  ( Eq (BuildDirF p),
    Eq (MPathF p File),
    Eq (PathF p Dir),
    Eq (PathF p File),
    Eq (RunPathsF p)
  ) =>
  Eq (ChartParams p)

deriving stock instance
  ( Show (BuildDirF p),
    Show (MPathF p File),
    Show (PathF p Dir),
    Show (PathF p File),
    Show (RunPathsF p)
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
  Maybe ConfigWithPath ->
  Eff es ChartParamsFinal
evolvePhase @es params mConfigWithPath = do
  (chartRequestsPath, runLabelsPath, runPaths) <-
    getChartInputs params mConfigWithPath

  assertExists chartRequestsPath
  for_ runPaths assertExists
  for_ runLabelsPath assertExists

  buildDir <-
    case params.buildDir of
      Just d -> do
        -- --build-dir exists, parse absolute or resolve relative to cwd
        cwdPath <- Types.getCachedCurrentDirectory
        customBuildDir cwdPath d
      Nothing -> case mConfigWithPath of
        Just t ->
          case preview (#config % #chartConfig %? #buildDir % _Just) t of
            -- config.build-dir exists, parse absolute or resolve relative to
            -- config path.
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
        runPaths
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

type ChartInputs =
  Tuple3
    -- chart-requests
    (Path Abs File)
    -- run-labels
    (Maybe (Path Abs File))
    -- run-paths
    (NonEmpty (Path Abs File))

getChartInputs ::
  forall es.
  ( HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  ChartParamsArgs ->
  Maybe ConfigWithPath ->
  Eff es ChartInputs
getChartInputs params mConfigWithPath = do
  Identity chartRequestsPath <-
    resolveRequiredChartInput
      @Maybe
      params
      mConfigWithPath
      "chart-requests"
      chartRequestsSearch
      params.chartRequestsPath
      (#chartConfig %? #chartRequestsPath)

  runPaths <-
    resolveRequiredChartInput
      @[]
      params
      mConfigWithPath
      "runs"
      runsSearch
      params.runPaths
      (#chartConfig %? #runPaths)

  runLabelsPath <-
    resolveChartInput
      @Maybe
      params
      mConfigWithPath
      "run-labels"
      runLabelsSearch
      params.runLabelsPath
      (#chartConfig %? #runLabelsPath)

  pure (chartRequestsPath, runLabelsPath, runPaths)

-- | Like 'resolveChartInput', except throws an exception if that path
-- is not determined. Similarly, does not check actual file existence.
resolveRequiredChartInput ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es,
    Traversable f
  ) =>
  ChartParamsArgs ->
  Maybe ConfigWithPath ->
  -- Text description
  Text ->
  -- Expected file_name(s) e.g. runs file
  SearchFiles ->
  -- Maybe file.
  f OsPath ->
  -- Config selector.
  AffineTraversal' Config (f OsPath) ->
  -- Returned path.
  Eff es (Alt1 f (Path Abs File))
resolveRequiredChartInput params mConfigWithPath desc fileNames mInputOsPath = do
  resolveChartInput' >=> throwIfMissing'
  where
    resolveChartInput' =
      resolveChartInput params mConfigWithPath desc fileNames mInputOsPath
    throwIfMissing' = throwIfMissing params mConfigWithPath fileNames

-- | Uses CLI, (possible) Config, and XDG to resolve a chart input path. Note
-- that this does _NOT_ guarantee that the path actually exists. This merely
-- reduces the entire configuration into the path(s) to be used. For
-- instance, if the user supplies a path on the CLI config, resolveChartInput
-- will return that path (without checking existence) as the CLI has the
-- highest priority.
--
-- The 'FromAlt' / 'Traversable' instances allow us to be polymorphic over
-- multiplicity. For instance, we want at most one file for chart-requests,
-- so f is specialized to 'Maybe'. On the other hand, runs allows multiple
-- files (e.g. runs.json and activities.csv), so f is 'List'.
--
-- Returns 'empty' (per 'Alternative') if no explicit paths were given and
-- we do not find any "expected" paths as a fallback.
resolveChartInput ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es,
    Traversable f
  ) =>
  ChartParamsArgs ->
  Maybe ConfigWithPath ->
  -- Text description
  Text ->
  -- Expected file_name(s) e.g. runs file(s)
  SearchFiles ->
  -- Maybe file.
  f OsPath ->
  -- Config selector.
  AffineTraversal' Config (f OsPath) ->
  -- Path, if we were given one or found an expected one.
  Eff es (f (Path Abs File))
resolveChartInput params mConfigWithPath desc fileNames mInputOsPath configSel =
  addNamespace "resolveChartInput" $ addNamespace desc $ do
    -- 1. Try Cli first.
    cliResult <- findCliPath params.dataDir fileNames mInputOsPath
    if isNonEmpty cliResult
      then pure cliResult
      else do
        $(Logger.logDebug) "No cli path(s) found, checking config"
        -- 2. Try Config next.
        configResult <- findConfigPath fileNames configSel mConfigWithPath
        if isNonEmpty configResult
          then pure configResult
          else do
            -- 3. Finally, fall back to xdg.
            $(Logger.logDebug) "No config path(s) found, falling back to xdg"
            (findXdgPath fileNames)
  where

findCliPath ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    Traversable f
  ) =>
  -- | Maybe data dir.
  Maybe OsPath ->
  -- | File names.
  SearchFiles ->
  -- Maybe file.
  f OsPath ->
  Eff es (f (Path Abs File))
findCliPath mDataDir fileNames mFiles = addNamespace "findCliPath" $ do
  if isNonEmpty mFiles
    -- 1. If mFiles is not empty (i.e. Nothing or empty list), we use it.
    then for mFiles parseCanonicalAbsFile
    -- 2. If mFiles is empty, then we search the data directory, if it is
    --    given.
    else fallback
  where
    fallback = case mDataDir of
      Nothing -> pure empty
      Just dataDir ->
        parseCanonicalAbsDir dataDir >>= Utils.searchFiles fileNames

findConfigPath ::
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    Traversable f
  ) =>
  -- | File names.
  SearchFiles ->
  -- | Config selector.
  AffineTraversal' Config (f OsPath) ->
  -- | Maybe config.
  Maybe ConfigWithPath ->
  Eff es (f (Path Abs File))
-- 1. Config does not exist. Nothing to do, just return empty.
findConfigPath _ _ Nothing = pure empty
findConfigPath fileNames configSel (Just configWithPath) =
  addNamespace "findConfigPath" $ do
    case preview (#config % configSel) configWithPath of
      -- 2. Config.paths field exists.
      Just mPath ->
        -- 2.1. Config.paths is non-empty, use it.
        if isNonEmpty mPath
          then
            for mPath
              $ handleUnknownPath
                configWithPath.dirPath
                Path.parseAbsFile
                Path.parseRelFile
          -- 2.2. Config.paths exists but is empty, try config data dir.
          else tryConfigDataDir
      -- 3. Config.paths does not exist, try config data dir.
      Nothing -> tryConfigDataDir
  where
    configDataSel :: AffineTraversal' ConfigWithPath OsPath
    configDataSel = #config % #chartConfig %? #dataDir % _Just

    tryConfigDataDir = case preview configDataSel configWithPath of
      Just dataDir -> do
        configDataDirPath <-
          handleUnknownPath
            configWithPath.dirPath
            Path.parseAbsDir
            Path.parseRelDir
            dataDir
        Utils.searchFiles fileNames configDataDirPath
      Nothing -> pure empty

findXdgPath ::
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  -- | File names.
  SearchFiles ->
  Eff es (f (Path Abs File))
findXdgPath fileNames = addNamespace "findXdgPath" $ do
  -- 3. Fallback to xdg
  xdgDir <- getCachedXdgConfigPath
  Utils.searchFiles fileNames xdgDir

chartRequestsSearch :: SearchFiles
chartRequestsSearch =
  MkSearchFiles
    $ NE.singleton
    $ MkFileAliases
    $ [relfile|chart-requests.json|]
    :| [[relfile|chart-requests.jsonc|]]

runsSearch :: SearchFiles
runsSearch = MkSearchFiles $ runsJsonName :| [runsGarminName]
  where
    runsJsonName =
      MkFileAliases $ [relfile|runs.json|] :| [[relfile|runs.jsonc|]]
    runsGarminName =
      MkFileAliases $ NE.singleton [relfile|Activities.csv|]

runLabelsSearch :: SearchFiles
runLabelsSearch =
  MkSearchFiles
    $ NE.singleton
    $ MkFileAliases
    $ [relfile|run-labels.json|]
    :| [[relfile|run-labels.jsonc|]]

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
  let p' = toOsPath p
  exists <- PR.doesFileExist p'
  unless exists $ throwM (MkFileNotFoundE p')

throwIfMissing ::
  ( FromAlt f,
    HasCallStack,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  ChartParamsArgs ->
  Maybe ConfigWithPath ->
  SearchFiles ->
  f a ->
  Eff es (Alt1 f a)
throwIfMissing params mConfigWithPath fileNames mVal = case toAlt1 mVal of
  Just ps -> pure ps
  Nothing -> do
    xdgDir <- getCachedXdgConfigPath
    throwM
      $ MkChartFileMissingE
        { cliDataDir = params.dataDir,
          expectedFiles = fileNames',
          configDataDir =
            mConfigWithPath >>= \t -> t.config.chartConfig >>= (.dataDir),
          xdgDir
        }
  where
    fileNames' = Utils.searchFilesToList fileNames

makeFieldLabelsNoPrefix ''ChartParams
