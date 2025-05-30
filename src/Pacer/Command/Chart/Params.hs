{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Chart parameters.
module Pacer.Command.Chart.Params
  ( -- * Types
    ChartParams (..),
    ChartParamsArgs,
    ChartParamsFinal,
    ActivitiesType (..),

    -- * Functions
    evolvePhase,

    -- * Type families
    ActivityPathsF,
    BuildDirF,
    MPathF,
    PathF,
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.List.NonEmpty qualified as NE
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import FileSystem.Path qualified as Path
import GHC.TypeError qualified as TE
import GHC.TypeLits (ErrorMessage (ShowType, (:<>:)), TypeError)
import Pacer.Class.FromAlt
  ( FromAlt (Alt1, toAlt1),
    (<+<|>+>),
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
    ConfigPhaseF,
  )
import Pacer.Exception
  ( ChartFileMissingE
      ( MkChartFileMissingE,
        cliDataDir,
        configDataDir,
        expectedFiles,
        xdgDir
      ),
  )
import Pacer.Prelude
import Pacer.Utils.FileSearch
  ( DirNotExistsStrategy (DirNotExistsFail),
    FileAliases (MkFileAliases),
    FileNotFoundE (MkFileNotFoundE),
    FileSearchStrategy (MkFileSearchStrategy),
    SearchFileType (SearchFileAliases, SearchFileInfix),
    SearchFiles (MkSearchFiles),
  )
import Pacer.Utils.FileSearch qualified as Utils.FileSearch
import System.OsPath qualified as OsPath

-- | The type of activities file.
data ActivitiesType
  = -- | Default activities type, activities.json.
    ActivitiesDefault
  | -- | Garmin activities type, activities.csv.
    ActivitiesGarmin
  deriving stock (Eq, Show)

instance Display ActivitiesType where
  displayBuilder = \case
    ActivitiesDefault -> "default (json)"
    ActivitiesGarmin -> "garmin (csv)"

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

type ActivityPathsF :: ConfigPhase -> Type
type family ActivityPathsF p where
  ActivityPathsF ConfigPhaseArgs = Seq OsPath
  ActivityPathsF ConfigPhaseFinal = NESeq (Path Abs File)

-- | Chart params.
type ChartParams :: ConfigPhase -> Type
data ChartParams p = MkChartParams
  { -- | Optional path to activity labels file.
    activityLabelsPath :: MPathF p File,
    -- | Optional path to activities file.
    activityPaths :: ActivityPathsF p,
    -- | Build directory.
    buildDir :: BuildDirF p,
    -- | If true, copies clean install of web dir.
    cleanInstall :: Bool,
    -- | Optional path to chart-requests.json.
    chartRequestsPath :: PathF p File,
    -- | Optional path to directory with activities file and chart-requests.json.
    dataDir :: PathF p Dir,
    -- | If true, stops the build after generating the intermediate json
    -- file.
    json :: Bool,
    -- | Port the server runs on.
    port :: ConfigPhaseF p Word16
  }

type ChartParamsArgs = ChartParams ConfigPhaseArgs

type ChartParamsFinal = ChartParams ConfigPhaseFinal

deriving stock instance
  ( Eq (ActivityPathsF p),
    Eq (BuildDirF p),
    Eq (ConfigPhaseF p Word16),
    Eq (MPathF p File),
    Eq (PathF p Dir),
    Eq (PathF p File)
  ) =>
  Eq (ChartParams p)

deriving stock instance
  ( Show (ActivityPathsF p),
    Show (BuildDirF p),
    Show (ConfigPhaseF p Word16),
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
  Maybe ConfigWithPath ->
  Eff es ChartParamsFinal
evolvePhase @es params mConfigWithPath = do
  (chartRequestsPath, activityLabelsPath, activityPaths) <-
    getChartInputs params mConfigWithPath

  -- This is _mostly_ unnecessary, as most logic verifies file existence.
  -- The json config doesn't in all cases though, and we should probably
  -- keep this out of paranoia, in any case.
  assertExists chartRequestsPath
  for_ activityPaths assertExists
  for_ activityLabelsPath assertExists

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

  let port = fromMaybe 3000 params.port

  pure
    $ MkChartParams
      { activityLabelsPath,
        activityPaths,
        buildDir,
        cleanInstall = params.cleanInstall,
        chartRequestsPath,
        dataDir = (),
        json = params.json,
        port
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
    -- activity-labels
    (Maybe (Path Abs File))
    -- activity-paths
    (NESeq (Path Abs File))

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

  activityPaths <-
    resolveRequiredChartInput
      @Seq
      params
      mConfigWithPath
      "activities"
      activitiesSearch
      params.activityPaths
      (#chartConfig %? #activityPaths)

  activityLabelsPath <-
    resolveChartInput
      @Maybe
      params
      mConfigWithPath
      "activity-labels"
      activityLabelsSearch
      params.activityLabelsPath
      (#chartConfig %? #activityLabelsPath)

  pure (chartRequestsPath, activityLabelsPath, activityPaths)

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
  -- Expected file_name(s) e.g. activities file
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
-- so f is specialized to 'Maybe'. On the other hand, activities allows multiple
-- files (e.g. activities.json and activities.csv), so f is 'List'.
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
  -- Expected file_name(s) e.g. activities file(s)
  SearchFiles ->
  -- Maybe file.
  f OsPath ->
  -- Config selector.
  AffineTraversal' Config (f OsPath) ->
  -- Path, if we were given one or found an expected one.
  Eff es (f (Path Abs File))
resolveChartInput params mConfigWithPath desc fileNames mInputOsPath configSel =
  addNamespace "resolveChartInput" $ addNamespace desc $ do
    -- When resolving a particular chart path, we try, in order:
    --
    -- 1. Cli file paths e.g. --chart-requests.
    -- 2. Cli dir e.g. --data
    -- 3. Config path.
    -- 4. Current directory.
    -- 4. Xdg dir.
    Utils.FileSearch.resolveFilePath
      desc
      fileNames
      [ Utils.FileSearch.findFilePath mInputOsPath,
        Utils.FileSearch.findDirectoryPath params.dataDir,
        findConfigPath configSel mConfigWithPath,
        Utils.FileSearch.findCurrentDirectoryPath,
        Utils.FileSearch.findXdgPath
      ]

findConfigPath ::
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    Traversable f
  ) =>
  -- | Config selector.
  AffineTraversal' Config (f OsPath) ->
  -- | Maybe config.
  Maybe ConfigWithPath ->
  FileSearchStrategy f es
-- 1. Config does not exist. Nothing to do, just return empty.
findConfigPath _ Nothing = mempty
-- 2. Config exists, check it.
findConfigPath configSel (Just configWithPath) = MkFileSearchStrategy $ \fileNames ->
  addNamespace "findConfigPath" $ do
    case preview (#config % configSel) configWithPath of
      -- 2. Config.paths field exists.
      Just mPath ->
        -- 2.1. Config.paths is non-empty, use it.
        -- 2.2. Config.paths exists but is empty, try config data dir.
        tryConfigPaths mPath <+<|>+> tryConfigDataDir fileNames
      -- 3. Config.paths does not exist, try config data dir.
      Nothing -> tryConfigDataDir fileNames
  where
    configDataSel :: AffineTraversal' ConfigWithPath OsPath
    configDataSel = #config % #chartConfig %? #dataDir % _Just

    tryConfigPaths mPath =
      for mPath
        $ handleUnknownPath
          configWithPath.dirPath
          Path.parseAbsFile
          Path.parseRelFile

    tryConfigDataDir fileNames = case preview configDataSel configWithPath of
      Just dataDir -> do
        configDataDirPath <-
          handleUnknownPath
            configWithPath.dirPath
            Path.parseAbsDir
            Path.parseRelDir
            dataDir
        Utils.FileSearch.searchFiles fileNames DirNotExistsFail configDataDirPath
      Nothing -> pure empty

chartRequestsSearch :: SearchFiles
chartRequestsSearch =
  MkSearchFiles
    $ NE.singleton
    $ SearchFileAliases
    $ MkFileAliases aliases
  where
    aliases =
      [ [relfile|chart-requests.json|],
        [relfile|chart-requests.jsonc|],
        [relfile|chart_requests.json|],
        [relfile|chart_requests.jsonc|]
      ]

activitiesSearch :: SearchFiles
activitiesSearch =
  MkSearchFiles
    [ SearchFileInfix [relfile|activities|] exts
    ]
  where
    exts =
      [ [osp|.csv|],
        [osp|.json|],
        [osp|.jsonc|]
      ]

activityLabelsSearch :: SearchFiles
activityLabelsSearch =
  MkSearchFiles
    $ NE.singleton
    $ SearchFileAliases
    $ MkFileAliases aliases
  where
    aliases =
      [ [relfile|activity-labels.json|],
        [relfile|activity-labels.jsonc|],
        [relfile|activity_labels.json|],
        [relfile|activity_labels.jsonc|]
      ]

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
    fileNames' = Utils.FileSearch.searchFilesToList fileNames

makeFieldLabelsNoPrefix ''ChartParams
