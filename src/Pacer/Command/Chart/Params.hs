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

import Data.Foldable qualified as F
import Data.Functor.Identity (Identity (Identity))
import Data.Text qualified as T
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.OsPath (decodeLenient, encodeLenient)
import FileSystem.Path qualified as Path
import GHC.TypeError qualified as TE
import GHC.TypeLits (ErrorMessage (ShowType, (:<>:)), TypeError)
import Pacer.Class.FromAlt
  ( FromAlt (Alt1, toAlt1),
    isNonEmpty,
  )
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
  Maybe TomlWithPath ->
  Eff es ChartParamsFinal
evolvePhase @es params mTomlWithPath = do
  (chartRequestsPath, runLabelsPath, runPaths) <-
    getChartInputs params mTomlWithPath

  assertExists chartRequestsPath
  for_ runPaths assertExists
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
  Maybe TomlWithPath ->
  Eff es ChartInputs
getChartInputs params mTomlWithPath = do
  Identity chartRequestsPath <-
    resolveRequiredChartInput
      @Maybe
      params
      mTomlWithPath
      "chart-requests"
      [chartRequestsName]
      params.chartRequestsPath
      (#chartConfig %? #chartRequestsPath)

  let runsFileNames = [runsTomlName, runsGarminName]

  runPaths <-
    resolveRequiredChartInput
      @[]
      params
      mTomlWithPath
      "runs"
      runsFileNames
      params.runPaths
      (#chartConfig %? #runPaths)

  runLabelsPath <-
    resolveChartInput
      @Maybe
      params
      mTomlWithPath
      "run-labels"
      [runLabelsName]
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
  Maybe TomlWithPath ->
  -- Text description
  Text ->
  -- Expected file_name(s) e.g. runs file
  List (Path Rel File) ->
  -- Maybe file.
  f OsPath ->
  -- Toml selector.
  AffineTraversal' Toml (f OsPath) ->
  -- Returned path.
  Eff es (Alt1 f (Path Abs File))
resolveRequiredChartInput params mTomlWithPath desc fileNames mInputOsPath = do
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
  Maybe TomlWithPath ->
  -- Text description
  Text ->
  -- Expected file_name(s) e.g. runs file
  List (Path Rel File) ->
  -- Maybe file.
  f OsPath ->
  -- Toml selector.
  AffineTraversal' Toml (f OsPath) ->
  -- Path, if we were given one or found an expected one.
  Eff es (f (Path Abs File))
resolveChartInput params mTomlWithPath desc fileNames mInputOsPath tomlSel =
  addNamespace "getMChartInput" $ addNamespace desc $ do
    -- 1. Try Cli first.
    cliResult <- findCliPath params.dataDir fileNames mInputOsPath
    if isNonEmpty cliResult
      then pure cliResult
      else do
        $(Logger.logDebug) "No cli path(s) found, checking toml"
        -- 2. Try Toml next.
        tomlResult <- findTomlPath fileNames tomlSel mTomlWithPath
        if isNonEmpty tomlResult
          then pure tomlResult
          else do
            -- 3. Finally, fall back to xdg.
            $(Logger.logDebug) "No toml path(s) found, falling back to xdg"
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
  List (Path Rel File) ->
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
        parseCanonicalAbsDir dataDir >>= findMatches fileNames

findTomlPath ::
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    Traversable f
  ) =>
  -- | File names.
  List (Path Rel File) ->
  -- | Toml selector.
  AffineTraversal' Toml (f OsPath) ->
  -- | Maybe toml.
  Maybe TomlWithPath ->
  Eff es (f (Path Abs File))
-- 1. Toml does not exist. Nothing to do, just return empty.
findTomlPath _ _ Nothing = pure empty
findTomlPath fileNames tomlSel (Just tomlWithPath) = addNamespace "findTomlPath" $ do
  case preview (#toml % tomlSel) tomlWithPath of
    -- 2. Toml.paths field exists.
    Just mPath ->
      -- 2.1. Toml.paths is non-empty, use it.
      if isNonEmpty mPath
        then
          for mPath
            $ handleUnknownPath
              tomlWithPath.dirPath
              Path.parseAbsFile
              Path.parseRelFile
        -- 2.2. Toml.paths exists but is empty, try toml data dir.
        else tryTomlDataDir
    -- 3. Toml.paths does not exist, try toml data dir.
    Nothing -> tryTomlDataDir
  where
    tomlDataSel :: AffineTraversal' TomlWithPath OsPath
    tomlDataSel = #toml % #chartConfig %? #dataDir % _Just

    tryTomlDataDir = case preview tomlDataSel tomlWithPath of
      Just dataDir -> do
        tomlDataDirPath <-
          handleUnknownPath
            tomlWithPath.dirPath
            Path.parseAbsDir
            Path.parseRelDir
            dataDir
        findMatches fileNames tomlDataDirPath
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
  List (Path Rel File) ->
  Eff es (f (Path Abs File))
findXdgPath fileNames = addNamespace "findXdgPath" $ do
  -- 3. Fallback to xdg
  xdgDir <- getCachedXdgConfigPath
  findMatches fileNames xdgDir

findMatches ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    PathReader :> es
  ) =>
  -- | File names.
  List (Path Rel File) ->
  -- | Data dir to search.
  Path Abs Dir ->
  Eff es (f (Path Abs File))
findMatches fileNames dataDir = do
  $(Logger.logDebug) msg

  allFiles <- PR.listDirectory dataDirOsPath

  let go [] = pure empty
      go (f : fs) = do
        let path = dataDir <</>> f
        exists <- PR.doesFileExist (pathToOsPath path)
        if exists
          then (pure path <|>) <$> go fs
          else liftA2 (<|>) (searchCaseInsens f) (go fs)

      searchCaseInsens :: Path Rel File -> Eff es (f (Path Abs File))
      searchCaseInsens f = do
        let p = pathToOsPath f
            pLower = toLower p

        case F.find ((==) pLower . toLower) allFiles of
          Just osPath -> do
            relFile <- Path.parseRelFile osPath
            let absFile = dataDir <</>> relFile
                warn =
                  mconcat
                    [ "Did not find exact match for '",
                      packText $ Utils.showPath f,
                      "' but found '",
                      packText $ Utils.showPath absFile,
                      "', using it."
                    ]
            $(Logger.logWarn) warn
            pure $ pure absFile
          Nothing -> pure empty

  go fileNames
  where
    toLower :: OsPath -> OsPath
    toLower =
      encodeLenient
        . unpackText
        . T.toCaseFold
        . packText
        . decodeLenient

    dataDirOsPath = pathToOsPath dataDir
    msg =
      mconcat
        [ "Searching for path(s) ",
          Utils.showListF Utils.showtPath fileNames,
          " in: ",
          Utils.showtPath dataDir
        ]

runsTomlName :: Path Rel File
runsTomlName = [relfile|runs.toml|]

runsGarminName :: Path Rel File
runsGarminName = [relfile|Activities.csv|]

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
  ( FromAlt f,
    HasCallStack,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  ChartParamsArgs ->
  Maybe TomlWithPath ->
  List (Path Rel File) ->
  f a ->
  Eff es (Alt1 f a)
throwIfMissing params mTomlWithPath fileNames mVal = case toAlt1 mVal of
  Just ps -> pure ps
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
