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
  ( Toml (chartBuildDir, chartRequestsPath, dataDir, runsPath),
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
    -- | Optional path to runs file.
    runsPath :: PathF p File,
    -- | Optional specification for which runs type file we are using.
    runsType :: Maybe RunsType
  }

type ChartParamsArgs = ChartParams ConfigPhaseArgs

type ChartParamsFinal = ChartParams ConfigPhaseFinal

deriving stock instance
  ( Eq (BuildDirF p),
    Eq (PathF p Dir),
    Eq (PathF p File)
  ) =>
  Eq (ChartParams p)

deriving stock instance
  ( Show (BuildDirF p),
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
  (chartRequestsPath, runsPath) <- getChartInputs

  assertExists chartRequestsPath
  assertExists runsPath

  buildDir <-
    case params.buildDir of
      Just d -> do
        -- --build-dir exists, parse absolute or resolve relative to cwd
        cwdPath <- Types.getCachedCurrentDirectory
        customBuildDir cwdPath d
      Nothing -> case mTomlWithPath of
        Just t ->
          case t.toml.chartBuildDir of
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
    getChartInputs :: Eff es (Path Abs File, Path Abs File)
    getChartInputs = do
      chartRequestsPath <-
        getChartInput
          "chart-requests"
          [chartRequestsName]
          params.chartRequestsPath
          (.chartRequestsPath)

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
        getChartInput
          "runs"
          runsFileNames
          params.runsPath
          (.runsPath)

      pure (chartRequestsPath, runsPath)

    getChartInput ::
      -- Text description
      Text ->
      -- Expected file_name(s) e.g. runs file
      List (Path Rel File) ->
      -- Maybe file.
      Maybe OsPath ->
      -- Toml selector.
      (Toml -> Maybe OsPath) ->
      -- Tuple2 file
      Eff es (Path Abs File)
    getChartInput desc fileNames mInputOsPath tomlSel =
      addNamespace "getChartInput" $ addNamespace desc $ do
        -- 1. Try Cli first.
        findCliPath >>= \case
          Just p -> pure p
          Nothing -> do
            $(Logger.logDebug) "No cli path(s) found, checking toml"
            -- 2. Try Toml next.
            findTomlPath >>= \case
              Just p -> pure p
              -- 3. Finally, fall back to xdg.
              Nothing -> do
                $(Logger.logDebug) "No toml path(s) found, falling back to xdg"
                findXdgPath
      where
        findCliPath :: Eff es (Maybe (Path Abs File))
        findCliPath = addNamespace "findCliPath" $ do
          case mInputOsPath of
            -- 1.1 Cli path exists, use it.
            Just inputPath -> do
              Just <$> parseCanonicalAbsFile inputPath
            Nothing -> do
              -- 1.2 Try Cli.data-dir/path
              join <$> for params.dataDir (parseCanonicalAbsDir >=> findFirstMatch)

        findTomlPath :: Eff es (Maybe (Path Abs File))
        findTomlPath = addNamespace "findTomlPath"
          $ case mTomlWithPath of
            Just tomlWithPath -> case tomlSel tomlWithPath.toml of
              -- 2.1. Toml.path exists, use it
              Just p ->
                Just
                  <$> handleUnknownPath
                    tomlWithPath.dirPath
                    Path.parseAbsFile
                    Path.parseRelFile
                    p
              Nothing -> case tomlWithPath.toml.dataDir of
                Just dataDir -> do
                  -- 2.2. Toml.data-dir/path exists, try it
                  tomlDataDirPath <-
                    handleUnknownPath
                      tomlWithPath.dirPath
                      Path.parseAbsDir
                      Path.parseRelDir
                      dataDir
                  findFirstMatch tomlDataDirPath
                Nothing -> pure Nothing
            Nothing -> pure Nothing

        findXdgPath :: Eff es (Path Abs File)
        findXdgPath = addNamespace "findXdgPath" $ do
          -- 3. Fallback to xdg
          xdgDir <- getCachedXdgConfigPath
          mXdgPath <- findFirstMatch xdgDir

          case mXdgPath of
            Just xdgPath -> pure xdgPath
            Nothing ->
              throwM
                $ MkChartFileMissingE
                  { cliDataDir = params.dataDir,
                    expectedFiles = fileNames,
                    tomlDataDir = mTomlWithPath >>= \t -> t.toml.dataDir,
                    xdgDir
                  }

        findFirstMatch :: Path Abs Dir -> Eff es (Maybe (Path Abs File))
        findFirstMatch dataDir = do
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

    -- Handles an unknown (wrt. absolute/relative) path, resolving any
    -- relative paths. Polymorphic over file/dir path.
    handleUnknownPath ::
      -- Containing dir for relative paths.
      Path Abs Dir ->
      -- Absolute parser.
      (OsPath -> Eff es (Path Abs t)) ->
      -- Relative parser.
      (OsPath -> Eff es (Path Rel t)) ->
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

    assertExists :: (HasCallStack) => Path b t -> Eff es ()
    assertExists p = do
      let p' = pathToOsPath p
      exists <- PR.doesFileExist p'
      unless exists $ throwM (MkFileNotFoundE p')

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
