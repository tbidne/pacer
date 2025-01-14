{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Chart parameters.
module Pacer.Command.Chart.Params
  ( -- * Types
    ChartParams (..),
    ChartParamsArgs,
    ChartParamsFinal,

    -- * Functions
    evolvePhase,

    -- * Type families
    PathF,
  )
where

import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.Path qualified as Path
import GHC.TypeError qualified as TE
import GHC.TypeLits (ErrorMessage (ShowType, (:<>:)), TypeError)
import Pacer.Config.Env.Types (CachedPaths, getCachedXdgConfigPath)
import Pacer.Config.Phase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseFinal),
  )
import Pacer.Config.Toml
  ( Toml (chartRequestsPath, dataDir, runsPath),
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

-- | Chart params.
type ChartParams :: ConfigPhase -> Type
data ChartParams p = MkChartParams
  { -- | If true, copies clean install of web dir and installs node deps.
    cleanInstall :: Bool,
    -- | Optional path to chart-requests.toml.
    chartRequestsPath :: PathF p File,
    -- | Optional path to directory with runs.toml and chart-requests.toml.
    dataDir :: PathF p Dir,
    -- | If true, stops the build after generating the intermediate json
    -- file.
    json :: Bool,
    -- | Optional path to runs.toml.
    runsPath :: PathF p File
  }

type ChartParamsArgs = ChartParams ConfigPhaseArgs

type ChartParamsFinal = ChartParams ConfigPhaseFinal

deriving stock instance
  ( Eq (PathF p Dir),
    Eq (PathF p File)
  ) =>
  Eq (ChartParams p)

deriving stock instance
  ( Show (PathF p Dir),
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

  pure
    $ MkChartParams
      { cleanInstall = params.cleanInstall,
        chartRequestsPath,
        dataDir = (),
        json = params.json,
        runsPath
      }
  where
    -- Retrieves (Tuple2 chart-requests-path runs-path)
    getChartInputs :: Eff es (Path Abs File, Path Abs File)
    getChartInputs = do
      chartRequestsPath <-
        getChartInput
          "chart-requests"
          [chartRequestsName]
          params.chartRequestsPath
          (.chartRequestsPath)

      runsPath <-
        getChartInput
          "runs"
          [runsName]
          params.runsPath
          (.runsPath)

      pure (chartRequestsPath, runsPath)

    getChartInput ::
      -- Text description
      Text ->
      -- Expected file_name(s) e.g. runs.toml
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
                  <$> handleTomlPath
                    Path.parseAbsFile
                    Path.parseRelFile
                    tomlWithPath.dirPath
                    p
              Nothing -> case tomlWithPath.toml.dataDir of
                Just dataDir -> do
                  -- 2.2. Toml.data-dir/path exists, try it
                  tomlDataDirPath <-
                    handleTomlPath
                      Path.parseAbsDir
                      Path.parseRelDir
                      tomlWithPath.dirPath
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

    -- Handles toml config path, resolving any relative paths. Polymorphic
    -- over file/dir path.
    handleTomlPath ::
      -- Absolute parser.
      (OsPath -> Eff es (Path Abs t)) ->
      -- Relative parser.
      (OsPath -> Eff es (Path Rel t)) ->
      -- Dir containing the toml config.
      Path Abs Dir ->
      -- Path to resolve
      OsPath ->
      Eff es (Path Abs t)
    handleTomlPath absParser relParser tomlDir otherPath =
      if OsPath.isAbsolute otherPath
        -- Path is absolute, just parse it.
        then absParser otherPath
        -- Path is relative, parse and combine with tomlDir.
        else do
          relFile <- relParser otherPath
          pure $ tomlDir <</>> relFile

    assertExists :: (HasCallStack) => Path b t -> Eff es ()
    assertExists p = do
      let p' = pathToOsPath p
      exists <- PR.doesFileExist p'
      unless exists $ throwM (MkFileNotFoundE p')

    runsName :: Path Rel File
    runsName = [relfile|runs.toml|]

    chartRequestsName :: Path Rel File
    chartRequestsName = [relfile|chart-requests.toml|]
