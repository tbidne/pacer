{-# LANGUAGE QuasiQuotes #-}
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

import Effects.FileSystem.PathReader qualified as PR
import FileSystem.OsPath qualified as OsPath
import GHC.TypeError qualified as TE
import GHC.TypeLits (ErrorMessage (ShowType, (:<>:)), TypeError)
import Pacer.Config.Phase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseFinal),
  )
import Pacer.Config.Toml (Toml (chartRequestsPath, dataDir, runsPath))
import Pacer.Exception
  ( FileMissingE
      ( MkFileMissingE,
        cliDataDir,
        expectedFiles,
        tomlDataDir,
        xdgDir
      ),
  )
import Pacer.Log qualified as Log
import Pacer.Prelude
import Pacer.Utils qualified as Utils
import System.IO.Error qualified as Error

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
    MonadPathReader m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  ChartParamsArgs ->
  Maybe Toml ->
  m ChartParamsFinal
evolvePhase @m params mToml = do
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
    getChartInputs :: m (Path Abs File, Path Abs File)
    getChartInputs = do
      (mXdgDir, chartRequestsPath) <-
        getChartInput
          Nothing
          [chartRequestsName]
          params.chartRequestsPath
          (.chartRequestsPath)

      (_, runsPath) <-
        getChartInput
          mXdgDir
          [runsName]
          params.runsPath
          (.runsPath)

      pure (chartRequestsPath, runsPath)

    getChartInput ::
      -- Maybe xgd_directory. We take this as a param
      Maybe (Path Abs Dir) ->
      -- Expected file_name(s) e.g. runs.toml
      List (Path Rel File) ->
      -- Maybe file.
      Maybe OsPath ->
      -- Toml selector.
      (Toml -> Maybe OsPath) ->
      -- Tuple2 (Maybe xgd_directory) file
      m (Tuple2 (Maybe (Path Abs Dir)) (Path Abs File))
    getChartInput mXdgDir fileNames mInputOsPath tomlSel = do
      -- 1. Try Cli first.
      findCliPath >>= \case
        Just p -> pure (mXdgDir, p)
        Nothing ->
          -- 2. Try Toml next.
          findTomlPath >>= \case
            Just p -> pure (mXdgDir, p)
            -- 3. Finally, fall back to xdg.
            Nothing -> findXdgPath
      where
        findCliPath :: m (Maybe (Path Abs File))
        findCliPath = do
          case mInputOsPath of
            -- 1.1 Cli path exists, use it.
            Just inputPath -> do
              Just <$> parseCanonicalAbsFile inputPath
            Nothing -> do
              -- 1.2 Try Cli.data-dir/path
              join <$> for params.dataDir (parseCanonicalAbsDir >=> findFirstMatch)

        findTomlPath :: m (Maybe (Path Abs File))
        findTomlPath =
          case mToml >>= tomlSel of
            -- 2.1. Toml.path exists, use it
            Just p -> Just <$> parseCanonicalAbsFile p
            Nothing -> case mToml >>= (.dataDir) of
              -- 2.2. Toml.data-dir/path exists, try it
              Just tomlDataDirOsPath -> do
                tomlDataDirPath <- parseCanonicalAbsDir tomlDataDirOsPath
                findFirstMatch tomlDataDirPath
              Nothing -> pure Nothing

        findXdgPath :: m (Maybe (Path Abs Dir), Path Abs File)
        findXdgPath = do
          -- 3. Fallback to xdg
          xdgDir <- maybe getXdgConfigPath pure mXdgDir
          mXdgPath <- findFirstMatch xdgDir

          case mXdgPath of
            Just xdgPath -> pure $ (Just xdgDir, xdgPath)
            Nothing ->
              throwM
                $ MkFileMissingE
                  { cliDataDir = params.dataDir,
                    expectedFiles = fileNames,
                    tomlDataDir = mToml >>= (.dataDir),
                    xdgDir
                  }

        findFirstMatch :: Path Abs Dir -> m (Maybe (Path Abs File))
        findFirstMatch dataDir = do
          Log.debug msg
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
                  Utils.showListF (packText . Utils.showPath) fileNames,
                  " in: ",
                  showtPath dataDir
                ]

    showtPath = packText . OsPath.decodeLenient . pathToOsPath

    assertExists p = do
      let p' = pathToOsPath p
      exists <- PR.doesFileExist p'
      unless exists $ do
        throwPathIOError
          p'
          "createCharts"
          Error.doesNotExistErrorType
          "Required file does not exist."

    runsName :: Path Rel File
    runsName = [relfile|runs.toml|]

    chartRequestsName :: Path Rel File
    chartRequestsName = [relfile|chart-requests.toml|]
