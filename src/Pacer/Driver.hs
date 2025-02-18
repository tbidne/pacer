{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pacer.Driver
  ( -- * Main
    runApp,

    -- * Logger
    runLogger,

    -- * Exceptions
    displayInnerMatchKnown,
    knownExceptions,
  )
where

import Control.Exception (SomeException (SomeException))
import Control.Exception.Annotation.Utils (ExceptionProxy (MkExceptionProxy))
import Control.Exception.Annotation.Utils qualified as Ex.Ann.Utils
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic (Logger (LoggerLog))
import Effectful.Logger.Dynamic qualified as Logger
import Effectful.LoggerNS.Static
  ( LocStrategy (LocNone),
    LogFormatter
      ( MkLogFormatter,
        locStrategy,
        newline,
        threadLabel,
        timezone
      ),
  )
import Effectful.LoggerNS.Static qualified as LoggerNS
import FileSystem.Path qualified as Path
import Pacer.Command
  ( Command (Chart, Convert, Derive, Scale),
    CommandPhaseArgs,
  )
import Pacer.Command qualified as Command
import Pacer.Command.Chart qualified as Chart
import Pacer.Command.Chart.Data.Run (RunDatetimeOverlapE)
import Pacer.Command.Convert qualified as Convert
import Pacer.Command.Derive qualified as Derive
import Pacer.Command.Scale qualified as Scale
import Pacer.Config.Args (Args (command, configPath), parserInfo)
import Pacer.Config.Env qualified as Env
import Pacer.Config.Env.Types
  ( CachedPaths (MkCachedPaths, currentDirectory, xdgConfigPath),
    LogEnv (logLevel),
  )
import Pacer.Config.Toml (Toml, TomlWithPath (MkTomlWithPath, toml))
import Pacer.Exception qualified as PacerEx
import Pacer.Prelude
import System.OsPath qualified as FP
import TOML qualified

runApp ::
  ( HasCallStack,
    Concurrent :> es,
    FileReader :> es,
    FileWriter :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es,
    Time :> es,
    TypedProcess :> es
  ) =>
  Eff es ()
runApp = withEnv runCommand

runCommand ::
  ( HasCallStack,
    Concurrent :> es,
    FileReader :> es,
    FileWriter :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es,
    Time :> es,
    TypedProcess :> es
  ) =>
  Config ->
  Eff es ()
runCommand (cmd, mToml, cachedPaths, logEnv) = runner $ logAndRethrow $ do
  command <- Command.evolvePhase cmd mToml
  case command of
    Chart params -> Chart.handle params
    Convert params -> Convert.handle params
    Derive params -> Derive.handle params
    Scale params -> Scale.handle params
  where
    runner =
      evalState cachedPaths
        . runReader logEnv
        . runLoggerNS "main"
        . runLogger

    logAndRethrow m =
      trySync m >>= \case
        Right x -> pure x
        Left err -> do
          $(Logger.logFatal) $ displayExceptiont err
          throwM err

withEnv ::
  ( HasCallStack,
    FileReader :> es,
    PathReader :> es,
    Optparse :> es
  ) =>
  (Config -> Eff es a) ->
  Eff es a
withEnv onEnv = getConfiguration >>= onEnv

type Config =
  Tuple4
    -- Command to run, before evolution
    (CommandPhaseArgs Double)
    -- Possible toml config
    (Maybe TomlWithPath)
    -- Cached paths
    CachedPaths
    -- Logging env
    LogEnv

getConfiguration ::
  ( HasCallStack,
    FileReader :> es,
    PathReader :> es,
    Optparse :> es
  ) =>
  Eff es Config
getConfiguration @es = do
  args <- execParser (parserInfo @Double)

  -- Get toml and xdg config, if necessary.
  (mXdgConfig, mToml) <- do
    case args.configPath of
      -- No config path, try xdg
      Nothing -> do
        xdgConfig <- getXdgConfigPath
        let path = xdgConfig <</>> [relfile|config.toml|]

        exists <- PR.doesFileExist (pathToOsPath path)
        if exists
          then do
            toml <- readToml path
            let tomlPath = MkTomlWithPath xdgConfig toml
            pure $ (Just xdgConfig, Just tomlPath)
          else pure (Just xdgConfig, Nothing)
      -- Config path exists, use it.
      Just configPath -> do
        absPath <- parseCanonicalAbsFile configPath
        toml <- readToml absPath

        absTomlDir <- Path.parseAbsDir $ FP.takeDirectory (pathToOsPath absPath)

        let tomlPath = MkTomlWithPath absTomlDir toml
        pure (Nothing, Just tomlPath)

  let cachedPaths =
        MkCachedPaths
          { currentDirectory = Nothing,
            xdgConfigPath = mXdgConfig
          }
      logEnv = Env.mkLogEnv args (mToml <&> (.toml))

  pure (args.command, mToml, cachedPaths, logEnv)
  where
    readToml :: Path b t -> Eff es Toml
    readToml path = do
      c <- readFileUtf8ThrowM (pathToOsPath path)
      throwLeft $ TOML.decode c

runLogger ::
  ( HasCallStack,
    Concurrent :> es,
    LoggerNS :> es,
    Reader LogEnv :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Eff (Logger : es) a ->
  Eff es a
runLogger = interpret_ $ \case
  LoggerLog _loc _logSrc lvl msg -> do
    mLogLevel <- asks @LogEnv (.logLevel)
    case mLogLevel of
      Nothing -> pure ()
      Just logLevel -> do
        Logger.guardLevel logLevel lvl $ do
          formatted <- LoggerNS.formatLog fmt lvl msg
          let txt = LoggerNS.logStrToText formatted
          putText txt
    where
      fmt =
        MkLogFormatter
          { locStrategy = LocNone,
            newline = True,
            threadLabel = False,
            timezone = False
          }

displayInnerMatchKnown :: (Exception e) => e -> String
displayInnerMatchKnown e =
  if Ex.Ann.Utils.matchesException knownExceptions e
    then case toException e of
      SomeException innerEx -> displayException innerEx
    else displayException e

knownExceptions :: List ExceptionProxy
knownExceptions =
  [ MkExceptionProxy @PacerEx.ChartFileMissingE,
    MkExceptionProxy @PacerEx.CommandConvertE,
    MkExceptionProxy @PacerEx.CommandDeriveE,
    MkExceptionProxy @PacerEx.CommandScaleE,
    MkExceptionProxy @PacerEx.CreateChartE,
    MkExceptionProxy @PacerEx.FileNotFoundE,
    MkExceptionProxy @PacerEx.GarminE,
    MkExceptionProxy @PacerEx.NpmE,
    MkExceptionProxy @RunDatetimeOverlapE,
    MkExceptionProxy @PacerEx.TomlE
  ]
