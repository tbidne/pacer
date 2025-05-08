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
import Effectful.Logger.Dynamic (LogLevel (LevelInfo), Logger (LoggerLog))
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
import Pacer.Configuration.Args (Args (command, configPath), parserInfo)
import Pacer.Configuration.Config (ConfigWithPath (MkConfigWithPath, config))
import Pacer.Configuration.Env qualified as Env
import Pacer.Configuration.Env.Types
  ( CachedPaths (MkCachedPaths, currentDirectory, xdgConfigPath),
    LogEnv (MkLogEnv, logLevel, logNamespace),
  )
import Pacer.Exception qualified as PacerEx
import Pacer.Prelude
import Pacer.Utils (FileAliases (MkFileAliases))
import Pacer.Utils qualified as Utils
import System.OsPath qualified as FP

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
  Env ->
  Eff es ()
runCommand (cmd, mConfig, cachedPaths, logEnv) = runner $ logAndRethrow $ do
  command <- Command.evolvePhase cmd mConfig
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
  ( Concurrent :> es,
    FileReader :> es,
    HasCallStack,
    PathReader :> es,
    Optparse :> es,
    Terminal :> es,
    Time :> es
  ) =>
  (Env -> Eff es a) ->
  Eff es a
withEnv onEnv = getEnv >>= onEnv

type Env =
  Tuple4
    -- Command to run, before evolution
    (CommandPhaseArgs Double)
    -- Possible config
    (Maybe ConfigWithPath)
    -- Cached paths
    CachedPaths
    -- Logging env
    LogEnv

getEnv ::
  ( Concurrent :> es,
    FileReader :> es,
    HasCallStack,
    PathReader :> es,
    Optparse :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Eff es Env
getEnv = do
  args <- execParser (parserInfo @Double)

  -- Get Config and xdg config dir, if necessary.
  (mXdgConfig, mConfig) <- case args.command of
    -- Because the config (currently) only affects the chart command,
    -- searching for it is pointless for other commands, hence skip it.
    --
    -- 1. Chart command, try to find command.
    Chart {} -> configRunner $ do
      case args.configPath of
        -- 1.1. No config path, try xdg
        Nothing -> do
          xdgConfig <- getXdgConfigPath

          let configAliases =
                MkFileAliases
                  $ [relfile|config.json|]
                  :| [[relfile|config.jsonc|]]

          mPath <- Utils.searchFileAliases @Maybe True xdgConfig configAliases

          case mPath of
            Nothing -> pure (Just xdgConfig, Nothing)
            Just path -> do
              config <- Utils.readDecodeJson path
              let configPath = MkConfigWithPath xdgConfig config

              $(Logger.logInfo) $ "Using config: " <> (Utils.showtPath path)

              pure $ (Just xdgConfig, Just configPath)
        -- 1.2. Config path exists, use it.
        Just configPath -> do
          absPath <- parseCanonicalAbsFile configPath
          config <- Utils.readDecodeJson absPath

          absConfigDir <- Path.parseAbsDir $ FP.takeDirectory (toOsPath absPath)

          let configWithPath = MkConfigWithPath absConfigDir config
          $(Logger.logInfo) $ "Using config: " <> (Utils.showtPath absPath)
          pure (Nothing, Just configWithPath)

    -- 2. Non-chart command, skip config.
    _ -> pure (Nothing, Nothing)

  let cachedPaths =
        MkCachedPaths
          { currentDirectory = Nothing,
            xdgConfigPath = mXdgConfig
          }
      logEnv = Env.mkLogEnv args (mConfig <&> (.config))

  pure (args.command, mConfig, cachedPaths, logEnv)
  where
    configRunner =
      runReader configLogEnv
        . runLoggerNS "config"
        . runLogger

    configLogEnv =
      MkLogEnv
        { logLevel = Just LevelInfo,
          logNamespace = mempty
        }

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
  [ MkExceptionProxy @Utils.AesonE,
    MkExceptionProxy @PacerEx.ChartFileMissingE,
    MkExceptionProxy @PacerEx.CommandConvertE,
    MkExceptionProxy @PacerEx.CommandDeriveE,
    MkExceptionProxy @PacerEx.CommandScaleE,
    MkExceptionProxy @PacerEx.CreateChartE,
    MkExceptionProxy @PacerEx.FileNotFoundE,
    MkExceptionProxy @PacerEx.GarminE,
    MkExceptionProxy @PacerEx.NpmE,
    MkExceptionProxy @RunDatetimeOverlapE
  ]
