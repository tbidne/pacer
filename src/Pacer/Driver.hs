{-# LANGUAGE QuasiQuotes #-}

module Pacer.Driver
  ( -- * Main
    runApp,

    -- * Logger
    runLogger,
  )
where

import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic (Logger (LoggerLog))
import Effectful.Logger.Dynamic qualified as Logger
import Effectful.LoggerNS.Static
import Effectful.LoggerNS.Static qualified as LoggerNS
import Pacer.Command
  ( Command (Chart, Convert, Derive, Scale),
    CommandPhaseArgs,
  )
import Pacer.Command qualified as Command
import Pacer.Command.Chart qualified as Chart
import Pacer.Command.Convert qualified as Convert
import Pacer.Command.Derive qualified as Derive
import Pacer.Command.Scale qualified as Scale
import Pacer.Config.Args (Args (command, configPath), parserInfo)
import Pacer.Config.Env qualified as Env
import Pacer.Config.Env.Types
  ( CachedPaths (MkCachedPaths, xdgConfigPath),
    LogEnv (logLevel),
  )
import Pacer.Config.Toml (Toml)
import Pacer.Prelude
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
runCommand (cmd, mToml, cachedPaths, logEnv) = runner $ do
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
        . runLoggerNS mempty
        . runLogger

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
    (Maybe Toml)
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

  (mXdgConfig, mToml) <- do
    case args.configPath of
      Nothing -> do
        xdgConfig <- getXdgConfigPath
        let path = xdgConfig <</>> [relfile|config.toml|]

        exists <- PR.doesFileExist (pathToOsPath path)
        if exists
          then (Just xdgConfig,) . Just <$> readToml path
          else pure (Just xdgConfig, Nothing)
      Just p -> (Nothing,) . Just <$> (parseCanonicalAbsFile p >>= readToml)

  let cachedPaths =
        MkCachedPaths
          { xdgConfigPath = mXdgConfig
          }
      logEnv = Env.mkLogEnv args mToml

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
