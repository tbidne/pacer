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
import Pacer.Config.Env (Env)
import Pacer.Config.Env qualified as Env
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
runCommand (cmd, mToml, env) = runner $ do
  command <- Command.evolvePhase cmd mToml
  case command of
    Chart params -> Chart.handle params
    Convert params -> Convert.handle params
    Derive params -> Derive.handle params
    Scale params -> Scale.handle params
  where
    runner =
      runReader env
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
  Tuple3
    (CommandPhaseArgs Double)
    (Maybe Toml)
    Env

getConfiguration ::
  ( HasCallStack,
    FileReader :> es,
    PathReader :> es,
    Optparse :> es
  ) =>
  Eff es Config
getConfiguration @es = do
  args <- execParser (parserInfo @Double)

  mToml <- do
    case args.configPath of
      Nothing -> do
        xdgConfig <- getXdgConfigPath
        let path = xdgConfig <</>> [relfile|config.toml|]

        exists <- PR.doesFileExist (pathToOsPath path)
        if exists
          then Just <$> readToml path
          else pure Nothing
      Just p -> Just <$> (parseCanonicalAbsFile p >>= readToml)

  pure (args.command, mToml, Env.mkEnv args mToml)
  where
    readToml :: Path b t -> Eff es Toml
    readToml path = do
      c <- readFileUtf8ThrowM (pathToOsPath path)
      throwLeft $ TOML.decode c

runLogger ::
  ( HasCallStack,
    Concurrent :> es,
    LoggerNS :> es,
    Reader Env :> es,
    Terminal :> es,
    Time :> es
  ) =>
  Eff (Logger : es) a ->
  Eff es a
runLogger = interpret_ $ \case
  LoggerLog _loc _logSrc lvl msg -> do
    mLogLevel <- asks @Env (.logLevel)
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
