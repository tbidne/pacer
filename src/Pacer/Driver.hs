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

    -- * Misc
    Env,
    getEnv,
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
import Pacer.Command.Chart.Data.Activity (ActivityDatetimeOverlapE)
import Pacer.Command.Chart.Server (ServerEff)
import Pacer.Command.Convert qualified as Convert
import Pacer.Command.Derive qualified as Derive
import Pacer.Command.Scale qualified as Scale
import Pacer.Configuration.Args
  ( Args (command, configPath, logLevel, logVerbosity),
    parserInfo,
  )
import Pacer.Configuration.Config (ConfigWithPath (MkConfigWithPath, config))
import Pacer.Configuration.Env qualified as Env
import Pacer.Configuration.Env.Types
  ( CachedPaths (MkCachedPaths, currentDirectory, xdgConfigPath),
    LogEnv (MkLogEnv, logLevel, logNamespace, logVerbosity),
  )
import Pacer.Configuration.Logging (LogLevelParam (LogNone, LogSome))
import Pacer.Exception qualified as PacerEx
import Pacer.Prelude
import Pacer.Utils
  ( DirExistsCheck (DirExistsCheckOn),
    DirNotExistsHandler (DirNotExistsOk),
    FileAliases (MkFileAliases),
  )
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
    ServerEff :> es,
    Terminal :> es,
    Time :> es
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
    ServerEff :> es,
    Terminal :> es,
    Time :> es
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
  -- NOTE: [Numeric Type]
  --
  -- This is where we choose the core numeric type for pacer for cli commands.
  -- The chart command is set in fixed in ChartData. A couple notes:
  --
  -- We have tried to relax the mandatory invariants, that is, reducing
  -- core types from e.g.
  --
  --   MkDistance { unDistance :: Positive a }
  --
  -- to one of:
  --
  --   1. MkDistance { unDistance :: NonNegative a }
  --   2. MkDistance { unDistance :: a }
  --
  -- There are two aspects here:
  --
  --   A. Moving the invariant from the core type to here.
  --   B. Reducing the type from Positive to NonNegative.
  --
  -- B is arguably the Right Thing To Do, since zero is a perfectly sensible
  -- (albeit silly) value for the general CLI commands. Positive was only
  -- chosen for convenience i.e. to ensure division was okay.
  --
  -- Thus relaxing to NonNegative is _fine_, though care has to be taken
  -- to ensure division is safe (e.g. only using 'safe' division operators,
  -- and failing fast otherwise). The real reason this hasn't been done yet
  -- is it is a major pain with charts. The charts relies on Positive values
  -- in several places (calculating pace for filters, output values), and
  -- refactoring everything to allow failure is pretty terrible.
  --
  -- It's doable, but in the absence of a compelling reason, does not seem
  -- worth it. Especially because such values cannot even be charted, without
  -- giving them an arbitrary value like zero. So we give up logging errors
  -- in the parser, only to log them later in the complicated chart creation
  -- logic? And this is all so that CLI allows zero values? Probably not
  -- worth it.
  --
  -- A, OTOH, is interesting since we can still preserve our Positive
  -- invariants, i.e. just move Positive here and hardcode it in ChartData.
  -- That's fine. A nice bonus is that some of the tests are easier to write,
  -- since we can then write Monoid/Group instances. But other parts get
  -- worse, since we still need to generate Positive values in some cases.
  -- This also means we could have a divergence in test and actual behavior,
  -- given actual behavior is using Positive and tests might not be.
  --
  -- Once again, it's probably not worth it unless there is a compelling
  -- reason.
  --
  -- The whole impetus to explore this was that the parse error in garmin
  -- was terrible, since it logged every non-positive error on a new line,
  -- and it turns out there can be quite a few in real data. We have
  -- improved on the error by special-casing this to print all non-positive
  -- errors inline. So it could be a very long line, but at least it's just
  -- one. If this gets more annoying, we could add config to ignore this
  -- specific error.
  args <- execParser (parserInfo @Double)

  -- Normally, we combine the Args' LogLevel with the json Config's LogLevel
  -- to create the LogEnv used throughout the program. However, we have a
  -- chicken-and-egg problem where the Config creation process potentially
  -- has logging too i.e. we need to log but don't have the merged config.
  --
  -- Therefore, we use the Args' LogLevel for Config creation, then use the
  -- combined version for everything else.
  let cfgLogLvl = case args.logLevel of
        -- 1. User specifed no logging, turn it off.
        Just LogNone -> Nothing
        -- 2. User did not specify; default to info.
        Nothing -> Just LevelInfo
        -- 3. User specified something; use it.
        Just (LogSome lvl) -> Just lvl

      cfgLogVerbosity = fromMaybe mempty args.logVerbosity

  -- Get Config and xdg config dir, if necessary.
  (mXdgConfig, mConfig) <- case args.command of
    -- Because the config (currently) only affects the chart command,
    -- searching for it is pointless for other commands, hence skip it.
    --
    -- 1. Chart command, try to find command.
    Chart {} -> configRunner cfgLogLvl cfgLogVerbosity $ do
      case args.configPath of
        -- 1.1. No config path, try xdg
        Nothing -> do
          xdgConfig <- getXdgConfigPath

          let configAliases =
                MkFileAliases
                  $ [relfile|config.json|]
                  :| [[relfile|config.jsonc|]]

          mPath <-
            Utils.searchFileAliases
              @Maybe
              (DirExistsCheckOn DirNotExistsOk)
              xdgConfig
              configAliases

          case mPath of
            Nothing -> pure (Just xdgConfig, Nothing)
            Just path -> do
              config <- Utils.readDecodeJson path
              let configPath = MkConfigWithPath xdgConfig config

              $(Logger.logInfo) $ "Using config: " <> Utils.showtPath path

              pure (Just xdgConfig, Just configPath)
        -- 1.2. Config path exists, use it.
        Just configPath -> do
          absPath <- parseCanonicalAbsFile configPath
          config <- Utils.readDecodeJson absPath

          absConfigDir <- Path.parseAbsDir $ FP.takeDirectory (toOsPath absPath)

          let configWithPath = MkConfigWithPath absConfigDir config
          $(Logger.logInfo) $ "Using config: " <> Utils.showtPath absPath
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
    configRunner cfgLogLvl logVerbosity =
      runReader (configLogEnv cfgLogLvl logVerbosity)
        . runLoggerNS "config"
        . runLogger

    configLogEnv logLevel logVerbosity =
      MkLogEnv
        { logLevel,
          logNamespace = mempty,
          logVerbosity
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
    MkExceptionProxy @Utils.DirNotFoundE,
    MkExceptionProxy @PacerEx.FileNotFoundE,
    MkExceptionProxy @PacerEx.GarminE,
    MkExceptionProxy @ActivityDatetimeOverlapE
  ]
