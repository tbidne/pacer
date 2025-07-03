{-# LANGUAGE OverloadedLists #-}
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
import Data.Set qualified as Set
import Effectful.Logger.Dynamic (LogLevel (LevelInfo), Logger (LoggerLog))
import Effectful.Logger.Dynamic qualified as Logger
import Effectful.LoggerNS.Static
  ( LocStrategy (LocNone, LocPartial),
    LogFormatter
      ( MkLogFormatter,
        locStrategy,
        newline,
        threadLabel,
        timezone
      ),
  )
import Effectful.LoggerNS.Static qualified as LoggerNS
import Effectful.Terminal.Dynamic (putBinary)
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
import Pacer.Configuration.Args (parserInfo)
import Pacer.Configuration.Config (ConfigWithPath (MkConfigWithPath))
import Pacer.Configuration.Env qualified as Env
import Pacer.Configuration.Env.Types
  ( CachedPaths (MkCachedPaths, currentDirectory, xdgConfigPath),
    LogEnv (MkLogEnv, logLevel, logNamespace, logVerbosity),
  )
import Pacer.Configuration.Logging
  ( LogLevelParam (LogNone, LogSome),
    LogVerbosity (LogV0, LogV1),
  )
import Pacer.Exception qualified as PacerEx
import Pacer.Prelude
import Pacer.Utils.FileSearch
  ( FileAliases (MkFileAliases),
    FileSearch (SearchFileAliases),
  )
import Pacer.Utils.FileSearch qualified as Utils.FileSearch
import Pacer.Utils.Json qualified as Json
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
  Eff es Unit
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
  Eff es Unit
runCommand (cmd, mConfig, cachedPaths, logEnv) = runner $ logAndRethrow $ do
  command <- Command.evolvePhase cmd mConfig
  case command of
    Chart params -> do
      let mChartConfig = preview (_Just % #config % #chartConfig % _Just) mConfig
      Chart.handle mChartConfig params
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
  let cfgLogLvl = case args ^. #logLevel of
        -- 1. User specifed no logging, turn it off.
        Just LogNone -> Nothing
        -- 2. User did not specify; default to info.
        Nothing -> Just LevelInfo
        -- 3. User specified something; use it.
        Just (LogSome lvl) -> Just lvl

      cfgLogVerbosity = fromMaybe mempty (args ^. #logVerbosity)

  -- Get Config and xdg config dir, if necessary.
  (cachedPaths, mConfig) <- case args ^. #command of
    -- Because the config (currently) only affects the chart command,
    -- searching for it is pointless for other commands, hence skip it.
    --
    -- 1. Chart command, try to find command.
    Chart {} -> configRunner cfgLogLvl cfgLogVerbosity $ do
      let cachedPaths =
            MkCachedPaths
              { currentDirectory = Nothing,
                xdgConfigPath = Nothing
              }

      -- Resolve config path, get cached paths.
      (mPath, cachedPaths') <-
        runState cachedPaths
          $ Utils.FileSearch.resolveFilePath
            @Maybe
            "config"
            configSearchFiles
            [ -- Unlike the other files, we do _not_ search the data directory,
              -- because that partially comes from the config file, so it's
              -- a chicken-and-egg problem. Finding and parsing the config file
              -- should be wholly independent of the chart command's arguments,
              -- hence it cannot rely on data directory.
              Utils.FileSearch.findFilePath (args ^. #configPath),
              Utils.FileSearch.findCurrentDirectoryPath,
              Utils.FileSearch.findXdgPath
            ]

      mConfigWithPath <- case mPath of
        Just path -> do
          config <- Json.readDecodeJson path
          absConfigDir <- Path.parseAbsDir $ FP.takeDirectory (toOsPath path)
          let configWithPath = MkConfigWithPath absConfigDir config
          pure $ Just configWithPath
        Nothing -> pure Nothing

      pure (cachedPaths', mConfigWithPath)

    -- 2. Non-chart command, skip config.
    _ -> pure (mempty, Nothing)

  let logEnv = Env.mkLogEnv args (mConfig <&> view #config)

  pure (args ^. #command, mConfig, cachedPaths, logEnv)
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

    configSearchFiles = SearchFileAliases configAliases
    configAliases =
      MkFileAliases
        [[relfile|config|]]
        exts
    exts =
      Set.fromList
        [ [osp|.json|],
          [osp|.jsonc|]
        ]

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
  LoggerLog loc _logSrc lvl msg -> do
    logEnv <- ask @LogEnv
    mLogLevel <- asks @LogEnv (view #logLevel)
    case mLogLevel of
      Nothing -> pure ()
      Just logLevel -> do
        Logger.guardLevel logLevel lvl $ do
          let locStrategy = case logEnv ^. #logVerbosity of
                LogV0 -> LocNone
                LogV1 -> LocPartial loc

          formatted <- LoggerNS.formatLog (fmt locStrategy) lvl msg
          putBinary $ LoggerNS.logStrToBs formatted
    where
      fmt locStrategy =
        MkLogFormatter
          { locStrategy,
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
  [ MkExceptionProxy @Json.AesonE,
    MkExceptionProxy @PacerEx.ChartFileMissingE,
    MkExceptionProxy @PacerEx.CommandConvertE,
    MkExceptionProxy @PacerEx.CommandDeriveE,
    MkExceptionProxy @PacerEx.CommandScaleE,
    MkExceptionProxy @PacerEx.CreateChartE,
    MkExceptionProxy @Utils.FileSearch.DirNotFoundE,
    MkExceptionProxy @Utils.FileSearch.FileNotFoundE,
    MkExceptionProxy @PacerEx.GarminE,
    MkExceptionProxy @ActivityDatetimeOverlapE
  ]
