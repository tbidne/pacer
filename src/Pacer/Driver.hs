{-# LANGUAGE QuasiQuotes #-}

module Pacer.Driver
  ( -- * Main
    runApp,
  )
where

import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Pacer.Command (Command (Chart, Convert, Derive, Scale), evolvePhase)
import Pacer.Command.Chart qualified as Chart
import Pacer.Command.Convert qualified as Convert
import Pacer.Command.Derive qualified as Derive
import Pacer.Command.Scale qualified as Scale
import Pacer.Config.Args (Args (command, configPath), parserInfo)
import Pacer.Config.Toml (Toml)
import Pacer.Prelude
import TOML qualified

runApp ::
  ( HasCallStack,
    FileReader :> es,
    FileWriter :> es,
    Optparse :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es,
    TypedProcess :> es
  ) =>
  Eff es ()
runApp @es = do
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

  cmd <- evolvePhase args.command mToml

  case cmd of
    Chart params -> Chart.handle params
    Convert params -> Convert.handle params
    Derive params -> Derive.handle params
    Scale params -> Scale.handle params
  where
    readToml :: Path b t -> Eff es Toml
    readToml path = do
      c <- readFileUtf8ThrowM (pathToOsPath path)
      throwLeft $ TOML.decode c
