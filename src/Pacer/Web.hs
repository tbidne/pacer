{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Pacer.Web
  ( ensureWebDirExists,
  )
where

import Data.List (and)
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import Pacer.Log qualified as Log
import Pacer.Prelude
import Pacer.Web.Paths qualified as WPaths

-- | Writes the web directory if it does not exist.
ensureWebDirExists ::
  ( HasCallStack,
    FileWriter :> es,
    PathReader :> es,
    PathWriter :> es,
    Terminal :> es
  ) =>
  -- | Path to web path dir. This should be WPaths.getWebPath, though we
  -- take it as a parameter to save a function call.
  Path Abs Dir ->
  Bool ->
  Eff es ()
ensureWebDirExists webPath cleanInstall = do
  let webOsPath = pathToOsPath webPath

  exists <- webDirExists webPath

  let copyFiles = not exists || cleanInstall

  when copyFiles $ do
    Log.debug "Copying web source"
    when cleanInstall (PW.removePathForcibly webOsPath)
    writeWebDir webPath

-- | Determines if the web dir exists.
webDirExists :: (HasCallStack, PathReader :> es) => Path Abs Dir -> Eff es Bool
webDirExists webPath = do
  let webOsPath = pathToOsPath webPath
  dstExists <- PR.doesDirectoryExist webOsPath

  if not dstExists
    then pure False
    else do
      -- Should this be more robust? I.e. check contents somehow (hash)?
      exists <-
        for
          WPaths.webInternalFiles
          (PR.doesFileExist . pathToOsPath . (webPath <</>>))
      pure $ and exists

-- | Writes the web directory.
writeWebDir ::
  ( HasCallStack,
    FileWriter :> es,
    PathWriter :> es
  ) =>
  Path Abs Dir ->
  Eff es ()
writeWebDir webPath = do
  let webOsPath = pathToOsPath webPath

  PW.createDirectoryIfMissing True webOsPath
  for_
    WPaths.webInternalDirs
    (PW.createDirectoryIfMissing True . pathToOsPath . (webPath <</>>))

  for_ $$WPaths.readWebDirTH $ \(p, c) -> do
    writeBinaryFile (pathToOsPath $ webPath <</>> p) c
