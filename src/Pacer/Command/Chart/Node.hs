{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Pacer.Command.Chart.Node
  ( NpmExe,
    findNpm,
    Internal.runNpm,
    Internal.runNpm_,
    Internal.NpmE,
  )
where

#if BUNDLE_NODE
import Codec.Archive.Tar qualified as Tar
import Codec.Compression.Zlib qualified as Zlib
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic qualified as PR
import FileSystem.OsPath (decodeThrowM)
import Pacer.Command.Chart.Node.Internal (NodeBundle, NpmExe (MkNpmExe))
#else
import Pacer.Command.Chart.Node.Internal (NpmExe)
#endif
import Pacer.Command.Chart.Node.Internal qualified as Internal
import Pacer.Prelude

findNpm ::
  ( HasCallStack,
    PathReader :> es,
    PathWriter :> es
  ) =>
  Bool ->
  Eff es NpmExe

#if BUNDLE_NODE


findNpm cleanInstall = do
  let nodeBundle :: NodeBundle
      --bundled = $$Internal.bundledNpmTH
      nodeBundle = todo

  xdgData <- getXdgDataPath
  let xdgDataOsPath = toOsPath xdgData
      npmPath = xdgData <</>> nodeBundle.npmExePath
      npmOsPath = toOsPath npmPath

  npmExists <- PR.doesFileExist npmOsPath
  let copyNode = not npmExists || cleanInstall

  when copyNode $ do

    let nodeTarDecompressed = Zlib.decompress nodeBundle.contents
        entries = Tar.read nodeTarDecompressed

    xdgDataStr <- decodeThrowM (toOsPath xdgData)

    PR.removeDirectoryIfExists_ xdgDataOsPath

    -- FIXME: Add effect!!!
    unsafeEff_ $ Tar.unpack xdgDataStr entries

  pure $ MkNpmExe npmPath

#else

findNpm _ = Internal.findNpm

#endif
