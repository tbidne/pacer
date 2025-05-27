{-# LANGUAGE QuasiQuotes #-}

module Pacer.Web.Paths
  ( -- * Internal paths
    webInternalPaths,
    webInternalDirs,
    webInternalFiles,

    -- * Paths
    webDir,
    distDir,
    getWebPath,

    -- * Functions
    readWebDirTH,
  )
where

import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import FileSystem.OsPath (decodeLenient)
import Language.Haskell.TH (Code, Q)
import Pacer.Prelude
import Pacer.Web.Utils qualified as WUtils

-- | Reads the web directory at compile-time.
readWebDirTH :: Code Q (List (Tuple2 (Path Rel File) ByteString))
readWebDirTH = WUtils.liftIOToTH readWebDirIO

readWebDirIO :: IO (List (Tuple2 (Path Rel File) ByteString))
readWebDirIO = runner $ do
  for webInternalFiles $ \fileName -> do
    let path = webDir <</>> fileName
        osPath = toOsPath path
    exists <- PR.doesFileExist (toOsPath path)

    unless exists $ do
      let msg =
            mconcat
              [ "Required build file '",
                packText $ decodeLenient osPath,
                "' does not exist. Has the nodejs frontend been built first? ",
                "Run 'npm install --save && npm run build' from the web/ ",
                "directory."
              ]
      throwText msg

    c <- readBinaryFile (toOsPath path)
    pure (fileName, c)
  where
    runner =
      runEff
        . runFileReader
        . runPathReader

-- | List of all paths used in frontend build.
webInternalPaths :: Tuple2 (List (Path Rel Dir)) (List (Path Rel File))
webInternalPaths = (webInternalDirs, webInternalFiles)

-- | List of all dirs used in frontend build. Nested dirs are only included
-- once e.g. @a/b@ instead of @[a, a/b]@.
webInternalDirs :: List (Path Rel Dir)
webInternalDirs = [[reldirPathSep|dist|]]

-- | List of all files used in frontend build.
webInternalFiles :: List (Path Rel File)
webInternalFiles =
  [ [relfilePathSep|dist/index.html|],
    [relfilePathSep|dist/bundle.js|]
  ]

-- | Web dir name.
webDir :: Path Rel Dir
webDir = [reldir|web|]

-- | Web output dir relative to 'webDir'.
distDir :: Path Rel Dir
distDir = [reldir|dist|]

-- | Web destination dir.
getWebPath ::
  ( HasCallStack,
    PathReader :> es
  ) =>
  Eff es (Path Abs Dir)
getWebPath = (<</>> webDir) <$> getXdgCachePath
