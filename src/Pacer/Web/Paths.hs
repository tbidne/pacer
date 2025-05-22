{-# LANGUAGE QuasiQuotes #-}

module Pacer.Web.Paths
  ( -- * Internal paths
    webInternalPaths,
    webInternalDirs,
    webInternalFiles,

    -- * Paths
    webDir,
    dataDir,
    distDir,
    getWebPath,

    -- * Functions
    readWebDirTH,
  )
where

import FileSystem.IO (readBinaryFileIO)
import Language.Haskell.TH (Code, Q)
import Pacer.Prelude
import Pacer.Web.Utils qualified as WUtils

-- | Reads the web directory at compile-time.
readWebDirTH :: Code Q (List (Tuple2 (Path Rel File) ByteString))
readWebDirTH = WUtils.liftIOToTH $ for webInternalFiles $ \fileName -> do
  c <- readBinaryFileIO (toOsPath $ webDir <</>> fileName)
  pure (fileName, c)

-- | List of all paths used in frontend build.
webInternalPaths :: Tuple2 (List (Path Rel Dir)) (List (Path Rel File))
webInternalPaths = (webInternalDirs, webInternalFiles)

-- | List of all dirs used in frontend build. Nested dirs are only included
-- once e.g. @a/b@ instead of @[a, a/b]@.
webInternalDirs :: List (Path Rel Dir)
webInternalDirs = [[reldirPathSep|src/marshal/chartjs|]]

-- | List of all files used in frontend build.
webInternalFiles :: List (Path Rel File)
webInternalFiles =
  [ [relfile|package-lock.json|],
    [relfile|package.json|],
    [relfile|tsconfig.json|],
    [relfile|webpack.config.js|]
  ]
    ++ srcFiles
    ++ marshalFiles
  where
    srcFiles =
      ([reldir|src|] <</>>)
        <$> [ [relfile|build_charts.ts|],
              [relfile|index.html|],
              [relfile|index.ts|],
              [relfile|style.css|],
              [relfile|theme.ts|],
              [relfile|utils.ts|]
            ]
    marshalFiles =
      ([reldirPathSep|src/marshal|] <</>>)
        <$> [ [relfilePathSep|chartjs/types.ts|],
              [relfile|chartjs.ts|],
              [relfile|common.ts|],
              [relfile|pacer.ts|]
            ]

-- | Web dir name.
webDir :: Path Rel Dir
webDir = [reldir|web|]

-- | Web data input dir relative to 'webDir'.
dataDir :: Path Rel Dir
dataDir = [reldir|data|]

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
