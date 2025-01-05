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

import Language.Haskell.TH (Code, Q)
import Pacer.Prelude
import Pacer.Web.Utils qualified as WUtils

-- | Reads the web directory at compile-time.
readWebDirTH :: Code Q (List (Tuple2 (Path Rel File) ByteString))
readWebDirTH = WUtils.liftIOToTH $ for webInternalFiles $ \fileName -> do
  let webDir' = pathToOsPath webDir
      fileName' = pathToOsPath fileName
  c <- readBinaryFile (webDir' </> fileName')
  pure (fileName, c)

-- | List of all paths used in frontend build.
webInternalPaths :: Tuple2 (List (Path Rel Dir)) (List (Path Rel File))
webInternalPaths = (webInternalDirs, webInternalFiles)

-- | List of all dirs used in frontend build.
webInternalDirs :: List (Path Rel Dir)
webInternalDirs = [[reldir|src|]]

-- | List of all files used in frontend build.
webInternalFiles :: List (Path Rel File)
webInternalFiles =
  [ [relfile|package-lock.json|],
    [relfile|package.json|],
    [relfile|tsconfig.json|],
    [relfile|webpack.config.js|]
  ]
    ++ srcFiles
  where
    srcFiles =
      ([reldir|src|] <</>>)
        <$> [ [relfile|build_charts.ts|],
              [relfile|index.html|],
              [relfile|index.ts|],
              [relfile|style.css|],
              [relfile|types.ts|],
              [relfile|utils.ts|]
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
    MonadPathReader m,
    MonadThrow m
  ) =>
  m (Path Abs Dir)
getWebPath = (<</>> webDir) <$> getXdgCachePath
