{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import FileSystem.OsPath (decodeLenient)
import Functional.Chart qualified
import Functional.Derive qualified
import Functional.Prelude
import Functional.Scale qualified
import System.Directory.OsPath qualified as Dir
import System.Environment.Guard
import Test.Tasty (defaultMain, localOption, withResource)
import Test.Tasty.Golden (DeleteOutputFile (OnPass))

main :: IO ()
main =
  defaultMain
    $ localOption OnPass
    $ withResource setup teardown
    $ \getTestDir ->
      testGroup
        "Functional"
        [ Functional.Chart.tests getTestDir,
          Functional.Derive.tests,
          Functional.Scale.tests
        ]

setup :: IO OsPath
setup = do
  rootTmpDir <- (</> [osp|pacer|]) <$> Dir.getTemporaryDirectory
  let tmpDir = rootTmpDir </> tmpName

  -- Make sure we delete any leftover files from a previous run, so tests
  -- have a clean environment.
  dirExists <- Dir.doesDirectoryExist tmpDir
  when dirExists (Dir.removeDirectoryRecursive tmpDir)

  Dir.createDirectoryIfMissing True tmpDir
  pure tmpDir
  where
    tmpName = [osp|test|] </> [osp|functional|]

teardown :: OsPath -> IO ()
teardown tmpDir = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = do
      dirExists <- Dir.doesDirectoryExist tmpDir
      when dirExists (Dir.removeDirectoryRecursive tmpDir)

    doNothing =
      putStrLn
        $ "*** Not cleaning up tmp dir: '"
        <> decodeLenient tmpDir
        <> "'"
