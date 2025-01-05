{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import FileSystem.OsPath (decodeLenient)
import Functional.Chart qualified
import Functional.Convert qualified
import Functional.Derive qualified
import Functional.Prelude
import Functional.Scale qualified
import System.Environment.Guard
  ( ExpectEnv (ExpectEnvSet),
    guardOrElse',
  )
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
          Functional.Convert.tests,
          Functional.Derive.tests,
          Functional.Scale.tests
        ]

setup :: IO OsPath
setup = do
  rootTmpDir <- (</> [osp|pacer|]) <$> PR.getTemporaryDirectory
  let tmpDir = rootTmpDir </> tmpName

  -- Make sure we delete any leftover files from a previous run, so tests
  -- have a clean environment.
  PW.removeDirectoryRecursiveIfExists_ tmpDir

  PW.createDirectoryIfMissing True tmpDir
  pure tmpDir
  where
    tmpName = [osp|test|] </> [osp|functional|]

teardown :: OsPath -> IO ()
teardown tmpDir = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = do
      PW.removeDirectoryRecursiveIfExists_ tmpDir

    doNothing =
      putStrLn
        $ "*** Not cleaning up tmp dir: '"
        <> decodeLenient tmpDir
        <> "'"
