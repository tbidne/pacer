{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- ORMOLU_DISABLE -}

module Pacer.Command.Chart.Node.Internal
  ( -- * Finding Npm
    NpmExe (..),

    -- ** Normal
    findNpm,

#if BUNDLE_NODE

    -- ** Bundled
    NodeBundle (..),
    nodeBundleTH,

#endif

    -- * Running npm
    NpmE (..),
    runNpm,
    runNpm_,

    -- * Node version
    nodeVersionTH,
    npmExeToNodeVersion,
  )
where

{- ORMOLU_ENABLE -}

import Data.List qualified as L
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Process.Typed.Dynamic qualified as TP
import FileSystem.OsPath (decodeLenient, decodeThrowM, encodeThrowM)
import FileSystem.Path qualified as Path
import FileSystem.UTF8 (decodeUtf8Lenient)
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Language.Haskell.TH (Code, Q)
import Pacer.Prelude
import Pacer.Web.Utils qualified as Web.Utils
import System.IO (FilePath)
import System.IO.Error qualified as Error
import System.OsPath qualified as OsP

#if BUNDLE_NODE
import Codec.Archive.Tar qualified as Tar
import Codec.Compression.Zlib qualified as Zlib
import Data.Text qualified as T
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import Language.Haskell.TH.Syntax (Lift)
#endif

#if BUNDLE_NODE

data NodeBundle = MkNodeBundle
  { -- | Compressed node directory.
    contents :: LazyByteString,
    -- | Relative path to npm exe within the node directory i.e.
    -- the unpacked contents.
    npmExePath :: Path Rel File
  }
  deriving stock (Lift)

nodeBundleTH :: Code Q NodeBundle
nodeBundleTH = Web.Utils.liftIOToTH run
  where
    run =
      runEff
        . runPathReader
        . runPathWriter
        . runTypedProcess
        $ nodeBundle

nodeBundle ::
  ( HasCallStack,
    IOE :> es,
    PathReader :> es,
    PathWriter :> es
  ) =>
  Eff es NodeBundle
nodeBundle = do
  -- /.../parent/nodejs-22/bin/npm
  MkNpmExe npmPath <- findNpm
  let npmOsPath = toOsPath npmPath
      exeName = OsP.takeFileName npmOsPath
      -- /.../parent/nodejs-22/bin
      binDir = OsP.takeDirectory npmOsPath
      -- /.../parent/nodejs-22
      nodeDir = OsP.takeDirectory binDir
      -- nodejs-22
      (_, nodeDirName) = OsP.splitFileName (OsP.dropTrailingPathSeparator nodeDir)

      -- /.../parent
      parentDir = OsP.takeDirectory nodeDir

      -- bin/npm
      exeRelOsPath =
        nodeDirName
          </> (OsP.takeBaseName binDir)
          </> exeName

  exeRelPath <- Path.parseRelFile exeRelOsPath

  nodeDirNameStr <- decodeThrowM nodeDirName
  npmStr <- decodeThrowM npmOsPath
  let npmTxt = packText npmStr
      nodeDirNameTxt = packText nodeDirNameStr

  unless ("nodejs" `T.isInfixOf` nodeDirNameTxt) $ do
    let msg =
          mconcat
            [ "node folder derived from npm exe '",
              npmTxt,
              "' does include expected nodejs string: ",
              nodeDirNameTxt
            ]
    throwText msg

  PW.withCurrentDirectory parentDir $ do
    -- The obvious alternative -- Tar.pack parentDir [nodeDir] -- does not
    -- work because the paths are then made relative to the absolute parentDir,
    -- and this causes a write failure when we actually use it at run-time.
    entries <- liftIO $ Tar.pack "./" [nodeDirNameStr]
    let nodeTarBs = Tar.write entries
        nodeTarCompressed = Zlib.compress nodeTarBs

    pure
      $ MkNodeBundle
        { contents = nodeTarCompressed,
          npmExePath = exeRelPath
        }

#endif

newtype NpmExe = MkNpmExe {unNpmExe :: Path Abs File}
  deriving stock (Show)

nodeVersionTH :: Code Q OsString
nodeVersionTH = Web.Utils.liftIOToTH run
  where
    run =
      runEff
        . runPathReader
        . runTypedProcess
        $ findNpm
        >>= npmExeToNodeVersion

npmExeToNodeVersion ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  NpmExe ->
  Eff es OsString
npmExeToNodeVersion (MkNpmExe npmExe) = do
  let binDir = OsP.takeDirectory npmOsPath
      nodeExe = binDir </> [osp|node|]

  nodeExeStr <- decodeThrowM nodeExe
  (ec, stdout, stderr) <- runExe nodeExeStr ["--version"]
  case ec of
    ExitSuccess ->
      decodeUtf8ThrowM (toStrictBS stdout)
        >>= encodeThrowM
        . unpackText
    ExitFailure _ -> do
      let msg =
            mconcat
              [ "Error finding node version for npm '",
                packText nodeExeStr,
                "': ",
                decodeUtf8Lenient (toStrictBS stderr)
              ]
      throwText msg
  where
    npmOsPath = toOsPath npmExe

findNpm :: (HasCallStack, PathReader :> es) => Eff es NpmExe
findNpm =
  -- It is very important we use findExecutable to find the exe, rather
  -- than relying on it being on the PATH via 'npm'. Otherwise windows on
  -- CI dies with mysterious errors about not being able to find certain
  -- files.
  PR.findExecutable npmName >>= \case
    Just npmOsPath -> MkNpmExe <$> Path.parseAbsFile npmOsPath
    Nothing -> do
      throwPathIOError
        npmName
        "createCharts"
        Error.doesNotExistErrorType
        "Required npm executable not found. Please add it to the PATH."

data NpmE = MkNpmE NpmExe (List String) Int LazyByteString
  deriving stock (Show)

instance Exception NpmE where
  displayException (MkNpmE npmExe args i t) =
    mconcat
      [ "Command '",
        L.unwords (npmStr : args),
        "' exited with error code ",
        show i,
        ": ",
        unpackText $ decodeUtf8Lenient $ toStrictBS t
      ]
    where
      npmStr = decodeLenient (toOsPath npmExe.unNpmExe)

runNpm_ ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  NpmExe ->
  List String ->
  Eff es ()
runNpm_ npmExe = void . runNpm npmExe

runNpm ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  NpmExe ->
  List String ->
  Eff es LazyByteString
runNpm npmExe args = do
  npmStr <- decodeThrowM (toOsPath npmExe.unNpmExe)
  (ec, stdout, stderr) <- runExe npmStr args
  case ec of
    ExitSuccess -> pure stdout
    ExitFailure i -> throwM $ MkNpmE npmExe args i (stdout <> "\n" <> stderr)

runExe ::
  ( HasCallStack,
    TypedProcess :> es
  ) =>
  FilePath ->
  List String ->
  Eff es (Tuple3 ExitCode LazyByteString LazyByteString)
runExe exe args = do
  let cmd = TP.proc exe args
  TP.readProcess cmd

npmName :: OsPath

#if WINDOWS
npmName = [osp|npm.cmd|]
#else
npmName = [osp|npm|]
#endif
