{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pacer.Utils.FileSearch
  ( -- * High-level
    FileSearchStrategy (..),
    resolveFilePath,

    -- ** Strategies
    findFilePath,
    findCurrentDirectoryPath,
    findDirectoryPath,
    findXdgPath,

    -- * Low-level

    -- ** File discovery
    DirNotExistsStrategy (..),
    FileSearch (..),
    FileAliases (..),
    directorySearch,
    searchFilesToList,

    -- ** Errors
    DirNotFoundE (..),
    FileNotFoundE (..),

    -- ** Misc
    MatchResult (..),
    satisfiesPattern,
    osPathToNameExts,
  )
where

import Control.Monad (filterM)
import Data.Foldable qualified as F
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic (LogLevel (LevelDebug))
import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.OsPath (decodeLenient)
import FileSystem.Path qualified as Path
import Pacer.Class.FromAlt (FromAlt (listToAlt), isNonEmpty)
import Pacer.Configuration.Env.Types
  ( CachedPaths,
    LogEnv (logLevel, logVerbosity),
    getCachedCurrentDirectory,
    getCachedXdgConfigPath,
  )
import Pacer.Configuration.Logging (LogVerbosity (LogV1))
import Pacer.Prelude
import Pacer.Utils.Show qualified as Show
import System.OsPath qualified as OsP

-- | Attempts to resolve a file, based on the expected file names and
-- search strategies. The strategies are tried in order, returning the
-- first non-empty, per 'FromAlt'.
resolveFilePath ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    LoggerNS :> es
  ) =>
  -- | Text description of what we are trying to find. Used for logging.
  Text ->
  -- | Expected file_name(s) e.g. activities file(s).
  FileSearch ->
  -- | Search strategies.
  List (FileSearchStrategy f es) ->
  Eff es (f (Path Abs File))
resolveFilePath desc fileNames strategies =
  addNamespace "resolveFilePath" $ addNamespace desc $ do
    (fold strategies).unFileSearchStrategy fileNames

-- | General exception for when a file at an expected path does not exist.
-- We would normally use IOException for this, except we want a custom type
-- so that we can ignore callstacks. We do not want to ignore IOException
-- since other errors may throw them, and those we want to know about.
newtype FileNotFoundE = MkFileNotFoundE OsPath
  deriving stock (Show)

instance Exception FileNotFoundE where
  displayException (MkFileNotFoundE p) =
    mconcat
      [ "File not found: ",
        decodeLenient p
      ]

-- | FileSearchStrategy represents a single strategy for finding a single
-- "type" of file(s), e.g. searching the current directory for the
-- chart-requests file, or searching xdg for multiple activities files.
--
-- The semigroup instance takes the first "non-empty", per the FromAlt
-- instance. In practice empty is generally Nothing (Maybe, for a single file),
-- or [] (List/Seq, for multiple results).
--
-- In other words, a strategy amount to searching for some file(s) in a
-- singular location. After we have a "success" (which be one or more files),
-- we stop.
type FileSearchStrategy :: (Type -> Type) -> List Effect -> Type
newtype FileSearchStrategy f es
  = MkFileSearchStrategy
  { unFileSearchStrategy :: FileSearch -> (Eff es (f (Path Abs File)))
  }

instance (FromAlt f) => Semigroup (FileSearchStrategy f es) where
  MkFileSearchStrategy f <> g = MkFileSearchStrategy $ \files -> do
    l <- f files

    if isNonEmpty l
      then pure l
      else g.unFileSearchStrategy files

instance (FromAlt f) => Monoid (FileSearchStrategy f es) where
  mempty = MkFileSearchStrategy $ \_ -> pure empty

-- | 'findDirectoryPath' specialized to the current directory.
findCurrentDirectoryPath ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    Reader LogEnv :> es,
    State CachedPaths :> es
  ) =>
  FileSearchStrategy f es
findCurrentDirectoryPath =
  MkFileSearchStrategy $ \fileNames -> addNamespace "findCurrentDirectoryPath" $ do
    dir <- getCachedCurrentDirectory
    ((findDirectoryPath (Just $ toOsPath dir)).unFileSearchStrategy) fileNames

-- | If the parameter is not empty, parses to an absolute file(s). Ignores
-- the parameter to searchFiles.
findFilePath ::
  forall f es.
  ( HasCallStack,
    PathReader :> es,
    Traversable f
  ) =>
  -- | Maybe file.
  f OsPath ->
  FileSearchStrategy f es
findFilePath mFiles = MkFileSearchStrategy $ \_ -> for mFiles $ \f -> do
  absPath <- parseCanonicalAbsFile f
  let absOsPath = toOsPath absPath
  exists <- PR.doesFileExist absOsPath
  if exists
    then pure absPath
    -- NOTE: We check existence here solely to be consistent. That is, the
    -- other (directory search) methods all check existence since they have
    -- to: We don't want to return, say, <dir>/file and thus skip the
    -- <xdg>/file check unless we have verified <dir>/file actually exists.
    -- Hence we check existence here, even if we technically do not have to.
    else throwM $ MkFileNotFoundE absOsPath

-- | Searches for the files in the given directory. Otherwise returns empty.
findDirectoryPath ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    Reader LogEnv :> es
  ) =>
  -- | Directory to maybe search.
  Maybe OsPath ->
  FileSearchStrategy f es
findDirectoryPath Nothing = MkFileSearchStrategy $ \_ -> pure empty
findDirectoryPath (Just dir) = MkFileSearchStrategy $ \fileNames -> addNamespace "findDirectoryPath" $ do
  $(Logger.logDebug) $ "Searching directory: " <> Show.showtOsPath dir
  parseCanonicalAbsDir dir >>= directorySearch fileNames DirNotExistsFail

-- | Searches for the given file in the xdg directory.
findXdgPath ::
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    Reader LogEnv :> es,
    State CachedPaths :> es
  ) =>
  FileSearchStrategy f es
findXdgPath = MkFileSearchStrategy $ \fileNames -> addNamespace "findXdgPath" $ do
  -- 3. Fallback to xdg
  xdgDir <- getCachedXdgConfigPath
  $(Logger.logDebug) $ "Searching xdg: " <> Show.showtPath xdgDir

  directorySearch fileNames DirNotExistsOk xdgDir

-- | Exception for a directory not existing.
newtype DirNotFoundE = MkDirNotFoundE OsPath
  deriving stock (Show)

instance Exception DirNotFoundE where
  displayException (MkDirNotFoundE p) =
    mconcat
      [ "Directory not found: ",
        decodeLenient p
      ]

-- | How to handle a directory not existing.
data DirNotExistsStrategy
  = -- | Directory not required; inexistence OK.
    DirNotExistsOk
  | -- | Directory required; inexistence should error.
    DirNotExistsFail

-- | Attempts to discover files based on 'FileSearch' i.e.
-- "expected" filenames. For each search file @s@, produces a search result
-- @r@ per 'searchFile', and combines the result via '(<|>)'.
--
-- In other words, depending on choice of type variable @f@, can return
-- multiple results ('List') or a single result ('Maybe').
directorySearch ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    Reader LogEnv :> es
  ) =>
  -- | File names to search.
  FileSearch ->
  -- | How to handle the directory not existing.
  DirNotExistsStrategy ->
  -- | Data dir to search.
  Path Abs Dir ->
  Eff es (f (Path Abs File))
directorySearch fileNames dner dataDir = do
  $(Logger.logDebug) msg

  dExists <- PR.doesDirectoryExist dataDirOsPath

  if dExists
    then do
      $(Logger.logDebug) (mkDirExistsMsg dataDir)
      go fileNames
    else handleEmptyDir dner dataDir
  where
    dataDirOsPath = toOsPath dataDir

    go :: (HasCallStack) => FileSearch -> Eff es (f (Path Abs File))
    go (SearchFileAliases aliases) = searchFileAliases dataDir aliases
    go (SearchFileInfix p exts) = searchFileInfix dataDir p exts

    msg =
      mconcat
        [ "Searching for path(s) ",
          Show.showMapListInline Show.showtPath fileNamesList,
          extsStr,
          " in: ",
          Show.showtPath dataDir
        ]

    extsStr
      | F.null fileExts = ""
      | otherwise =
          mconcat
            [ " with extension(s) ",
              Show.showMapListInline Show.showtOsPath fileExts
            ]

    (fileNamesList, fileExts) = searchFilesToList fileNames

searchFileInfix ::
  forall f es.
  ( FromAlt f,
    Logger :> es,
    LoggerNS :> es,
    HasCallStack,
    PathReader :> es,
    Reader LogEnv :> es
  ) =>
  Path Abs Dir ->
  Path Rel File ->
  Set OsPath ->
  Eff es (f (Path Abs File))
searchFileInfix dataDir pat exts = addNamespace "searchFileInfix" $ do
  runSearch T.isInfixOf dataDir pat exts
  where

-- | Searches for a single file with potentially multiple aliases. Returns
-- at most one result.
--
-- The search is case-insensitive.
searchFileAliases ::
  forall f es.
  ( FromAlt f,
    Logger :> es,
    LoggerNS :> es,
    HasCallStack,
    PathReader :> es,
    Reader LogEnv :> es
  ) =>
  Path Abs Dir ->
  FileAliases ->
  Eff es (f (Path Abs File))
searchFileAliases dataDir (MkFileAliases aliases exts) = addNamespace "searchFileAliases" $ do
  go $ toList aliases
  where
    go :: (HasCallStack) => List (Path Rel File) -> Eff es (f (Path Abs File))
    go [] = pure empty
    go (a : as) = (<|>) <$> runSearch' a <*> go as

    runSearch' a = runSearch (==) dataDir a exts

runSearch ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    Reader LogEnv :> es
  ) =>
  -- | File name comparison e.g. equality or infix. The LHS is the
  -- alias/pattern we are given, and the RHS is the potential candidates
  -- (directory contents).
  (Text -> Text -> Bool) ->
  -- | Directory to search.
  Path Abs Dir ->
  -- | File name.
  Path Rel File ->
  -- | Extensions to consider.
  Set OsPath ->
  Eff es (f (Path Abs File))
runSearch compFn dataDir pat exts = addNamespace "runSearch" $ do
  allFiles <- PR.listDirectory dataDirOsPath

  let filesTxt = Show.showMapListInline Show.showtOsPath allFiles
  $(Logger.logDebug) $ "Found files: " <> filesTxt

  matches <- filterM isMatch allFiles
  let matchesTxt = Show.showMapListInline Show.showtOsPath matches

  $(Logger.logDebug) $ "Matches: " <> matchesTxt

  matches' <- for matches (fmap (dataDir <</>>) . Path.parseRelFile)
  let matchesTxt' = Show.showMapListInline Show.showtPath matches'
  $(Logger.logDebug) $ "Full matches: " <> matchesTxt'

  -- A better way to do this might be to iterate through the results,
  -- combining with (<|>) manually (or use asum).
  pure $ listToAlt matches'
  where
    dataDirOsPath = toOsPath dataDir

    satisfiesPattern' = satisfiesPattern compFn exts pat

    isMatch :: OsPath -> Eff es Bool
    isMatch p = do
      let matchResult = satisfiesPattern' p

      logEnv <- ask @LogEnv
      case (logEnv.logLevel, logEnv.logVerbosity) of
        (Just LevelDebug, LogV1) -> do
          $(Logger.logDebug) matchResult.patternLog
          $(Logger.logDebug) matchResult.extensionLog
        _ -> pure ()

      pure
        $ matchResult.patternResult
        && fromMaybe True matchResult.extensionResult

-- | File match comparison.
data MatchResult = MkMatchResult
  { -- | The result for pattern comparisons i.e. file names.
    patternResult :: Bool,
    -- | The result for file extension comparisons.
    extensionResult :: Maybe Bool,
    -- | Log message for pattern result.
    patternLog :: Text,
    -- | Log message for extension result.
    extensionLog :: Text
  }
  deriving stock (Eq, Show)

-- | Returns the result of comparing a discovered file against a given
-- pattern and possible extension set. For extension comparisons, we
-- consider the "final" extension and the "full" extension e.g. ".gz" and
-- ".tar.gz" in "foo.tar.gz", respectively. Parameters extensions should
-- include the initial dot.
satisfiesPattern ::
  -- | Pattern comparison function. The LHS is for the pattern whereas the RHS
  -- is the discovered file i.e. Pattern -> File -> Bool. The comparison
  -- should be for file _names_ only i.e. ignore directories and
  -- extensions.
  (Text -> Text -> Bool) ->
  -- | Extension set. If empty, we ignore the discovered file's extension.
  Set OsString ->
  -- | Pattern i.e. alias or infix.
  Path Rel File ->
  -- | Discovered file.
  OsPath ->
  MatchResult
satisfiesPattern compFn exts pat p
  | F.null exts =
      let patResult = patTxt `compFn` pNameTxt
       in MkMatchResult
            { patternResult = patResult,
              extensionResult = Nothing,
              patternLog = mkPatternLog (showt patResult),
              extensionLog = mkExtensionLog "<not compared>"
            }
  | otherwise =
      let patResult = patTxt `compFn` pNameTxt
          extResult = hasExtFn pExt || hasExtFn pExts
       in MkMatchResult
            { patternResult = patResult,
              extensionResult = Just extResult,
              patternLog = mkPatternLog (showt patResult),
              extensionLog = mkExtensionLog (showt extResult)
            }
  where
    -- Split the discovered file to its name and extensions, throw away its
    -- directories.
    (pName, pExt, pExts) = osPathToNameExts p

    patTxt = toLowerTxt $ toOsPath pat
    pNameTxt = toLowerTxt pName

    hasExtFn :: OsPath -> Bool
    hasExtFn = flip Set.member exts

    mkPatternLog b =
      mconcat
        [ "File ",
          Show.showtOsPath p,
          " compared with pattern ",
          Show.showtPath pat,
          " is: ",
          b
        ]

    mkExtensionLog b =
      mconcat
        [ "File ",
          Show.showtOsPath p,
          " compared against extensions ",
          Show.showtOsPath pExt,
          " and ",
          Show.showtOsPath pExts,
          " is: ",
          b
        ]

-- | Transforms a path to (name, final_extension, full_extension).
--
-- @
--   pathToNameExts /path/to/foo.tar.gz === (foo, .gz, .tar.gz)
-- @
osPathToNameExts :: OsPath -> (OsPath, OsString, OsString)
osPathToNameExts p = (name, ext, exts)
  where
    name = toBaseFileName p
    ext = OsP.takeExtension p
    exts = OsP.takeExtensions p

toBaseFileName :: OsPath -> OsPath
toBaseFileName = OsP.dropExtensions . OsP.takeFileName

toLowerTxt :: OsPath -> Text
toLowerTxt =
  T.toCaseFold
    . packText
    . decodeLenient

-- | Handles empty directory according to the parameter i.e. either throws
-- an exception or logs and returns empty.
handleEmptyDir ::
  ( Alternative f,
    Logger :> es
  ) =>
  DirNotExistsStrategy ->
  Path b Dir ->
  Eff es (f a)
handleEmptyDir DirNotExistsFail dataDir = throwM $ MkDirNotFoundE (toOsPath dataDir)
handleEmptyDir DirNotExistsOk dataDir = do
  $(Logger.logDebug) $ mkDirNotExistMsg dataDir
  pure empty

mkDirNotExistMsg :: Path b Dir -> Text
mkDirNotExistMsg d =
  mconcat
    [ "Directory '",
      packText $ Show.showPath d,
      "' does not exist."
    ]

mkDirExistsMsg :: Path b Dir -> Text
mkDirExistsMsg d =
  mconcat
    [ "Directory '",
      packText $ Show.showPath d,
      "' exists."
    ]

-- | Represents searching for a particular file.
data FileSearch
  = -- | Search for a file by aliases.
    SearchFileAliases FileAliases
  | -- | Search for a file by infix pattern, optionally filtering
    -- by known extensions. For example, searching for "activities"
    -- and {"json", "csv"} will find all files with (case-insensitive)
    -- infix "activities" and extensions "json" or "csv".
    SearchFileInfix (Path Rel File) (Set OsPath)
  deriving stock (Eq, Show)

-- | Represents a single file for which we want to search. Holds multiple
-- aliases for the case where the file can have multiple names.
-- For instance, both chart-requests.json and chart-requests.jsonc are valid
-- expected names for the chart-requests file, so we search for both, even
-- though we want at most one.
data FileAliases = MkFileAliases
  { aliases :: NonEmpty (Path Rel File),
    extensions :: Set OsPath
  }
  deriving stock (Eq, Show)

searchFilesToList :: FileSearch -> Tuple2 (List (Path Rel File)) (Set OsPath)
searchFilesToList = f
  where
    f :: FileSearch -> Tuple2 (List (Path Rel File)) (Set OsPath)
    f (SearchFileAliases as) = (NE.toList as.aliases, as.extensions)
    f (SearchFileInfix p exts) = ([p], exts)
