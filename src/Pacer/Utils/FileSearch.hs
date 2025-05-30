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
    fileSearch,
    DirExistsCheck (..),
    searchFileAliases,
    searchFilesToList,

    -- ** Errors
    DirNotFoundE (..),
    FileNotFoundE (..),
  )
where

import Control.Monad (filterM)
import Data.Foldable qualified as F
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.OsPath (decodeLenient, encodeLenient)
import FileSystem.Path qualified as Path
import Pacer.Class.FromAlt (FromAlt (listToAlt), isNonEmpty)
import Pacer.Configuration.Env.Types (CachedPaths, getCachedCurrentDirectory, getCachedXdgConfigPath)
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
    State CachedPaths :> es
  ) =>
  FileSearchStrategy f es
findCurrentDirectoryPath =
  MkFileSearchStrategy $ \fileNames -> addNamespace "findCurrentDirectoryPath" $ do
    dir <- getCachedCurrentDirectory
    ((findDirectoryPath (Just $ toOsPath dir)).unFileSearchStrategy) fileNames

-- | If the parameter is not empty, parses to an absolute file(s).
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
    PathReader :> es
  ) =>
  -- | Directory to maybe search.
  Maybe OsPath ->
  FileSearchStrategy f es
findDirectoryPath Nothing = MkFileSearchStrategy $ \_ -> pure empty
findDirectoryPath (Just dir) = MkFileSearchStrategy $ \fileNames -> addNamespace "findDirectoryPath" $ do
  $(Logger.logDebug) $ "Searching directory: " <> Show.showtOsPath dir
  parseCanonicalAbsDir dir >>= fileSearch fileNames DirNotExistsFail

-- | Searches for the given file in the xdg directory.
findXdgPath ::
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    LoggerNS :> es,
    PathReader :> es,
    State CachedPaths :> es
  ) =>
  FileSearchStrategy f es
findXdgPath = MkFileSearchStrategy $ \fileNames -> addNamespace "findXdgPath" $ do
  -- 3. Fallback to xdg
  xdgDir <- getCachedXdgConfigPath
  $(Logger.logDebug) $ "Searching xdg: " <> Show.showtPath xdgDir

  fileSearch fileNames DirNotExistsOk xdgDir

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
fileSearch ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    PathReader :> es
  ) =>
  -- | File names to search.
  FileSearch ->
  -- | How to handle the directory not existing.
  DirNotExistsStrategy ->
  -- | Data dir to search.
  Path Abs Dir ->
  Eff es (f (Path Abs File))
fileSearch fileNames dner dataDir = do
  $(Logger.logDebug) msg

  dExists <- PR.doesDirectoryExist dataDirOsPath

  if dExists
    then go fileNames
    else handleEmptyDir dner dataDir
  where
    dataDirOsPath = toOsPath dataDir

    go :: (HasCallStack) => FileSearch -> Eff es (f (Path Abs File))
    go (SearchFileAliases aliases) =
      searchFileAliases DirExistsCheckOff dataDir aliases
    go (SearchFileInfix p exts) =
      searchFileInfix DirExistsCheckOff dataDir p exts

    msg =
      mconcat
        [ "Searching for path(s) ",
          Show.showMapListInline Show.showtPath fileNamesList,
          " in: ",
          Show.showtPath dataDir
        ]

    fileNamesList = searchFilesToList fileNames

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

-- | Determines whether to check a directory for existence prior to usage.
-- Generally used for the purposes of better error messages / not repeating
-- checks.
data DirExistsCheck
  = -- | Check existence prior to usage.
    DirExistsCheckOn DirNotExistsStrategy
  | -- | Do not check existence prior to usage.
    DirExistsCheckOff

searchFileInfix ::
  forall f es.
  ( FromAlt f,
    Logger :> es,
    HasCallStack,
    PathReader :> es
  ) =>
  DirExistsCheck ->
  Path Abs Dir ->
  Path Rel File ->
  Set OsPath ->
  Eff es (f (Path Abs File))
searchFileInfix checkExists dataDir pat exts = do
  -- Cases:
  --
  -- 1. fileSearch: dir must exist but already checked
  -- 2. config: dir might not exist
  case checkExists of
    DirExistsCheckOn existsHandler -> do
      -- NOTE: [Data dir existence]
      --
      -- Check dir existence first, since otherwise none of this matters.
      dExists <- PR.doesDirectoryExist dataDirOsPath

      if dExists
        then runSearch
        -- Lack of existence OK, hence log and return empty.
        else handleEmptyDir existsHandler dataDir
    DirExistsCheckOff -> runSearch
  where
    dataDirOsPath = toOsPath dataDir
    patOsPath = toOsPath pat
    pLowerTxt = toLowerTxt patOsPath

    runSearch :: Eff es (f (Path Abs File))
    runSearch = do
      allFiles <- PR.listDirectory dataDirOsPath
      matches <- filterM isMatch allFiles
      matches' <- for matches (fmap (dataDir <</>>) . Path.parseRelFile)

      -- A better way to do this might be to iterate through the results,
      -- combining with (<|>) manually (or use asum).
      pure $ listToAlt matches'

    isMatch :: OsPath -> Eff es Bool
    isMatch p = do
      let -- takeExtension for _last_ extension (e.g. .gz in foo.tar.gz)
          -- and takeExtension for full extension (.tar.gz). No other
          -- variants are supported.
          pExt = OsP.takeExtension p
          pExts = OsP.takeExtensions p
          pTxt = toLowerTxt p
          isInfix = pLowerTxt `T.isInfixOf` pTxt
          hasExt = hasExtFn pExt || hasExtFn pExts

      pure $ isInfix && hasExt

    hasExtFn :: OsPath -> Bool
    hasExtFn
      | F.null exts = const True
      | otherwise = (flip Set.member) exts

    toLowerTxt :: OsPath -> Text
    toLowerTxt =
      T.toCaseFold
        . packText
        . decodeLenient

-- | Searches for a single file with potentially multiple aliases. Returns
-- at most one result.
--
-- The search is case-insensitive.
searchFileAliases ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    PathReader :> es
  ) =>
  -- | Determines whether we check the directory for existence.
  -- If we do not check, we still use it i.e. we assume the check has
  -- already taken place. If the check is off, then failures are allowed
  -- i.e. return empty.
  DirExistsCheck ->
  Path Abs Dir ->
  FileAliases ->
  Eff es (f (Path Abs File))
searchFileAliases checkExists dataDir aliases = do
  -- Cases:
  --
  -- 1. fileSearch: dir must exist but already checked
  -- 2. config: dir might not exist
  case checkExists of
    DirExistsCheckOn existsHandler -> do
      -- NOTE: [Data dir existence]
      --
      -- Check dir existence first, since otherwise none of this matters.
      dExists <- PR.doesDirectoryExist dataDirOsPath

      if dExists
        then runSearch
        -- Lack of existence OK, hence log and return empty.
        else handleEmptyDir existsHandler dataDir
    DirExistsCheckOff -> runSearch
  where
    runSearch :: Eff es (f (Path Abs File))
    runSearch = go . NE.toList $ aliases.unAliases
    dataDirOsPath = toOsPath dataDir

    go :: (HasCallStack) => List (Path Rel File) -> Eff es (f (Path Abs File))
    go [] = pure empty
    go (f : fs) = do
      let path = dataDir <</>> f
      exists <- PR.doesFileExist (toOsPath path)
      if exists
        -- 1. File exists, return single result
        then (pure path <|>) <$> go fs
        else do
          -- 2. File does not exist, try case insensitive search.
          caseInsensResult <- searchCaseInsens f
          (caseInsensResult <|>) <$> go fs

    searchCaseInsens :: (HasCallStack) => Path Rel File -> Eff es (f (Path Abs File))
    searchCaseInsens f = do
      let p = toOsPath f
          pLower = toLower p

      -- NOTE: This will _fail_ if the data directory does not exist. Hence
      -- we should check existence first. See NOTE: [Data dir existence].
      allFiles <- PR.listDirectory dataDirOsPath

      case F.find ((==) pLower . toLower) allFiles of
        Just osPath -> do
          relFile <- Path.parseRelFile osPath
          let absFile = dataDir <</>> relFile
              msg =
                mconcat
                  [ "Did not find exact match for '",
                    packText $ Show.showPath f,
                    "' but found '",
                    packText $ Show.showPath absFile,
                    "', using it."
                  ]
          -- NOTE: As we only perform a case-insens search during file
          -- discovery, this really shouldn't be a warning, since it's
          -- "best-effort" anyway. For instance, if someone names their garmin
          -- file 'activites.csv' -- cf. 'Activites'csv' -- then we should
          -- find it without any fanfare.
          --
          -- The info level already logs the exact filepath being used,
          -- so this slightly more granular message (searched vs. found)
          -- makes more sense for debug.
          $(Logger.logDebug) msg
          pure $ pure absFile
        Nothing -> pure empty

    toLower :: OsPath -> OsPath
    toLower =
      encodeLenient
        . unpackText
        . T.toCaseFold
        . packText
        . decodeLenient

-- | Represents searching for a particular file.
data FileSearch
  = -- | Search for a file by aliases.
    SearchFileAliases FileAliases
  | -- | Search for a file by infix pattern, optionally filtering
    -- by known extensions. For example, searching for "activities"
    -- and {"json", "csv"} will find all files with (case-insensitive)
    -- infix "activities" and extensions "json" or "csv".
    SearchFileInfix (Path Rel File) (Set OsPath)

-- | Represents a single file for which we want to search. Holds multiple
-- aliases for the case where the file can have multiple names.
-- For instance, both chart-requests.json and chart-requests.jsonc are valid
-- expected names for the chart-requests file, so we search for both, even
-- though we want at most one.
newtype FileAliases = MkFileAliases
  { unAliases :: NonEmpty (Path Rel File)
  }

searchFilesToList :: FileSearch -> List (Path Rel File)
searchFilesToList = f
  where
    f :: FileSearch -> List (Path Rel File)
    f (SearchFileAliases as) = NE.toList as.unAliases
    f (SearchFileInfix p _) = [p]
