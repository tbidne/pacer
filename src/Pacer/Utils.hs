{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pacer.Utils
  ( -- * JSON

    -- ** Encoding
    encodeMaybe,
    encodeMaybes,

    -- ** Decoding
    (.:?:),
    failUnknownFields,
    decodeJson,
    AesonE (..),
    readDecodeJson,

    -- * Show
    showtOsPath,
    showPath,
    showtPath,
    showListInline,
    showMapListInline,
    showListNewlines,
    showMapListNewlines,

    -- * File discovery
    SearchFiles (..),
    FileAliases (..),
    searchFiles,
    searchFileAliases,
    searchFilesToList,

    -- * Seq
    seqGroupBy,

    -- * Misc
    PaceMetersErrMsg,
  )
where

import Data.Aeson (Key, (<?>))
import Data.Aeson qualified as Asn
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (JSONPathElement (Key), Pair)
import Data.Aeson.Types qualified as AsnT
import Data.Foldable qualified as F
import Data.HashMap.Strict qualified as HMap
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Logger.Dynamic qualified as Logger
import FileSystem.OsPath (decodeLenient, encodeLenient)
import FileSystem.OsPath qualified as OsPath
import FileSystem.Path qualified as Path
import GHC.IsList (IsList (Item))
import GHC.IsList qualified as IL
import Pacer.Class.FromAlt (FromAlt, isNonEmpty)
import Pacer.Class.Parser qualified as P
import Pacer.Prelude

-- | Encodes 'Maybe's to a possibly empty list.
encodeMaybes :: (ToJSON v) => List (Tuple2 Key (Maybe v)) -> List Pair
encodeMaybes = (>>= encodeMaybe)

-- | Encodes a 'Maybe' to a possibly empty list.
encodeMaybe :: (ToJSON v) => Tuple2 Key (Maybe v) -> List Pair
encodeMaybe (_, Nothing) = []
encodeMaybe (k, Just v) = [k .= v]

type PaceMetersErrMsg = "Meters are disallowed in Pace; use km or mi."

showtOsPath :: OsPath -> Text
showtOsPath = packText . OsPath.decodeLenient

showPath :: Path b t -> String
showPath = OsPath.decodeLenient . pathToOsPath

showtPath :: Path b t -> Text
showtPath = showtOsPath . pathToOsPath

showListInline :: (IsString a, Semigroup a) => List a -> a
showListInline = showMapListInline identity

showMapListInline :: (IsString b, Semigroup b) => (a -> b) -> List a -> b
showMapListInline _ [] = "[]"
showMapListInline f xs@(_ : _) = "[" <> go xs
  where
    go [] = "]"
    go [y] = f y <> "]"
    go (y : ys) = f y <> ", " <> go ys

showListNewlines :: (IsString a, Semigroup a) => List a -> a
showListNewlines = showMapListNewlines identity

showMapListNewlines :: (IsString b, Semigroup b) => (a -> b) -> List a -> b
showMapListNewlines f = go
  where
    go [] = ""
    go (y : ys) = "\n  - " <> f y <> go ys

seqGroupBy :: forall a. (a -> a -> Bool) -> Seq a -> Seq (NESeq a)
seqGroupBy p = go
  where
    go :: Seq a -> Seq (NESeq a)
    go Seq.Empty = Seq.Empty
    go (x :<| xs) = (x :<|| ys) :<| go zs
      where
        (ys, zs) = Seq.spanl (p x) xs

-- | We use this rather than aeson's AesonException for two reasons:
--
-- 1. AesonException does not have an NFData instance, which we require for
--    the benchmarks.
--
-- 2. Optional path improves the error message.
data AesonE = MkAesonE (Maybe OsPath) String
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

instance Exception AesonE where
  displayException (MkAesonE mPath s) =
    case mPath of
      Just p ->
        mconcat
          [ "Error decoding json path '",
            decodeLenient p,
            "': ",
            s
          ]
      Nothing -> "Error decode json: " ++ s

-- | Decodes a json(c) file.
readDecodeJson ::
  forall a es.
  ( FileReader :> es,
    FromJSON a,
    HasCallStack
  ) =>
  Path Abs File ->
  Eff es a
readDecodeJson path = do
  contents <- readBinaryFile osPath
  throwErr
    $ first toAesonPathE
    $ decodeJson @a contents
  where
    osPath = pathToOsPath path
    toAesonPathE (MkAesonE _ s) = MkAesonE (Just osPath) s

-- | Fails if there are any unknown fields in the object.
failUnknownFields ::
  -- | Key label to improve error message.
  Key ->
  -- | Known keys.
  List Key ->
  -- | Actual keys.
  KeyMap Asn.Value ->
  AsnT.Parser ()
failUnknownFields name knownKeys kmap = do
  case HSet.toList unknownKeys of
    [] -> pure ()
    unknownKeyList@(_ : _) ->
      ( fail
          $ mconcat
            [ "Encountered unknown keys: ",
              showKeys unknownKeyList
            ]
      )
        <?> Key name
  where
    actualKeyMap = KM.toHashMap kmap
    actualKeySet = HMap.keysSet actualKeyMap

    knownKeySet = HSet.fromList knownKeys
    unknownKeys = HSet.difference actualKeySet knownKeySet

    showKeys :: List Key -> String
    showKeys = show . L.sort . fmap Key.toString

-- | Decodes json(c).
decodeJson :: (FromJSON a) => ByteString -> Result AesonE a
decodeJson =
  first (MkAesonE Nothing)
    <<< (review #eitherIso)
    . Asn.eitherDecodeStrict
    <=< P.stripComments

-- | Decodes a json list into an 'IsList'. Like '(.:?)', omitted keys are fine
-- and decode to an empty list.
(.:?:) ::
  forall l.
  (FromJSON (Item l), IsList l) =>
  Asn.Object ->
  Key ->
  AsnT.Parser l
(.:?:) o k = do
  o .:? k <&> \case
    Nothing -> IL.fromList []
    Just xs -> IL.fromList xs

infixl 9 .:?:

-- | Attempts to discover files based on 'SearchFiles' i.e.
-- "expected" filenames. For each search file @s@, produces a search result
-- @r@ per 'searchFile', and combines the result via '(<|>)'.
--
-- In other words, depending on choice of type variable @f@, can return
-- multiple results ('List') or a single result ('Maybe').
searchFiles ::
  forall f es.
  ( FromAlt f,
    HasCallStack,
    Logger :> es,
    PathReader :> es
  ) =>
  -- | File names to search.
  SearchFiles ->
  -- | Data dir to search.
  Path Abs Dir ->
  Eff es (f (Path Abs File))
searchFiles fileNames dataDir = do
  $(Logger.logDebug) msg

  dExists <- PR.doesDirectoryExist dataDirOsPath

  if dExists
    then go . NE.toList $ fileNames.unSearchFiles
    else do
      $(Logger.logDebug) $ mkDirNotExistMsg dataDir
      pure empty
  where
    dataDirOsPath = pathToOsPath dataDir

    go :: (HasCallStack) => List FileAliases -> Eff es (f (Path Abs File))
    go [] = pure empty
    go (f : fs) = do
      result <- searchFileAliases False dataDir f
      (result <|>) <$> go fs

    msg =
      mconcat
        [ "Searching for path(s) ",
          showMapListInline showtPath fileNamesList,
          " in: ",
          showtPath dataDir
        ]

    fileNamesList = searchFilesToList fileNames

mkDirNotExistMsg :: Path b Dir -> Text
mkDirNotExistMsg d =
  mconcat
    [ "Directory '",
      packText $ showPath d,
      "' does not exist."
    ]

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
  -- | If True, checks dataDir for existence. If False, skips the check.
  Bool ->
  Path Abs Dir ->
  FileAliases ->
  Eff es (f (Path Abs File))
searchFileAliases checkExists dataDir aliases = do
  if checkExists
    then do
      -- NOTE: [Data dir existence]
      --
      -- Check dir existence first, since otherwise none of this matters.
      dExists <- PR.doesDirectoryExist dataDirOsPath

      if dExists
        then runSearch
        else do
          $(Logger.logDebug) $ mkDirNotExistMsg dataDir
          pure empty
    else runSearch
  where
    runSearch = go . NE.toList $ aliases.unAliases
    dataDirOsPath = pathToOsPath dataDir

    go :: (HasCallStack) => List (Path Rel File) -> Eff es (f (Path Abs File))
    go [] = pure empty
    go (f : fs) = do
      let path = dataDir <</>> f
      exists <- PR.doesFileExist (pathToOsPath path)
      if exists
        -- 1. File exists, return single result
        then pure $ pure path
        else do
          -- 2. File does not exist, try case insensitive search.
          caseInsensResult <- searchCaseInsens f
          if isNonEmpty caseInsensResult
            -- 2.1. Some case-insensitive match, return.
            then pure caseInsensResult
            -- 2.2. No matches, try remaining aliases.
            else go fs

    searchCaseInsens :: (HasCallStack) => Path Rel File -> Eff es (f (Path Abs File))
    searchCaseInsens f = do
      let p = pathToOsPath f
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
                    packText $ showPath f,
                    "' but found '",
                    packText $ showPath absFile,
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

-- | Represents multiple files for which we search. Unlike 'FileAliases',
-- 'SearchFiles' represents _different_ files (not mere alias differences),
-- hence a 'SearchFiles' search can return multiple results.
newtype SearchFiles = MkSearchFiles
  { unSearchFiles :: NonEmpty FileAliases
  }

-- | Represents a single file for which we want to search. Holds multiple
-- aliases for the case where the file can have multiple names.
-- For instance, both chart-requests.json and chart-requests.jsonc are valid
-- expected names for the chart-requests file, so we search for both, even
-- though we want at most one.
newtype FileAliases = MkFileAliases
  { unAliases :: NonEmpty (Path Rel File)
  }

searchFilesToList :: SearchFiles -> List (Path Rel File)
searchFilesToList =
  (>>= NE.toList)
    . NE.toList
    . fmap (.unAliases)
    . (.unSearchFiles)
